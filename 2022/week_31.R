library(tidyverse)
library(lubridate)
library(svgparser)
library(grid)
library(gggrid)

frogs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frogs.csv')

colors <- MetBrewer::MetPalettes$Hokusai1[[1]]

scales::show_col(colors)

frog_ids <-
  frogs |> 
  count(Frequency, sort = TRUE) |> 
  slice_head(n = 10) |> 
  mutate(Frequency = scales::number(Frequency, accuracy = 1/1000)) |> 
  pull(Frequency)

plot_df <-
  frogs |>
  mutate(date = mdy(SurveyDate),
         Frequency = scales::number(Frequency, accuracy = 1/1000),
         Frequency = as.factor(Frequency)) |> 
  arrange(date) |> 
  filter(Frequency %in% frog_ids) |>
  group_by(Frequency) |> 
  filter(date == min(date) | Structure != lag(Structure)) |> 
  mutate(num_changes = n()) |>
  ungroup() |> 
  mutate(Frequency = fct_reorder(Frequency, num_changes),
         img_path = paste0("2022/week_31_icons/", Structure, ".svg"),
         img_color = case_when(
           Structure == "Herbaceous veg" ~ "#98768e",
           Structure == "Woody debris" ~ colors[4],
           Structure == "Open" ~ "#f1af3a",
           Structure == "Leaf litter" ~ colors[7],
           Structure == "Woody veg" ~ colors[6],
         )) 

# function taken from this tutorial: https://coolbutuseless.github.io/package/svgparser/articles/gggrid.html
position_image <- function(data, coords) {
  grobs <- lapply(seq(nrow(coords)), function(id) {
    gnew <- read_svg(coords[id, ][["img_path"]], style_default = list(fill = coords[id, ][["img_colour"]]))
    
    if (coords[id, ][["img_path"]] == "2022/week_31_icons/Herbaceous veg.svg")
      gnew$vp <- viewport(width = unit(20, "mm"), height = unit(10, "mm"))
    else
      gnew$vp <- viewport(width = unit(20, "mm"), height = unit(20, "mm"))
    
    gnew$vp$x <- unit(coords[id, ][["x"]], "npc")
    gnew$vp$y <- unit(coords[id, ][["y"]], "npc")
    gnew$name <- strftime(Sys.time(), "%H%M%OS6") # enforce unique name per grob
    gnew
  })
  do.call(grobTree, grobs) 
}

# create a legend for the chart 
img_path <- "2022/week_31_icons/"

img_herbaceous <- read_svg(paste0(img_path, "Herbaceous veg.svg"), style_default = list(fill = "#98768e"))
img_herbaceous$vp <- viewport(x = unit(0.25, "npc"), y = unit(0.8, "npc"), width = unit(20, "mm"), height = unit(10, "mm"))
text_herbaceous <- textGrob("Herbaceous\nveg", x = unit(0.25, "npc") - unit(5, "mm"), y = unit(0.8, "npc") - unit(10, "mm"), hjust = 0.5,
                            gp = gpar(family = "News Cycle", fontsize = unit(11, "pt"), col = "#98768e", fontface = "bold"))

img_open <- read_svg(paste0(img_path, "Open.svg"), style_default = list(fill = "#f1af3a"))
img_open$vp <- viewport(x = unit(0.75, "npc"), y = unit(0.8, "npc"), width = unit(20, "mm"), height = unit(20, "mm"))
text_open <- textGrob("Open", x = unit(0.75, "npc") + unit(1, "mm"), y = unit(0.8, "npc") - unit(10, "mm"), hjust = 0.5,
                            gp = gpar(family = "News Cycle", fontsize = unit(11, "pt"), col = "#f1af3a", fontface = "bold"))


img_leaf_litter <- read_svg(paste0(img_path, "Leaf litter.svg"), style_default = list(fill = colors[7]))
img_leaf_litter$vp <- viewport(x = unit(0.25, "npc"), y = unit(0.35, "npc"), width = unit(20, "mm"), height = unit(20, "mm"))
text_leaf_litter <- textGrob("Leaf litter", x = unit(0.25, "npc") - unit(1, "mm"), y = unit(0.35, "npc") - unit(10, "mm"), hjust = 0.5,
                      gp = gpar(family = "News Cycle", fontsize = unit(11, "pt"), col = colors[7], fontface = "bold"))


img_woody_debris <- read_svg(paste0(img_path, "Woody debris.svg"), style_default = list(fill = colors[4]))
img_woody_debris$vp <- viewport(x = unit(0.75, "npc"), y = unit(0.35, "npc") - unit(4, "mm"), width = unit(20, "mm"), height = unit(20, "mm"))
text_woody_debris <- textGrob("Woody debris", x = unit(0.75, "npc") - unit(1, "mm"), y = unit(0.35, "npc") - unit(10, "mm"), hjust = 0.5,
                             gp = gpar(family = "News Cycle", fontsize = unit(11, "pt"), col = colors[4], fontface = "bold"))

img_woody_veg <- read_svg(paste0(img_path, "Woody veg.svg"), style_default = list(fill = colors[6]))
img_woody_veg$vp <- viewport(x = unit(0.5, "npc"), y = unit(0.55, "npc"), width = unit(20, "mm"), height = unit(20, "mm"))
text_woody_veg <- textGrob("Woody veg", x = unit(0.5, "npc") - unit(1, "mm"), y = unit(0.55, "npc") - unit(10, "mm"), hjust = 0.5,
                             gp = gpar(family = "News Cycle", fontsize = unit(11, "pt"), col = colors[6], fontface = "bold"))

legend_background <- rectGrob(x = unit(0, "npc"), y = unit(0.2, "npc"), width = unit(100, "mm"), height = unit(72, "mm"), 
                              just = c("left", "bottom"), gp = gpar(fill = "grey98", col = "grey90"))

plot_legend <- grobTree(legend_background, img_herbaceous, img_open, img_leaf_litter, img_woody_debris, img_woody_veg, text_herbaceous, text_open, text_leaf_litter, text_woody_debris, text_woody_veg,
                        vp = viewport(x = unit(0.84, "npc"), y = unit(1.15, "npc"), width = unit(100, "mm"), height = unit(100, "mm")))

p <- ggplot(plot_df, aes(date, as.factor(Frequency), img_path = img_path, img_colour = img_color)) +
  grid_panel(position_image) +
  grid_group(textGrob("Unique telemetry\nfrequency", x = unit(-0.055, "npc"), y = unit(1.01, "npc"), hjust = 0,
                      gp = gpar(family = "News Cycle", fontface = "bold", fontsize = unit(10, "pt"), col = "grey60", lineheight = 0.9))) +
  grid_group(plot_legend) +
  scale_x_date(date_breaks = "2 weeks", limits = c(ymd(20180901), ymd(20181130)), labels = scales::label_date("%b-%d")) +
  labs(
    title = "Hop, skip and jump - a frog's journey",
    subtitle = str_wrap("Using radio-telemetry, the United States Geological Survey (USGS) studied late-season movement of Oregon spotted frogs (Rana pretiosa) at Crane Prairie Reservoir in Oregon from early September till late November 2018. The below chart shows the timelines for ten frogs on when they changed their habitate structure.", 
                        90),
    caption = "Data: usgs.gov spotted frog data | Plot: Kaustav Sen | Icons from the Noun Project") +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 16, base_family = "News Cycle") +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(size = rel(0.8), hjust = 0.5, color = "grey70", face = "bold", margin = margin(t = 15, b = 5)),
        plot.margin = margin(20, 30, 10, 15),
        plot.title = element_text(family = "Chewy", size = rel(2.7), color = colors[7], hjust = 0),
        plot.subtitle = element_text(size = rel(1), color = "grey60", lineheight = 1.2, margin = margin(t=3, b = 60)),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.9, linetype = "dashed", color = "grey93"),
        axis.text.y = element_text(hjust = 0, face = "bold", size = rel(1.1)),
        axis.text.x = element_text(color = "grey50", face = "bold", size = rel(0.8)),
        axis.ticks.x = element_line(size = 0.6, color = "grey50"),
        axis.ticks.length.x = unit(3, "mm"),
        axis.title = element_blank())
ggsave("2022/plots/week_31.png", plot = p, width = 16, height = 12, dpi = 300)
