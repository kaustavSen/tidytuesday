library(tidyverse)
library(grid)
library(gggrid)
library(patchwork)

summary <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/summary.csv')

color_higher <- "#2ec4b6"
color_lower <- "#de172e"
  
plot_data <- summary |>
  mutate(
    fill_finale = if_else(viewers_premier >= viewers_finale, color_lower, color_higher),
    fill_reunion = if_else(viewers_premier >= viewers_reunion, color_lower, color_higher)
  )

# Add markers to aid creating annotation lines at the end
marker <- function(data, coords) {
  nullGrob(
    x = coords$x[which.max(coords$y)],
    y = max(coords$y),
    name = "mark"
  )
}

plot_fuction <- function(data, y_var, y_fill) {
  x_var_str <- substitute(y_var) |> deparse()
  label_lower <- if(x_var_str == "viewers_finale") {
    "Finale viewership lower\nthan Premier viewship"
  } else {
    "Reunion viewership lower\nthan Premier viewship"
  }
  
  label_higher <- if(x_var_str == "viewers_finale") {
    "Finale viewership higher\nthan Premier viewship"
  } else {
    "Reunion viewership higher\nthan Premier viewship"
  }
  
  y_axis_position <- if_else(x_var_str== "viewers_finale", "left", "right")
  
  ggplot(data, aes(viewers_premier, {{ y_var }}, fill = {{ y_fill }})) +
    geom_area(
      data = tibble(x = seq(0, 55, 0.5)),
      aes(x = x, y = x), fill = "grey95", alpha = 0.5
    ) +
    geom_point(size = 5, shape = 21, color = "white", 
               alpha = 0.5, stroke = 0.75) +
    grid_panel(marker) +
    geom_abline(slope = 1) +
    annotate("text", x = 35, y = 10, label = label_lower, 
             family = "Roboto Condensed", fontface = "bold", size = 4,
             lineheight = 0.9, hjust = 0, color = color_lower) +
    annotate("text", x = 2.5, y = 30, label = label_higher, 
             family = "Roboto Condensed", fontface = "bold", size = 4,
             lineheight = 0.9, hjust = 0, color = color_higher) +
    scale_x_continuous("", expand = c(0,0), limits = c(0, 55)) +
    scale_y_continuous("", expand = c(0,0), limits = c(0, 55), position = y_axis_position) +
    scale_fill_identity() +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 14, base_family = "Roboto Condensed") +
    theme(
      panel.border = element_rect(fill = NA, size = 0.5),
      panel.grid.minor = element_blank()
    ) 
}

# Create plots
plot_finale <- plot_fuction(plot_data, viewers_finale, fill_finale)
plot_reunion <- plot_fuction(plot_data, viewers_reunion, fill_reunion)
plot <- plot_finale + plot_reunion + 
  plot_annotation(
    title = "Survivor: Is the viewership of premier episodes more than those of finale and reunions?",
    caption = "Data: survivorR Package | Plot: Kaustav Sen"
  ) & 
  theme(
    plot.title = element_text(family = "Roboto Condensed", size = 18, face = "bold", hjust = 0.5),
    plot.caption.position = "plot",
    plot.caption = element_text(family = "Roboto Condensed", size = 11, face = "bold")
  )

# Create axis-labels
x_axis_rect <- rectGrob(x = 0, y = 0, width = 1, height = 0.2, 
                        just = "left", gp = gpar(fill = "grey60", col = "grey60", alpha = 0.8))
x_axis_text <- textGrob("Viewers (millions) at premier", x = 0.5, y = 0.05,
                        gp = gpar(fontfamily = "Roboto Condensed", fontsize = 16, 
                                  col = "white", fontface = "bold"))
x_axis_label <- gTree(children = gList(x_axis_rect, x_axis_text))

y_axis_left <- rectGrob(x = 0, y = 0, width = 0.1, height = 1,
                        just = "bottom", gp = gpar(fill = "grey60", col = "grey60", alpha = 0.8))
y_axis_left_text <- textGrob("Viewers at finale", x = 0.025, y = 0.5, rot = 90,
                        gp = gpar(fontfamily = "Roboto Condensed", fontsize = 16, 
                                  col = "white", fontface = "bold"))
y_axis_left_label <- gTree(children = gList(y_axis_left, y_axis_left_text))

y_axis_right <- rectGrob(x = 0.95, y = 0, width = 0.1, height = 1,
                        just = c("left", "bottom"), gp = gpar(fill = "grey60", col = "grey60", alpha = 0.8))
y_axis_right_text <- textGrob("Viewers at reunion", x = 0.975, y = 0.5, rot = -90,
                             gp = gpar(fontfamily = "Roboto Condensed", fontsize = 16, 
                                       col = "white", fontface = "bold"))
y_axis_right_label <- gTree(children = gList(y_axis_right, y_axis_right_text))

# Observational Comment
comment <- "The first season of Survivor, Borneo had the highest finale viewership as well as reunion viewership"
comment <- str_wrap(comment, width = 30)

comment_text <- textGrob(comment, x = 0.3, y = 0.82, just = "left",
                         gp = gpar(fontfamily = "Roboto Condensed", fontsize = 12, 
                                   lineheight = 0.9, col = "grey60"))

comment_textbox <- roundrectGrob(x = 0.28, y = 0.82, just = "left",
                            width = widthDetails(comment_text),
                            height = heightDetails(comment_text),
                            gp = gpar(col = "white", fill = "grey98", lwd = 1.5))

plot_viewport <- viewport(x = 0.1, y = 0.1, 
                          width = 0.8, height = 0.88,
                          just = c("left", "bottom"))

# Final plotting + annotations
ragg::agg_png(here::here(2021, "plots", "week_23.png"), 
              width = 14, height = 6, units = "in", res = 320)
grid.newpage()
grid.draw(x_axis_label)
grid.draw(y_axis_left_label)
grid.draw(y_axis_right_label)
pushViewport(plot_viewport)
plot(plot, newpage = FALSE)
upViewport()
grid.force()
markers <- grid.grep("mark", global = TRUE, viewports = TRUE)
# Create a function to draw the annotation lines
# The below is inspired by Paul Murrell's report:
# https://www.stat.auckland.ac.nz/~paul/Reports/gggrid/gggrid.html
drawLine <- function(x) {
  depth <- downViewport(attr(x, "vpPath"))
  m <- grid.get(x)
  loc <- deviceLoc(m$x, m$y)
  upViewport(depth)
  grid.segments(
    x0 = grobX(comment_text, 0),
    y0 = grobY(comment_text, 0),
    x1 = loc$x,
    y1 = loc$y,
    arrow = arrow(length = unit(3, "mm"), type = "closed"),
    gp = gpar(col = "grey40", fill = "grey60", lwd = 1.5, alpha = 0.5)
  )
}
lapply(markers, drawLine)
grid.draw(comment_textbox)
grid.draw(comment_text)
dev.off()