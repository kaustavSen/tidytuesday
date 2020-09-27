# By: Sil Aarts
# Source: https://github.com/silaarts/TidyTuesday/blob/master/TidyTuesday_Crop_corn.R

library(tidyverse)
library(ggforce)
library(cowplot)
library(patchwork)
library(showtext)

crop <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')

colnames(crop)[6] <- "Maize"

crop$Entity[crop$Entity == "Bosnia and Herzegovina"] <- "Bosnia &\nHerzegovina"
crop$Entity[crop$Entity == "Saint Vincent and the Grenadines"] <- "Saint Vincent\n&\nGrenadines"
crop$Entity[crop$Entity == "United Arab Emirates"] <- "United Arab\nEmirates"
crop$Entity[crop$Entity == "New Caledonia"] <- "New\nCaledonia"

data <- crop %>% 
  select(1:3, 6) %>% 
  filter(Year == 2018) %>% 
  drop_na(Code) %>% 
  top_n(50, Maize)

font_add_google("Calligraffitti", "P")
font_add_google("Bad Script", "B")
showtext_auto()

p1 <- 
  ggplot(data, aes(fill = Entity, alpha = Maize)) +
  geom_ellipse(aes(x0 = 0, y0 = 9, a = 7, b = 3, angle = pi /2, m1 = 3),
               fill = "gold3", color = "coral4")  +
  geom_ellipse(aes(x0 = 6.1, y0 = 6.1, a = 5, b = 1, angle = pi / 4, m1 = 1),
               fill = "olivedrab4", color = "transparent", alpha = 1) +
  geom_ellipse(aes(x0 = 6.2, y0 = 9.2, a = 3, b = 1, angle =  pi / 3, m1 = 1), 
               fill="olivedrab4",  color = "transparent", alpha=1) +
  geom_ellipse(aes(x0 = -6.1, y0 = 6.1, a = 5, b = 1, angle = 3*pi / 4, m1 = 1),
               fill = "olivedrab4", color = "transparent", alpha = 1) +
  geom_ellipse(aes(x0 = -6.2, y0 = 9.2, a = 3, b = 1, angle = 2*pi / 3, m1 = 1), 
               fill="olivedrab4",  color = "transparent", alpha=1) +
  facet_wrap(~ Entity, nrow = 5) +
  theme_void() +
  theme(
    legend.position = "none",
    strip.text.x = element_text(family = "B", size = 9, color = "white",
                                margin = margin(0.1, 0, 0.1, 0, "cm")),
    strip.background = element_rect(fill = "transparent", colour = "black", 
                                    size = 1),
    plot.background = element_rect(fill = "grey64", colour = "grey64"),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

plot_legend <- 
  ggplot(data) +
  geom_ellipse(aes(x0 = 0, y0 = 9, a = 7, b = 3, angle = pi /2, m1 = 3),
               fill = "gold3", color = "coral4")  +
  geom_ellipse(aes(x0 = 6.1, y0 = 6.1, a = 5, b = 1, angle = pi / 4, m1 = 1),
               fill = "olivedrab4", color = "transparent", alpha = 1) +
  geom_ellipse(aes(x0 = 6.2, y0 = 9.2, a = 3, b = 1, angle =  pi / 3, m1 = 1), 
               fill="olivedrab4",  color = "transparent", alpha=1) +
  geom_ellipse(aes(x0 = -6.1, y0 = 6.1, a = 5, b = 1, angle = 3*pi / 4, m1 = 1),
               fill = "olivedrab4", color = "transparent", alpha = 1) +
  geom_ellipse(aes(x0 = -6.2, y0 = 9.2, a = 3, b = 1, angle = 2*pi / 3, m1 = 1), 
               fill="olivedrab4",  color = "transparent", alpha=1) +
  geom_mark_circle(aes(x = 0, y = 15, 
                       label = "A lighter color is indicative for\nless tonnes per hectare"),
                   label.fill = "transparent", label.colour = "white", label.buffer = unit(20, "mm"),
                   label.family = "B", expand = unit(1, "mm")) +
  labs(caption="Source: Global Crop Yields & Our World in Data | Plot by @sil_aarts") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey64", color = "grey64"),
    plot.caption = element_text(colour = "white", size = 7, hjust = 0.5, family = "B")
  )

# plot_final <- plot_grid(p1, plot_legend, rel_heights = c(15, 15), rel_widths = c(15, 30))

plot_final <- 
  p1 + plot_legend + 
  plot_layout(widths = c(4, 2)) &
  theme(plot.background = element_rect(fill = "grey64", color = "grey64"))

plot_path <- here::here(2020, "reference", "week_36", "sil_aarts")

ggsave(paste0(plot_path, ".pdf"), plot_final, 
         height = 8, width = 12, device = cairo_pdf)

pdftools::pdf_convert(paste0(plot_path, ".pdf"),
                      filenames = paste0(plot_path, ".png"),
                      dpi = 320)
