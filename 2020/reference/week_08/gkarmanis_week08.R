library(tidyverse)
library(here)
library(showtext)

here("2020", "reference", "week_08")

font_add_google("IBM Plex Mono")
font_add_google("IBM Plex Sans")
font_add_google("Fira Code")

food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

food_co2 <- 
  food_consumption %>% 
  group_by(country) %>% 
  summarise(co2 = sum(co2_emmission)) %>% 
  slice_max(co2, n = 10) %>% 
  arrange(co2) %>% 
  mutate(n = -4:5) %>% 
  rowwise() %>% 
  mutate(
    x = list(c(-10, 0, 0, -10)),
    y = list(c(n*4 - 1.4, n*2 - 0.7, n*2 + 0.7, n*4 + 1.4))
  ) %>% 
  unnest(cols = c(x, y))

showtext_auto()

ggplot(food_co2) +
  geom_rect(aes(xmin = -42, ymin = n*4 - 1.4,
                xmax = -10, ymax = n*4 + 1.4),
            fill = "black", color = NA) +
  geom_polygon(aes(x, y, group = n), 
               fill = "black", color = NA) +
  geom_rect(aes(xmin = 0, ymin = n*2 - 0.7,
                xmax = co2/25, ymax = n*2 + 0.7),
            fill = "black", color = NA) +
  geom_text(aes(-40.5, n*4, label = country),
            color = "white", hjust = 0,
            fontface = "bold",
            size = 4.5, family = "IBM Plex Sans",
            check_overlap = TRUE) +
  geom_text(aes(co2/25-1, n*2, label = co2),
            family = "IBM Plex Mono",
            size = 3.5,
            hjust = 1,
            color = "white") +
  annotate("text", 85, 17, label = "Total food carbon footprint\nKg CO2 / person / year",
           family = "IBM Plex Sans",
           fontface = "bold",
           hjust = 1,
           size = 8,
           lineheight = 0.9) +
  scale_x_continuous(breaks = seq(0, 80, 20), labels = seq(0, 80*25, 20*25)) +
  theme_minimal(base_family = "Fira Code") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey50", size = 0.2),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  ) + 
  ggsave(here("2020", "reference", "week_08", "co2.pdf"),
         height = 6, width = 8) 

pdftools::pdf_convert(here("2020", "reference", "week_08", "co2.pdf"),
                      filenames = here("2020", "reference", "week_08", "co2.png"),
                      dpi = 320)
