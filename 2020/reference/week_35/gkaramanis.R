# Source: https://github.com/gkaramanis/tidytuesday/blob/master/2020-week35/chopped.R

library(tidyverse)
library(ggforce)
library(showtext)
library(here)

chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')

cuts <- 
  chopped %>% 
  filter(str_detect(episode_notes, "cut h")) %>%
  count(season) %>% 
  mutate(
    n = as.numeric(n),
    n = case_when(
      season %in% c(7, 39) ~ n + 1, # Two cuts in S17E11 and S39E08
      TRUE ~ n
    )
  )

blade <- data.frame(
  x = c(0.5, 46.5, 46.5, 46, 45.5, 45, 43, 0.5),
  y = c(0, 0, 17.5, 18.5, 19, 19.5, 20, 20),
  color = "#E0E0E0"
)

handle <- data.frame(
  x = c(-30, 5, 5, -30),
  y = c(20, 20, 14, 14),
  color = "#55555B"
)

bg  <-  "#F2B953"
f1  <-  "Montserrat"
f1b <-  "Lilita One"
f2b <-  "Lilita One"

font_add_google(f1)
font_add_google(f1b)
font_add_google(f2b)

showtext_auto()

ggplot(cuts) +
  geom_shape(
    data = handle,
    aes(x = x, y = y - 0.5, fill = color),
    radius = unit(0.5, "cm")
  ) +
  geom_polygon(
    data = blade,
    aes(x = x, y = y - 0.5, fill = color)
  ) +
  annotate("point", x = c(-3, -7, -11), y = 16.5, 
           size = 4.5, color = "#E0E0E0") +
  annotate("point", x = 43.5, y = 16, size = 9, color = bg) +
  annotate("tile", x = 23.5, y = 0, height = 2, width = 46,
           fill = "#F2F3FB", color = NA) +
  annotate("segment", x = -2, xend = 46, 
           y = 5 * -1:-5, yend = 5 * -1:-5, 
           color = "grey97", size = 0.2) +
  annotate("text", x = -4, y = 5 * -1:-5, 
           label = 1:5, color = "grey97", 
           family = f1, size = 5) +
  annotate("text", x = -8, y = -5, label = "Number of cut injuries",
           hjust = 1, family = f1b, size = 4.5, color = "grey97") +
  annotate("text", x = seq(5, 45, by = 5), y = 2, 
           label = seq(5, 45, by = 5), 
           family = f1, size = 4.5) +
  annotate("tile", x = seq(5, 45, by = 5), y = 0, 
           height = 2, width = 0.25, 
           fill = "#E0E0E0", color = NA) +
  annotate("text", x = -8, y = 2, label = "Season", 
           hjust = 1, family = f1b, size = 6) +
  geom_col(aes(x = season, y = -5 * n), fill = "#AA0000") +
  annotate("text", x = 4, y = 16,
           label = "Cut injuries in Chopped", hjust = 0,
           size = 8, family = f2b, vjust = 0.6) +
  annotate("text", x = 4.4, y = 13,
           label = "Cuts per season, as mentioned in episode notes", 
           hjust = 0, size = 4, family = f1, vjust = 0.6) + 
  scale_fill_identity() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg, color = bg),
    plot.margin = margin(20, 25, 20, 25),
  ) +
  ggsave(here("2020", "reference", "week_35", "gkaramanis.pdf"),
         width = 9, height = 6)
