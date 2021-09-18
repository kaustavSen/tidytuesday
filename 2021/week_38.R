library(tidyverse)
library(ggbump)
library(ggrepel)
library(ggforce)
library(ggfx)
library(patchwork)
library(ragg)

billboard <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/billboard.csv')

pink_floyd <-
  billboard %>% 
  filter(performer == "Pink Floyd") %>% 
  mutate(week_id = lubridate::mdy(week_id)) %>% 
  group_by(song) %>% 
  arrange(week_id) %>% 
  mutate(week_no = row_number()) %>% 
  select(week_id, week_no, song, week_position) %>% 
  ungroup()

# This is to ensure that all songs "emerge" from the same point.
week_0 <- pink_floyd %>% 
  distinct(song) %>% 
  mutate(week_no = 0,
         week_position = 100)

# Tweaking the position of the song labels
label_pos <- tibble(
  song = week_0$song,
  week_no = c(16, 14, 7.1, 11, 4.5),
  week_position = c(45, 6, 60, 97, 85)
)

# colors
colors <- c(
  "Money" = "#CC102D",
  "Another Brick In The Wall (Part II)" = "#E88514",
  "Run Like Hell" = "#7DC62C",
  "Learning To Fly" = "#71C0DE",
  "Take It Back" = "#634C8A"
)

plot_chart_pos <-
  pink_floyd %>% 
  add_row(week_0) %>% 
  ggplot(aes(week_no, week_position, color = song)) +
  with_outer_glow(geom_bump(size = 2), sigma = 4) +
  with_inner_glow(
    geom_text_repel(data = label_pos, aes(label = song),
                    family = "Roboto Condensed", fontface = "bold", 
                    size = 3.5, hjust = 0),
    color = "white",
    sigma = 1
  ) +
  scale_y_reverse() +
  scale_color_manual(values = colors) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey80", color = "grey80"),
    legend.position = "none"
  )

# Draw the prism
prism <- ggplot() +
  with_outer_glow(
    geom_regon(aes(x0 = 0, y0 = 0, sides = 3, angle = 0, r = 5), 
               color = "white", fill = "black", size = 2),
    color = "white",
    sigma = 10
  ) +
  geom_text(aes(x = 0.1, y = 0.5, label = "Pink Floyd"),
            family = "Broadway", color = "white", size = 5) +
  geom_text(aes(x = 0.1, y = -0.2, label = "Billboard 100 topping songs"),
            family = "Roboto Condensed", fontface = "italic", color = "white", size = 2.5) +
  geom_text(aes(x = 0.1, y = -2, label = "Data: Data.World | Plot: Kaustav Sen"),
            family = "Roboto Condensed", color = "white", fontface = "bold", size = 2.5) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey80", color = "grey80")
  )

plot <- plot_chart_pos + inset_element(prism, 0.5, 0.2, 0.8, 0.5) & 
  theme(plot.background = element_rect(fill = "grey80", color = "grey80"))

agg_png("2021/plots/week_38.png", width = 9, height = 6, units = "in", res = 200)
plot(plot)
dev.off()
