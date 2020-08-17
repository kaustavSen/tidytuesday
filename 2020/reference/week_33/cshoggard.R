# Credit: @CSHoggard (https://github.com/CSHoggard/-TidyTuesday/blob/master/R/w32_2020.R)

library(tidyverse)
library(extrafont)
loadfonts()

avatar <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')

avatar_clean <- 
  avatar %>%
  mutate(
    book_chapter = case_when(
      book == "Water" ~ chapter_num,
      book == "Earth" ~ chapter_num + 21,
      book == "Fire" ~ chapter_num + 42
    ),
    imdb_rating = if_else(book_chapter == 20, 9.7, imdb_rating)
  ) %>%
  select(book, director, imdb_rating, book_chapter) %>%
  group_by(book) %>%
  mutate(series_rating = mean(imdb_rating)) %>%
  unique()

ggplot(avatar_clean, aes(imdb_rating, book_chapter, colour = book)) + 
  geom_point() +
  geom_segment(aes(xend = series_rating, yend = book_chapter)) +
  geom_segment(
    data = avatar_clean %>% filter(book == "Water"),
    aes(
      y = min(book_chapter),
      yend = max(book_chapter),
      x = series_rating,
      xend = series_rating
    ),
    size = 0.5,
    color = "grey40"
  ) +
  geom_segment(
    data = avatar_clean %>% filter(book == "Fire"),
    aes(
      y = min(book_chapter),
      yend = max(book_chapter),
      x = series_rating,
      xend = series_rating
    ),
    size = 0.5,
    color = "grey80"
  ) +
  geom_segment(
    data = avatar_clean %>% filter(book == "Earth"),
    aes(
      y = min(book_chapter),
      yend = max(book_chapter),
      x = series_rating,
      xend = series_rating
    ),
    size = 0.5,
    color = "grey60"
  ) + 
  # geom_point(aes(color = book)) +
  labs(x = "IMDb Rating", 
       y = "Chapter",
       title = "Avatar: The Last Airbender",
       subtitle = "(IMDb ratings vs. chapter)",
       caption = "#TidyTuesday data from {appa} (https://github.com/averyrobbins1/appa)") +
  xlim(7,10) + 
  scale_color_manual(values = c(
    "Water" = "grey40",
    "Earth" = "grey60",
    "Fire" = "grey80"
  )) +
  theme(
    plot.background = element_rect(fill = "grey97", linetype = 0),
    panel.background = element_rect(fill = "grey97", linetype = 0),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.background = element_rect(fill = "grey97"),
    legend.key = element_rect(fill = "grey97", color = NA),
    axis.title.x = element_text(color="grey40" , vjust=-0.95),
    axis.title.y = element_text(color="grey40" , vjust=-0.95),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(hjust = .5, size = 16, margin = margin(5, 0, 5, 0), face = "bold"),
    plot.subtitle = element_text(hjust = .5, size = 10, margin = margin(5, 0, 5, 0)),
    plot.caption = element_text(
      hjust = 0.5,
      size = 7,
      family = "Century Gothic",
      color = "grey60"
    ),
    text = element_text(family = "Century Gothic"))

ggsave(here("images", "Week_32_Avatar.png"), dpi = 400)