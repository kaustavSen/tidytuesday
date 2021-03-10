library(tidyverse)
library(ragg)
library(showtext)
library(here)

tues_data <- tidytuesdayR::tt_load(2021, week = 11)

movies <- tues_data$movies

font_add_google("B612 Mono", "numbers")
font_add_google("Roboto", "text")
font_add_google("Staatliches", "caption")

bechdel_test_df <- 
  movies %>% 
  mutate(
    year_group = case_when(
      between(year, 1970, 1974) ~ "1970 -\n'74",
      between(year, 1975, 1979) ~ "1975 -\n'79",
      between(year, 1980, 1984) ~ "1980 -\n'84",
      between(year, 1985, 1989) ~ "1985 -\n'89",
      between(year, 1990, 1994) ~ "1990 -\n'94",
      between(year, 1995, 1999) ~ "1995 -\n'99",
      between(year, 2000, 2004) ~ "2000 -\n'04",
      between(year, 2005, 2009) ~ "2005 -\n'09",
      TRUE                      ~ "2010 -\n'13"
    )
  ) %>% 
  group_by(year_group, clean_test) %>% 
  summarise(category_count = n()) %>% 
  group_by(year_group) %>% 
  mutate(category_prop = category_count / sum(category_count)) %>% 
  ungroup()

bechdel_step_df <-
  bechdel_test_df %>% 
  filter(clean_test %in% c("ok", "dubious")) %>% 
  group_by(year_group) %>% 
  summarise(category_prop = sum(category_prop)) %>% 
  mutate(x_coord = seq(0.5, 9, 1), x_end_coord = seq(1.5, 9.5, 1), y_coord = category_prop, y_end_coord = lead(category_prop))

bechdel_test_text <-
  bechdel_test_df %>% 
  mutate(clean_test = factor(clean_test, levels = c("ok", "dubious", "men", "notalk", "nowomen"))) %>% 
  filter(year_group == "2010 -\n'13") %>% 
  arrange(clean_test) %>% 
  mutate(prop_cum = cumsum(category_prop),
         prop_cum_lag = lag(prop_cum),
         label = c("Passes\nBechdel\nTest", "Dubious", "Women only\ntalk about men", "Women don't\ntalk to each\nother", "Fewer than\ntwo women")) %>% 
  rowwise() %>% 
  mutate(
    y = sum(category_prop / 2, prop_cum_lag, na.rm = TRUE)
  )

showtext_auto()

bechdel_test_df %>% 
  mutate(clean_test = factor(clean_test, levels = c("ok", "dubious", "men", "notalk", "nowomen")),
         clean_test = fct_rev(clean_test)) %>% 
  ggplot(aes(year_group, category_prop)) +
  geom_col(aes(fill = clean_test), width = 1, color = "white", size = 0.6, show.legend = FALSE) +
  geom_segment(data = bechdel_step_df, aes(x = x_coord, xend = x_end_coord, y = category_prop, yend = category_prop), size = 1.5) +
  geom_segment(data = filter(bechdel_step_df, year_group != "2010 -\n'13"), aes(x = x_end_coord, xend = x_end_coord, y = y_coord, yend = y_end_coord), lineend = "round", size = 1.5) +
  geom_segment(aes(x = 0.12, xend = 9.5, y = 0, yend = 0), size = 0.8) +
  geom_segment(aes(x = 0.25, xend = 9.5, y = 1, yend = 1), size = 0.8, color = "#cdcdcd") +
  geom_segment(data = tibble(x = 0.12, xend = 0.5, y = c(0.25, 0.5, 0.75)), aes(x = x, xend = xend, y = y, yend = y), size = 0.8, color = "#cdcdcd") +
  geom_text(data = tibble(x = 0, y = c(0, 0.25, 0.5, 0.75, 1), label = c(0, 25, 50, 75, 100)), aes(x = x, y = y, label = label), family = "numbers", size = 6, hjust = 1) +
  geom_text(data = tibble(x = 0.2, y = 1, label = "%"), aes(x = x, y = y, label = label), family = "numbers", size = 7, hjust = 1) +
  geom_text(data = tibble(x = c(0.5, 2.5, 4.5, 6.5, 8.5), y = -0.06, label = c("1970-\n'74", "1980-\n'84", "1990-\n'94", "2000-\n'04", "2010-\n'13")), aes(x = x, y = y, label = label), family = "numbers", size = 6, hjust = -0.2, lineheight = 0.55) +
  geom_segment(data = bechdel_test_text, aes(x = 9.5, xend = 9.75, y = y, yend = y), size = 0.8) +
  geom_text(data = bechdel_test_text, aes(x = 9.85, y = y, label = label), family = "text", hjust = 0, vjust = 0.5, size = 6, lineheight = 0.6) +
  annotate("text", x = 3.75, y = 0.22, label = "PASS", family = "text", fontface = "bold", size = 25, hjust = 0, vjust = 0.5) +
  annotate("text", x = 4, y = 0.75, label = "FAIL", family = "text", fontface = "bold", size = 25, hjust = 0, vjust = 0.5) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("ok" = "#008fd5", "dubious" = "#6bb2d5", "men" = "#ffc9bf", "notalk" = "#ff9380", "nowomen" = "#ff2700")) +
  labs(title = "The Bechdel Test Over Time", subtitle = "How women are represented in movies", x = "", y = "", caption = "Original plot by Fivethirtyeight | Replicated in R by Kaustav Sen") +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(family = "text", face = "bold", size = 30, hjust = -0.12, margin = margin(b = 5)),
    plot.subtitle = element_text(family = "text", size = 24, hjust = -0.12, margin = margin(b = 25)),
    plot.caption = element_text(family = "caption", size = 14, hjust = 0.5, vjust = -25, color = "grey70"),
    plot.margin = margin(20, 90, 25, 45),
    plot.background = element_rect(fill = "#f0f0f0", color = "#f0f0f0")
  ) +
  ggsave(here("2021", "plots", "week_11.png"), height = 6, width = 8, dpi = 150, device = agg_png())
