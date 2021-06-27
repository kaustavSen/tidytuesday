library(tidyverse)
library(ggbump)
library(patchwork)

theme_set(theme_light(base_family = "Montserrat", base_size = 13))
theme_update(
  plot.margin = margin(10, 10, 10, 10),
  plot.title = element_text(family = "Poppins", hjust = 0.5, face = "bold", color = "#40916c", size = rel(1.8)),
  plot.caption = element_text(hjust = 0.5, family = "Roboto Condensed", color = "grey60", size = rel(1), face = "bold"),
  axis.title = element_text(family = "Roboto Condensed", color = "grey30", face = "bold", margin = margin(5, 5, 5, 5)),
  axis.text = element_text(family = "JetBrains Mono")
)

# Load in the data
tues_data <- tidytuesdayR::tt_load(2021, week = 26)
parks <- 
  tues_data$parks %>% 
  mutate(spend_per_resident_data = parse_number(spend_per_resident_data))

# Taking a quick look at the data
View(parks)
skimr::skim(parks)

# How has spending on parks changed over time? 
spending_parks <- 
  parks %>% 
  select(year, city, spend_per_resident_data)
  
spending_parks %>% 
group_by(year) %>% 
  summarise(median_spend = median(spend_per_resident_data)) %>% 
  ggplot(aes(year, median_spend)) +
  geom_point() +
  geom_line()

spending_parks %>% 
  mutate(city = fct_lump(city, 10, w = spend_per_resident_data)) %>% 
  filter(city != "Other") %>% 
  ggplot(aes(year, spend_per_resident_data, color = city)) +
  geom_line() +
  facet_wrap(~city)

# How does the ranking correlate with spending  
ggplot(parks, aes(rank, spend_per_resident_data)) +
  geom_jitter(alpha = 0.5) +
  geom_smooth(formula = y ~ x) +
  labs(title = "Ranking increases with increase in spending")

# Which cities have been topped ranked over the years?
parks %>% 
  filter(rank %in% 1:10) %>% 
  group_by(city) %>% 
  mutate(count = n()) %>% 
  filter(count > 5) %>% 
  ggplot(aes(year, rank, color = city)) +
  geom_bump() + 
  geom_point(size = 2) +
  scale_y_reverse(breaks = seq(1, 10, 2)) +
  facet_wrap(~city) +
  theme(legend.position = "none")

# Investigating San Francisco parks in more detail
parks_sf <- 
  parks %>% 
  filter(city == "San Francisco") %>% 
  select(year, rank, spend_per_resident_data)

p1 <- 
  ggplot(parks_sf, aes(year, spend_per_resident_data)) +
  geom_bump(size = 1.1, color = "#74c69d") +
  geom_point(size = 3, color = "#52b788") +
  geom_point(size = 4.5, stroke = 1.1, color = "#b7e4c7", shape = 1) +
  geom_text(aes(x = 2016, y = 353, angle = 20 + 13.375, label = "An increase of about 37% over a period of 8 years"), 
             family = "Roboto Condensed", color = "#2d6a4f") +
  geom_segment(aes(x = 2012, xend = 2020, y = 292, yend = 399), arrow.fill = "#52b788", color = "#52b788",
               arrow = arrow(end = "both", length = unit(2.5, "mm"), type = "closed")) +
  labs(
    subtitle = str_wrap("San Francisco has consistenly increased its spending on parks over the years", 55),
    x = "",
    y = "Spending per resident in USD"
  )

p2 <- 
  ggplot(parks_sf, aes(year, rank)) +
  geom_bump(size = 1.1, color = "#74c69d") +
  geom_point(size = 3, color = "#52b788") +
  geom_point(size = 4.5, stroke = 1.1, color = "#b7e4c7", shape = 1) +
  scale_y_reverse(position = "right") +
  labs(
    subtitle = str_wrap("...however, despite the increased spending its rank has fallen dramatically", 55),
    x = "",
    y = "Rank"
  )

plot_final <- 
  p1 + p2 & 
  plot_annotation(
    title = "The curious case of San Francisco park rankings",
    caption = "Data: TPL | Plot: Kaustav Sen"
  )

ggsave(here::here(2021, "plots", "week_26.png"), width = 12, height = 8, dpi = 150)

# Using {rtweet} to directly post the tweet from R! 
post_tweet(
  status =
  "#TidyTuesday Week 26: Looked at how San Francisco park ranking have been over the years.
  #RStats code: https://github.com/kaustavSen/tidytuesday/blob/master/2021/week_26.R",
  media = here::here(2021, "plots", "week_26.png"),
  media_alt_text =
  "Two line charts with one showing the spending per resident and other 
  showing park ranking over the years. The charts show that San Fransico has
  consistently increased its spending on parks over the years. However, despite
  the increased spending its ranking has fallen significantly."
)