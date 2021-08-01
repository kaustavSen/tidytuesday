library(tidyverse)
library(ggthemes)
library(wesanderson)

tues_data <- tidytuesdayR::tt_load(2021, week = 31)
olympics <- tues_data$olympics

sport_countries <- olympics %>% 
  filter(season == "Summer") %>% 
  group_by(year, sport) %>% 
  summarise(countries_unique = list(unique(noc))) %>% 
  rowwise() %>% 
  mutate(countries_count = length(countries_unique)) %>% 
  ungroup()

fct_sport <- sport_countries %>% 
  count(sport, sort = TRUE) %>% 
  mutate(sport = fct_reorder(sport, n)) %>% 
  pull(sport)

sport_countries %>% 
  filter(year != 1906) %>% 
  mutate(sport = factor(sport, levels = levels(fct_sport))) %>% 
  ggplot(aes(year, sport, fill = countries_count)) +
  geom_tile(size = 0.75, color = "#F0F0F0") +
  annotate("rect", xmin = 1914, xmax = 1918.25, ymin = -Inf, ymax = Inf, fill = "#D2D2D2", alpha = 0.6) +
  annotate("text", x = 1916.125, y = 26.5, label = "WORLD WAR I", angle = 90, color = "grey40", family = "Roboto Condensed", fontface = "bold", size = 6) +
  annotate("rect", xmin = 1938, xmax = 1946.25, ymin = -Inf, ymax = Inf, fill = "#D2D2D2", alpha = 0.6) +
  annotate("text", x = 1942.125, y = 26.5, label = "WORLD WAR II", angle = 90, color = "grey40", family = "Roboto Condensed", fontface = "bold", size = 6) +
  scale_x_continuous(breaks = seq(1896, 2016, 8), expand = expansion(mult = 0.02)) +
  scale_fill_gradientn(colours = wes_palette("Zissou1", n = 50, type = "continuous")) +
  guides(fill = guide_colorsteps(label.position = "bottom", title = "Number of countries", title.position = "top", 
                                 title.theme = element_text(family = "Roboto Condensed", hjust = 0.5, size = 16,face = "bold", color = "grey40"))) +
  labs(
    title = "Summer Olympic Games Participation 1896 to 2016",
    caption = "Data: Kaggle | Plot: Kaustav Sen"
  ) +
  theme_fivethirtyeight(base_size = 16, base_family = "Poppins") +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(face = "bold", size = rel(1.8)),
    plot.caption = element_text(size = rel(0.8), hjust = 0, color = "grey40", face = "bold"),
    legend.position = "top",
    legend.justification = c(0.35, 0.8),
    legend.key.width = unit(20, "mm"),
    legend.key.height = unit(4, "mm"),
    panel.grid.minor.x = element_line(color = "#D2D2D2")
  )
ggsave("2021/plots/week_31.png", width = 12, height = 13, dpi = 150)