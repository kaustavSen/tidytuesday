extrafont::loadfonts(device = "win")
library(tidyverse)

astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

astronauts %>% 
  mutate(decade = year_of_mission %/% 10 * 10) %>% 
  group_by(name, decade) %>% 
  add_count(name = "decade_missions") %>% 
  ungroup() %>% 
  distinct(year_of_birth, total_number_of_missions, total_hrs_sum, total_eva_hrs, year_of_mission,
           decade, decade_missions)

astro_table <- astronauts %>% 
  mutate(decade = year_of_mission %/% 10 * 10) %>% 
  group_by(name, decade) %>% 
  add_count(name = "decade_missions") %>% 
  ungroup() %>% 
  distinct(year_of_birth, name, total_number_of_missions, total_hrs_sum, total_eva_hrs, year_of_mission, decade, decade_missions) %>% 
  mutate(
    name = fct_reorder(name, total_hrs_sum),
    total_hrs_sum = round(total_hrs_sum),
    total_eva_hrs = round(total_eva_hrs, 1)
  ) %>% 
  filter(total_hrs_sum > 10000)

colorbar <- data.frame(name = "colorbar", decade = seq(1960, 2010, by = 10), decade_missions = 1:6)

family1 = "Cooper Black"
family1l = "Century Gothic"
family1b = "Century Gothic"
family2 = "Century Gothic"

ggplot(astro_table) +
  # bars --------------------------------------------------------------------
geom_tile(aes(1985, name, height = 0.4, width = 60), fill = "grey95") +
  geom_tile(aes(decade, name, fill = decade_missions, height = 0.35)) +
  annotate("text", 1985, 29.6, label = "'70                        '10", colour = "grey70", 
           family = family1l,
           vjust = 0, lineheight = 0.8, size = 1.5) +
  # text rows ---------------------------------------------------------------
geom_text(aes(1430, name, label = name), hjust = 0, family = family1, size = 3.5, check_overlap = TRUE) +
  geom_text(aes(1700, name, label = year_of_birth), hjust = 1, family = family2, colour = "grey70", size = 3.5, check_overlap = TRUE) +
  geom_text(aes(1790, name, label = scales::comma(total_hrs_sum)), hjust = 1, family = family2, size = 3.5, check_overlap = TRUE) +
  geom_text(aes(1865, name, label = scales::number(total_eva_hrs, accuracy = 0.1)), hjust = 1, family = family2, size = 3.5, check_overlap = TRUE) +
  geom_text(aes(1925, name, label = total_number_of_missions), hjust = 1, family = family2, size = 3.5, check_overlap = TRUE) +
  # column titles -----------------------------------------------------------
annotate("text", 1700, 30.8, label = toupper("year of\nbirth"), colour = "grey60", family = family1, hjust = 1, vjust = 0, lineheight = 0.8, size = 2.5) +
  annotate("text", 1790, 30.8, label = toupper("total mission\nhours"), colour = "grey60", family = family1, hjust = 1, vjust = 0, lineheight = 0.8, size = 2.5) +
  annotate("text", 1865, 30.8, label = toupper("total eva\nhours"), colour = "grey60", family = family1, hjust = 1, vjust = 0, lineheight = 0.8, size = 2.5) +
  annotate("text", 1925, 30.8, label = toupper("total\nmissions"), colour = "grey60", family = family1, hjust = 1, vjust = 0, lineheight = 0.8, size = 2.5) +
  # stars -------------------------------------------------------------------
# geom_text(data = subset(astro_table, total_hrs_sum == max(astro_table$total_hrs_sum)), aes(1775, name, label = "★"), colour = "darkgoldenrod1") +
#   geom_text(data = subset(astro_table, total_eva_hrs == max(astro_table$total_eva_hrs)), aes(1845, name, label = "★"), colour = "darkgoldenrod1") +
#   geom_text(data = subset(astro_table, total_number_of_missions == max(astro_table$total_number_of_missions)), aes(1910, name, label = "★"), colour = "darkgoldenrod1") +
  # table lines -------------------------------------------------------------
geom_hline(yintercept = seq(0.5, 30.5, by = 1), size = 0.25, colour = "grey70") +
  # colorbar ----------------------------------------------------------------
annotate("text", 1985, 32.3, label = toupper("missions\nper decade"), 
         colour = "grey60", family = family1, lineheight = 0.8, size = 2.5) +
  geom_tile(data = colorbar, aes(decade, 31.25, fill = decade_missions, height = 0.25)) +
  annotate("text", 1985, 30.8, label = toupper("fewer      more"), 
           colour = "grey60", family = family1, vjust = 0, lineheight = 0.8, size = 2) +
  # scales, theme, etc ------------------------------------------------------
coord_fixed(ratio = 20, clip = "off") +
  scale_x_continuous(limits = c(1430, 2015), expand = c(0, 0)) +
  scale_fill_gradient(low = "lightskyblue1", high = "purple4") +
  labs(
    title = "Astronauts with more than 10,000 mission hours",
    caption = "Source: M. Stavnichuk & T. Corlett (https://doi.org/10.1016/j.lssr.2020.06.003) | Graphic: Georgios Karamanis"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(5, 10, 20, 10),
    plot.title = element_text(family = family1, margin = margin(0, 0, 5, 0)),
    plot.caption = element_text(family = family1l, hjust = 0.5, colour = "grey70")
  ) +
  ggsave("2020/reference/astro-table.png", dpi = 320, width = 7, height = 10, type = "cairo")