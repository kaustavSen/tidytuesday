library(tidyverse)

theme_set(ggthemes::theme_fivethirtyeight(base_family = "JetBrains Mono", base_size = 12))

lap_times <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/lap_times.csv')
races <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/races.csv')
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-07/drivers.csv')

combined_data <- lap_times %>% 
  left_join(races, by = "raceId") %>% 
  left_join(drivers, by = "driverId")
  
main_races <- c("British Grand Prix", "Belgian Grand Prix", "Hungarian Grand Prix",
                "Spanish Grand Prix", "Italian Grand Prix")

combined_data_main <- combined_data %>% 
  filter(name %in% main_races)

plot_data <- combined_data %>% 
  filter(year <= 2020) %>% 
  group_by(name, year) %>% 
  summarise(time = min(time.x),
            time = time / 60) 

plot_data_main <- combined_data_main %>% 
  filter(year <= 2020) %>% 
  group_by(name, year) %>% 
  summarise(time = min(time.x),
            time = time / 60) 

ggplot(plot_data, aes(year, time, group = name)) +
  geom_line(color = "grey80", size = 0.8) +
  geom_line(data = plot_data_main, size = 0.9, aes(color = name)) +
  geom_point(data = plot_data_main, aes(fill = name), 
             size = 3, shape = 21, stroke = 1, color = "#F0F0F0") +
  geom_text(
    data = filter(plot_data_main, year == 2020),
    aes(x = year + 0.4, label = str_replace(name, "Grand Prix", "GP"), color = name),
    family = "Robtoto Condensed", size = 3.5, fontface = "bold.italic",
    hjust = 0, nudge_y = 0.4
  ) +
  scale_x_continuous(limits = c(1995, 2021), breaks = seq(1995, 2020, 5)) +
  scale_y_time(labels = function(x) strftime(x, "%M:%S")) +
  paletteer::scale_color_paletteer_d(palette = "colRoz::a_westwoodi") +
  paletteer::scale_fill_paletteer_d(palette = "colRoz::a_westwoodi") +
  labs(
    title = "Has F1 become faster over time?",
    y = "FASTEST LAP TIME",
    caption = "**Data:**  Ergast API | **Plot:** Kaustav Sen"
  ) +
  coord_cartesian(clip = "off", expand = 0) +
  theme(
    legend.position = "none",
    plot.margin = margin(20, 80, 10, 20),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = ggtext::element_markdown(family = "Roboto Condensed", hjust = 1.12,
                                size = rel(0.9), color = "grey70", margin = margin(t = 10)), 
    plot.title = element_text(family = "Roboto Condensed", face = "bold",
                              size = rel(2.5), margin = margin(b = 15), color = "#bb3e03"),
    axis.title.y = element_text(family = "Roboto Condensed", face = "bold",
                                size = rel(0.9), hjust = 1, color = "grey60")
  )
ggsave("2021/plots/week_37.png", width = 12, height = 7, dpi = 150)
