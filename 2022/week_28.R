library(tidyverse)
library(lubridate)
library(ggtext)
library(scales)

flights <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv')

color_pallette <- NatParksPalettes::NatParksPalettes$Acadia[[1]]

top_10_airports <- c("Frankfurt", "Amsterdam - Schiphol", "Paris-Charles-de-Gaulle", "London - Heathrow", "Madrid - Barajas",
                     "Munich", "Barcelona", "London - Gatwick", "Palma de Mallorca", "Paris-Orly")

flights_top_10_airports <- flights |> 
  filter(APT_NAME %in% top_10_airports)

airport_levels <- flights |> 
  filter(APT_NAME %in% top_10_airports) |> 
  group_by(APT_NAME) |> 
  summarise(tot_deps = sum(FLT_DEP_1), .groups = "drop") |> 
  mutate(APT_NAME = fct_reorder(APT_NAME, tot_deps)) |> 
  pull(APT_NAME)

covid_impact_df <- flights_top_10_airports |> 
  group_by(APT_NAME, STATE_NAME, YEAR, MONTH_NUM) |> 
  summarise(total_deps = sum(FLT_DEP_1),
            .groups = "drop") |> 
  mutate(month = as.numeric(MONTH_NUM)) |> 
  filter(month %in% 1:5) |> 
  mutate(era = case_when(
    YEAR < 2020 ~ "pre_COVID",
    YEAR == 2022 ~ "post_COVID",
    TRUE ~ "ignore"
  )) |> 
  filter(era != "ignore") |> 
  group_by(APT_NAME, STATE_NAME, era, month) |> 
  summarise(avg_deps = mean(total_deps), .groups = "drop") |> 
  pivot_wider(names_from = era, values_from = avg_deps) |> 
  mutate(perc_change = post_COVID / pre_COVID - 1,
         APT_NAME = factor(APT_NAME, levels = airport_levels))

flag_icon_path <- "2022/week_28/Spain.png"

plot_data <- 
  covid_impact_df |> 
  filter(month == 5) |> 
  mutate(
    APT_NAME = fct_reorder(APT_NAME, pre_COVID),
    color = if_else(perc_change < 0, color_pallette[8], color_pallette[2]),
    arrow_placement = if_else(perc_change < 0, post_COVID - 500, post_COVID + 500),
    arrow_shape = if_else(perc_change < 0, 25, 17),
    text_placement = if_else(perc_change < 0, post_COVID - 1100, post_COVID + 1000)
  )

subtitle <- "Despite relaxations to the lockdown imposed restrictions on air travel, the industry is yet to recover back to its pre-<br>pandemic levels<br><br>Below we look at how the monthly depatures in **May 2022** differs from the pre-pandemic average for the 10 most busiest<br>airports in Europe"

labels <- glue::glue("{plot_data$APT_NAME}<br><span style='font-size: 13px; color: #b3b3b3;'>{plot_data$STATE_NAME}</span>")

ggplot(plot_data) +
  ggforce::geom_link(aes(x = pre_COVID, xend = post_COVID, y = APT_NAME, yend = APT_NAME,
                alpha = stat(index), size = stat(index), color = color), lineend = "round") +
  geom_point(aes(x = arrow_placement, y = APT_NAME, fill = color, color = color, shape = arrow_shape), size = 4, alpha = 0.5) +
  geom_text(aes(x = text_placement, y = APT_NAME, label = percent(perc_change, accuracy = 0.1), color = color), 
            size = 3, family = "Inter", fontface = "bold", vjust = 0.6) +
  annotate("text", x = 12000, y = 9.4, label = "-12.8%", size = 14,
           family = "Inter", fontface = "bold", color = color_pallette[8], alpha = 0.8) +
  annotate("text", x = 12000, y = 8.8, label = "average decrease in\nmonthly departures",
           size = 4, family = "Inter", color = color_pallette[8], lineheight = 0.9) +
  annotate("curve", x = 19500, xend = 18000, y = 5.5, yend = 6, curvature = 0.2, alpha = 0.6,
           color = "grey70", size = 0.8, arrow = arrow(length = unit(2, "mm"), type = "closed")) +
  annotate("text", x = 19000, y = 5.25, label = "Pre-pandemic\nmonthly average",
           family = "Inter", size = 4, color = "grey40", hjust = 0, lineheight = 0.8) + 
  annotate("curve", x = 12200, xend = 13200, y = 6.8, yend = 6, curvature = 0.2, alpha = 0.6,
           color = "grey70", size = 0.8, arrow = arrow(length = unit(2, "mm"), type = "closed")) +
  annotate("text", x = 12200, y = 7, label = "May 2022\ndepartures",
           family = "Inter", size = 4, color = "grey40", hjust = 0.5, lineheight = 0.8) + 
  annotate("text", x = 13600, y = 2, label = "Only for 1 out of the 10 airports that we see an increase\nfrom the historic average",
           size = 4, family = "Inter", hjust = 0, color = color_pallette[2]) +
  scale_shape_identity() +
  scale_color_identity() +
  scale_fill_identity() +
  scale_alpha(range = c(0.1, 0.9)) +
  scale_size(range = c(1, 8)) +
  scale_x_continuous(name = "# of monthly departures", labels = number_format(scale = 1/1000, suffix = "k"), limits = c(NA, 24000),
                     breaks = seq(8000, 24000, 4000), expand = c(0, 0)) +
  scale_y_discrete(labels = labels) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Is this the new normal?",
    subtitle = subtitle,
    caption = "Data: Eurocontrol | Plot: Kaustav Sen"
  ) +
  theme_minimal(base_size = 16, base_family = "Inter") +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(15, 20, 15, 20),
    plot.background = element_rect(fill = "grey98", color = "grey98"),
    plot.title = element_text(family = "Alegreya Sans SC", face = "bold", size = rel(2.5), color = "grey30"),
    plot.caption = element_text(size = rel(0.7), hjust = 0.5, color = "grey70"),
    plot.subtitle = element_markdown(size = rel(0.9), color = "grey50", margin = margin(b = 15)),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(hjust = 1, face = "bold", color = "grey60", size = rel(0.8)),
    axis.text.y = element_markdown(hjust = 0, face = "bold", color = "grey40", lineheight = 1.1, vjust = 0.5),
    panel.grid.major.y = element_line(linetype = "dashed", color = "grey90"),
    legend.position = "none",
  )
ggsave("2022/plots/week_28.png", width = 12, height = 10, dpi = 200)
