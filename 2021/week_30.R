library(tidyverse)
library(lubridate)
library(geofacet)

tues_data <- tidytuesdayR::tt_load(2021, week = 30)
drought <- tues_data$drought

colors <- c("D0" = "#fdd0a2", "D1" = "#fdae6b", "D2" = "#fd8d3c", "D3" = "#e6550d", "D4" = "#a63603")
color_labels <- c("Abnormally\nDry", "Moderate\nDrought", "Severe\nDrought", "Extreme\nDrought", "Exceptional\nDrought")

states <- c("AZ", "NV", "NM", "UT", "CA", "CO")

plot_data <- 
  drought %>% 
  filter(state_abb %in% states, drought_lvl != "None") %>% 
  mutate(date = ISOdate(year(valid_start), month(valid_start), 1),
         date = as_date(date)) %>% 
  select(state_abb, date, drought_lvl, area_pct) %>% 
  group_by(state_abb, date, drought_lvl) %>% 
  summarise(area_pct = first(area_pct)) %>% 
  ungroup() %>% 
  left_join(us_state_grid1, by = c("state_abb" = "code"))

format_percent <- function(x) {
  if_else(x == 100, paste0(x, "%"), paste0(x, " "))
}

plot <- 
  ggplot(plot_data, aes(date, area_pct, fill = drought_lvl)) +
  geom_col(alpha = 0.9) +
  scale_x_date(name = "", expand = c(0, 0)) +
  scale_y_continuous(name = "", expand = c(0 , 0), labels = format_percent) +
  scale_fill_manual(values = colors, labels = color_labels) +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(label.position = "bottom", nrow = 1)) +
  facet_wrap(~name) +
  labs(
    title = "The Dry States of America",
    subtitle = "Percentage of total state area in drought category over the years",
    caption = "Data: Drought Monitor | Plot: Kaustav Sen"
  ) +
  theme_minimal(base_size = 16, base_family = "Roboto") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.key.width = unit(10, "mm"),
    legend.key.height = unit(2, "mm"),
    legend.text = element_text(family = "Roboto Condensed", size = rel(0.6)),
    plot.title.position = "panel",
    plot.caption.position = "plot",
    plot.title = element_text(size = rel(1.9), hjust = 0.5, family = "Roboto Condensed", face = "bold"),
    plot.caption = element_text(hjust = 0.5, size = rel(0.7), color = "grey60"),
    plot.subtitle = element_text(hjust = 0.5, color = "grey60"),
    plot.background = element_rect(fill = "grey95", color = "grey95"),
    plot.margin = margin(t = 10, r = 20, b = 10),
    axis.text.y = element_text(family = "JetBrains Mono"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey70", size = 0.4, linetype = "solid"),
    panel.spacing.x = unit(2, "line"),
    panel.spacing.y = unit(1, "line")
  )
ggsave("2021/plots/week_30.png", plot, width = 12, height = 7, dpi = 150)

