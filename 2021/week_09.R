library(tidyverse)
library(glue)
library(tsibble)
library(here)
library(ggalt)
library(showtext)

tues_data <- tidytuesdayR::tt_load(2021, week = 9)

earn <- tues_data$earn

font_add_google("Teko", "h")
font_add_google("PT Sans Narrow", "t")

df <- 
  earn %>% 
  filter(age == "16 years and over", ethnic_origin == "All Origins", sex != "Both Sexes") %>% 
  mutate(year_qrt = yearquarter(paste(year, " Q",quarter, sep = "")))  
  
df_ribbon <-
  df %>% 
  select(-n_persons) %>% 
  pivot_wider(names_from = sex, values_from = median_weekly_earn) %>% 
  rowwise() %>% 
  mutate(ymin = min(Men, Women),
         ymax = max(Men, Women))

showtext_auto()

ggplot(filter(df, race == "All Races")) +
  geom_ribbon(data = filter(df_ribbon, race == "All Races"), aes(x = year_qrt, ymin = ymin, ymax = ymax), fill = "grey80", alpha = 0.5) +
  geom_xspline(aes(x = year_qrt, y = median_weekly_earn, group = sex, color = sex), size = 1.05, show.legend = FALSE) +
  scale_color_manual(values = c("#2a9d8f", "#e9c46a")) +
  scale_x_yearquarter(name = "") +
  scale_y_continuous(name = "Medain Weekly Earnings", breaks = seq(700, 1100, 100), labels = paste0(c(" ", " ", " ", " ", "$"), c("700", "800", "900", "1,000", "1,100"))) +
  theme_minimal() +
  theme(
    plot.margin = margin(15, 15, 15, 15),
    axis.title.y.left = element_text(hjust = 0.95, size = 12, margin = margin(r = 10), family = "t"),
    axis.text = element_text(family = "t", size = 15)
  ) 
  ggsave(here(2021, "plots", "week_09.pdf"), height = 10, width = 14, device = cairo_pdf)
  
ggplot(filter(df, race != "All Races")) +
  geom_ribbon(data = filter(df_ribbon, race != "All Races"), aes(x = year_qrt, ymin = ymin, ymax = ymax), fill = "grey80", alpha = 0.5) +
  geom_xspline(aes(x = year_qrt, y = median_weekly_earn, group = sex, color = sex)) +
  facet_wrap(~race)

