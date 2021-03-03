library(tidyverse)
library(ggtext)
library(patchwork)
library(tsibble)
library(here)
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

plot_main <- 
  ggplot(filter(df, race == "All Races")) +
  geom_ribbon(data = filter(df_ribbon, race == "All Races"), aes(x = year_qrt, ymin = ymin, ymax = ymax), fill = "grey80", alpha = 0.5) +
  geom_line(aes(x = year_qrt, y = median_weekly_earn, group = sex, color = sex), size = 1.05, show.legend = FALSE) +
  ggrepel::geom_label_repel(data = filter(df, race == "All Races", year == 2020, quarter == 4),
                            mapping = aes(x = year_qrt, y = median_weekly_earn, label = sex, color = sex), label.size = NA, fill = NA, family = "t", size = 6, fontface = "bold", show.legend = FALSE) +
  annotate("label", x = yearquarter("2010Q1"), y = 1050, label = "The Great Wage Divide", label.size = 0, size = 21, hjust = 0, family = "h") +
  geom_textbox(aes(x = yearquarter("2014Q2"), y = 810, label = "Weekly wages of male workers have consistently been about <span style='color: #e76f51; face: bold'>20% higher</span> over the past 10 years relative to<br/>their female counterparts."), hjust = 0, family = "t", size = 7, color = "#264653", box.size = NA, fill = NA, width = unit(10, "cm")) +
  scale_color_manual(values = c("#2a9d8f", "#e9c46a")) +
  scale_x_yearquarter(name = "") +
  scale_y_continuous(name = "Medain Weekly Earnings", breaks = seq(700, 1100, 100), labels = paste0(c(" ", " ", " ", " ", "$"), c("700", "800", "900", "1,000", "1,100"))) +
  theme_minimal() +
  theme(
    plot.margin = margin(15, 15, 5, 15),
    axis.title.y.left = element_text(hjust = 0.95, size = 15, margin = margin(r = 10), family = "t"),
    axis.text = element_text(family = "t", size = 18)
  )

plot_bottom_row <- 
  ggplot(filter(df, race != "All Races")) +
  geom_ribbon(data = filter(df_ribbon, race != "All Races"), aes(x = year_qrt, ymin = ymin, ymax = ymax), fill = "grey80", alpha = 0.5) +
  geom_line(aes(x = year_qrt, y = median_weekly_earn, group = sex, color = sex), size = 1.05, show.legend = FALSE) +
  labs(caption = "Plot: Kaustav Sen | Data: BLS") +
  facet_wrap(~race, scales = "free") +
  scale_color_manual(values = c("#2a9d8f", "#e9c46a")) +
  scale_x_yearquarter(name = "") +
  scale_y_continuous(name = "") +
  theme_minimal() +
  theme(
    plot.margin = margin(0, 15, 0, 15),
    plot.caption = element_text(family = "t", size = 15, hjust = 0.5),
    axis.text = element_text(family = "t", size = 15),
    strip.text = element_text(family = "h", size = 18),
    panel.spacing.x = unit(1.2, "cm")
  ) 

plot_main / plot_bottom_row +
  plot_layout(heights = c(4, 1)) +
  ggsave(here(2021, "plots", "week_09.pdf"), height = 15, width = 12, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = here(2021, "plots", "week_09.pdf"),
  filenames = here(2021, "plots", "week_09.png"),
  dpi = 150
)
