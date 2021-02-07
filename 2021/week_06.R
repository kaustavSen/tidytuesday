library(tidyverse)
library(janitor)
library(gggibbous)
library(scales)
library(showtext)
library(ggtext)

tues_data <- tidytuesdayR::tt_load(2021, week = 6)

font_add_google("IBM Plex Mono")
font_add_google("IBM Plex Sans Condensed")

hbcu_all <- 
  tues_data$hbcu_all %>% 
  clean_names() %>% 
  select(year, total_enrollment, males, females)

showtext_auto()

hbcu_all %>% 
  mutate(prop_females = females / total_enrollment) %>% 
  ggplot(aes(year, total_enrollment)) +
  geom_line(color = "#003049", size = 1.05) +
  geom_moon(aes(ratio = prop_females), right = TRUE, size = 7, fill = "#d62828", color = NA) +
  geom_moon(aes(ratio = 1 - prop_females), right = FALSE, size = 7, fill = "#f77f00", color = NA) +
  annotate("text", x = 1980 + 0.1, y = 325000, label = "HBCU Enrollment", hjust = 0, size = 10, family = "IBM Plex Sans Condensed", fontface = "bold", color = "#fcbf49") +
  annotate("text", x = 1980, y = 325000, label = "HBCU Enrollment", hjust = 0, size = 10, family = "IBM Plex Sans Condensed", fontface = "bold", color = "#003049") +
  annotate("richtext", x = 1986, y = 315000, label = "<span style = 'color: #f77f00'>**Males**</span> and <span style = 'color: #d62828'>**Females**</span>", size = 5, family = "IBM Plex Sans Condensed", fill = NA, label.color = NA) +
  scale_y_continuous(name = "", limits = c(200000, 350000), labels = number_format(scale = 1/1000, suffix = "k")) +
  scale_x_continuous(name = "", breaks = seq(1980, 2015, 5)) +
  labs(caption = "Data: Data.World | Plot: Kaustav Sen") +
  theme_minimal() +
  theme(
    text = element_text(family = "IBM Plex Mono"),
    plot.margin = margin(10, 10, 10, 10),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#eae2b7", color = "#eae2b7"),
    plot.caption = element_text(family = "IBM Plex Mono", size = 9, color = "grey70")
  ) +
  ggsave(here::here(2021, "plots", "week_06.pdf"), width = 10, height = 7, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = here::here(2021, "plots", "week_06.pdf"),
  filenames = here::here(2021, "plots", "week_06.png"),
  dpi = 100
)
