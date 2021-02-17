# Load packages -----------------------------------------------------------

library(tidyverse)
library(colorspace)
library(showtext)
library(magick)
library(here)


# Load Data ---------------------------------------------------------------

tues_data <- tidytuesdayR::tt_load(2021, week = 8)


# Load fonts --------------------------------------------------------------

font_add_google("Public Sans")

# Data Prep ---------------------------------------------------------------

freed_slaves <- 
  tues_data$freed_slaves %>% 
  pivot_longer(cols = Slave:Free, names_to = "slave_free", values_to = "proportion") %>% 
  # Slave + Free should add up to 100 for all years
  rows_update(tibble(Year = 1800, slave_free = "Slave", proportion = 89), by = c("Year", "slave_free"))

free_prop_text <-
  freed_slaves %>% 
  filter(slave_free == "Free") %>% 
  mutate(
    y_text_position = 100 - if_else(Year == 1870, 11, proportion) + 1.85,
    y_line_position = y_text_position + 1,
    label = glue::glue("{proportion}%")
  )

colors <- c(
  darken("#00aa00", 0.35), # Green for "free" 
  lighten("#000000", 0.1) # Black for slaves
)

# Plot --------------------------------------------------------------------

showtext_auto()

freed_slaves %>% 
  ggplot(aes(Year, proportion, fill = slave_free)) +
  geom_area(show.legend = FALSE) +
  geom_text(
    data = free_prop_text,
    aes(x = Year, y = y_text_position, label = label),
    family = "Public Sans",
    fontface = "bold",
    size = 12
  ) +
  geom_segment(
    # We do not want the line segments at the end years and so filtering these out
    data = filter(free_prop_text, ! Year %in% c(1790, 1870)),
    aes(x = Year, xend = Year, y = 100, yend = y_line_position),
    size = 0.3
  ) +
  geom_label(aes(x = 1830, y = 95, label = "FREE — LIBRE"), family = "Public Sans", fontface = "bold", size = 16, label.size = NA, show.legend = FALSE) +
  geom_text(aes(x = 1830, y = 55, label = "SLAVES\nESCLAVES"), family = "Public Sans", fontface = "bold", size = 35, color = "white", lineheight = 0.25) +
  scale_x_continuous(expand = c(0,0), breaks = seq(1790, 1870, 10), position = "top", name = NULL) +
  scale_y_continuous(expand = c(0, 0), labels = NULL, name = NULL) +
  scale_fill_manual(values = colors) +
  labs(
    title = "PROPORTION OF FREEMEN AND SLAVES AMONG AMERICAN NEGROES .\nPROPORTION DES NÈGRES LIBRES ET DES ESCLAVES EN AMÉRIQUE .",
    subtitle = "DONE BY ATLANTA UNIVERSITY .",
    caption = "Plot: Kaustav Sen | Source: #DuBoisChallenge"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(20, 20, 20, 20),
    plot.title.position = "plot",
    plot.title = element_text(family = "Public Sans", face = "bold", size = 39, hjust = 0.5, lineheight = 0.5),
    plot.subtitle = element_text(family = "Public Sans", face = "bold", size = 30, hjust = 0.5, margin = margin(t = 10, b = 30)),
    plot.caption = element_text(family = "Public Sans", size = 30, hjust = 0.5, margin = margin(t = 10)),
    plot.background = element_rect(fill = "#fcf5eb", color = "#fcf5eb"),
    axis.ticks = element_blank(),
    axis.text = element_text(family = "Public Sans", face = "bold", size = 35)
  ) +
  ggsave(here(2021, "plots", "week_08.png"), height = 9, width = 9 * 0.78)

# Some magick touch-ups - just for fun! 

here(2021, "plots", "week_08.png") %>% 
  image_read() %>% 
  image_blur(10, 2) %>% 
  image_oilpaint() %>% 
  image_noise(noisetype = "gaussian") %>% 
  image_write(here(2021, "plots", "week_08_final.png"))