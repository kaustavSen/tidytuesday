library(tidyverse)
library(janitor)
library(lubridate)
library(ggimage)
library(futurevisions)
library(colorspace)
library(sf)
library(cowplot)

animal_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_complaints.csv') %>% 
  clean_names()

# Read in population data -------------------------------------------------
population <- read_csv("https://raw.githubusercontent.com/gkaramanis/tidytuesday/master/2020-week30/data/population.csv") %>% 
  set_names(c("area", "population"))

noisy <- animal_complaints %>% 
  left_join(population, by = c("electoral_division" = "area")) %>%
  filter(complaint_type == "Noise" & electoral_division != "Unallocated") %>%
  select(-suburb) %>%
  mutate(
    div_n = parse_number(electoral_division),
    date_received = as.Date(parse_date_time(date_received, orders = c("m Y"))),
    year = year(date_received),
    month_n = month(date_received),
    month_i = str_sub(month(month_n, label = TRUE), 1, 1)
  ) %>%
  group_by(div_n, year, month_n) %>% 
  mutate(
    total = n(),
    pct = total/population
  ) %>% 
  ungroup() %>% 
  group_by(div_n, year) %>% 
  mutate(year_total = n()) %>% 
  ungroup() %>% 
  distinct(electoral_division, div_n, year, month_n, month_i, total, year_total, pct) %>%
  filter(year < 2020, year > 2013, !is.na(year)) %>% 
  mutate(
    label_year = "Total\nby year",
    label_year_x = -2,
    label_year_y = 2013,
    label_month = "Complaints per capita\nby month",
    label_month_x = 12,
    label_month_y = 2013
  ) %>% 
  add_row(div_n = 5.3, electoral_division = "") %>%
  add_row(div_n = 5.7, electoral_division = " ") %>%
  mutate(electoral_division = fct_reorder(electoral_division, div_n))

# Palette -----------------------------------------------------------------
pal <- futurevisions("grand_tour")

# Month labels ------------------------------------------------------------
months  <-  data.frame(x = 1:12, y = 2020) %>% 
  mutate(label = month(x, abbr = FALSE, label = TRUE))

# Midpoints for monthly per capita and yearly complaints, used in  --------
mid_pct  <-  (max(noisy$pct, na.rm = TRUE) - min(noisy$pct, na.rm = TRUE))/2 + min(noisy$pct, na.rm = TRUE)
mid_year  <-  (max(noisy$year_total, na.rm = TRUE) - min(noisy$year_total, na.rm = TRUE))/2 + min(noisy$year_total, na.rm = TRUE)

# Plot --------------------------------------------------------------------
tiles <- 
  ggplot(noisy) +
  geom_tile(aes(month_n, year, height = 0.5, width = 0.9 * pct/max(pct, na.rm = TRUE), fill = pct), colour = NA) +
  geom_text(aes(month_n, 2019.6, label = month_i), check_overlap = TRUE, family = "Century Gothic", size = 2) +
  geom_text(aes(-2, year, label = year_total, colour = year_total), 
            family = "Century Gothic", check_overlap = TRUE, size = 2) +
  geom_text(aes(label_year_x, label_year_y, label = label_year), family = "Century Gothic", size = 2, lineheight = 0.8, check_overlap = TRUE) +
  geom_text(aes(label_month_x, label_month_y, label = label_month), family = "Century Gothic", size = 2, lineheight = 0.8, check_overlap = TRUE, hjust = 1) +
  labs(
    title = "Noisy Dogs",
    subtitle = "Complaints about noise caused by dogs in Townsville, Australia, 2014-2019",
    caption = "Source: data.gov.au | Graphic: Georgios Karamanis"
  ) +
  scale_x_continuous(limits = c(-5, 12.5), breaks = 1:12) +
  scale_y_reverse(limits = c(2020, 2012), breaks = 2019:2014, expand = c(0, 0)) +
  # Fill scale for tiles ----------------------------------------------------
  scale_fill_steps2(low = pal[2], mid = pal[6], high = pal[4], midpoint = mid_pct) +
  # Colour scale for year totals --------------------------------------------
  scale_colour_steps2(low = pal[2], mid = pal[6], high = pal[4], midpoint = mid_year) +
  facet_wrap(vars(electoral_division), ncol = 4) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = NA, color = NA),
    plot.title = element_text(hjust = 0.5, family = "Cooper Black", size = 14, margin = margin(0, 0, 8, 0)),
    plot.subtitle = element_text(hjust = 0.5, family = "Calibri", margin = margin(0, 0, 10, 0)),
    plot.caption = element_text(hjust = 0.5, family = "Calibri", size = 6, margin = margin(10, 0, 0, 0)),
    strip.text = element_text(family = "Cooper Black", hjust = 0.6, margin = margin(10, 0, 0, 0)),
    axis.text.y = element_text(family = "Century Gothic", size = 5),
    plot.margin = margin(20, 20, 20, 20)
  )

# Source script for map ---------------------------------------------------
# Read in shapefile for suburbs and union to get boundaries of city
subs <- st_read("2020/reference/TCC_Suburbs/TCC_Suburbs.shp") %>% 
  st_union() %>% 
  st_set_crs(4326)

# Read in shapefile for electoral divisions (some go into the sea)
divs <- st_read("2020/reference/Townsville_Final_Divisions-ESRI/Townsville_City_Divisions.shp") %>% 
  st_as_sf() %>% 
  st_transform(4326)

# Get intersection of electoral divisions and city boundaries
divs_clipped <- st_intersection(divs, subs) %>% 
  clean_names() %>% 
  mutate(
    facet = case_when(
      division_id %in% c(1, 2, 5, 7, 8) ~ "left",
      TRUE ~ "right"
    ))

# Create centroids of clipped electoral divisions
divs_centroid <- st_centroid(divs_clipped) %>% 
  clean_names()

map <- 
  ggplot(divs_clipped) +
  geom_sf(fill = pal[6], colour = pal[7], size = 0.25, alpha = 0.6) +
  geom_sf_text(aes(label = division_id), size = 3, family = "Century Gothic", colour = "grey10") +
  annotate("text", 146.3, -19.4, label = "Townsville\nelectoral divisions", 
           hjust = 0, family = "Cooper Black", size = 4, lineheight = 0.9, alpha = 0.4) +
  theme_void() +
  theme(
    legend.position = "none",
    strip.text = element_blank(),
    plot.background = element_rect(fill = pal[7], colour = NA),
    plot.margin = margin(130, 120, 130, 120)
  )

ggdraw(map) +
  draw_plot(tiles) +
  ggsave("2020/reference/dog-noises.png", dpi = 320, width = 8, height = 8)
