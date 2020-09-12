library(tidyverse)
library(janitor)
library(colorspace)
library(showtext)

key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')

# Top 3 crops by continent 2018
continents_2018 <- 
  key_crop_yields %>% 
  clean_names() %>% 
  filter(is.na(code)) %>% 
  filter(entity %in% c("Africa", "Americas", "Asia", "Europe",
                       "Oceania")) %>% 
  filter(year == 2018) %>% 
  rename_with(~ str_remove(.x, "_tonnes_per_hectare")) %>% 
  pivot_longer(cols = wheat:bananas, names_to = "crop") %>% 
  group_by(entity) %>% 
  mutate(
    total = sum(value, na.rm = TRUE),
    pct = value / total,
    ent_n = cur_group_id()
  ) %>% 
  slice_max(pct, n = 3) %>% 
  mutate(crop_n = row_number()) %>% 
  ungroup() 

# Cubes for totals
totals <- 
  continents_2018 %>% 
  distinct(entity, total) %>% 
  rowwise() %>% 
  mutate(
    # squares for totals
    x = list(c(0, 0, total, total)),
    y = list(c(0, total, total, 0)),
    # transform to a 2:1 "isometric projection
    # make a bottom square
    isox_bottom = list(x - y),
    isoy_bottom = list((x + y) / 2),
    # and then a top square
    isox_top = list(isox_bottom),
    isoy_top = list(isoy_bottom + total),
    # select parts of the square to make a 3d cube
    isox = list(c(isox_bottom[c(2, 1, 4)], isox_top[c(4, 3, 2)])),
    isoy = list(c(isoy_bottom[c(2, 1, 4)], isoy_top[c(4, 3, 2)]))
  ) %>% 
  ungroup() %>% 
  unnest(c(isox, isoy)) %>% 
  select(-isox_bottom, -isox_top, -isoy_bottom, -isoy_top) %>% 
  # "coordinates" for the continents
  mutate(
    x = case_when(
      entity == "Europe" ~ 50,
      entity == "Africa" ~ 0,
      entity == "Asia" ~ 185,
      entity == "Oceania" ~ 100,
      entity == "Americas" ~ -100,
    ),
    y = case_when(
      entity == "Europe" ~ -25,
      entity == "Africa" ~ -50,
      entity == "Asia" ~ -60,
      entity == "Oceania" ~ -125,
      entity == "Americas" ~ 0,
    )
  ) %>%
  mutate(entity = fct_relevel(entity, c("Americas", "Europe", "Africa", "Asia", "Oceania"))) %>% 
  group_by(entity) %>% 
  mutate(label_y = max(isoy)) %>% 
  ungroup()


cube_line_1 <- totals %>% 
  distinct(entity, x, y, total) %>%
  rowwise() %>% 
  mutate(isox = list(c(0, 0)),
         isoy = list(c(0, total))) %>% 
  unnest(c(isox, isoy)) %>% 
  filter(entity %in% c("Americas", "Europe", "Oceania")) %>%
  mutate(n = row_number()) %>% 
  rows_update(tibble(entity = "Europe", isoy = 72.5, n = 3), by = "n")

cube_line_2 <- totals %>% 
  distinct(entity, x, y, total) %>%
  rowwise() %>% 
  mutate(isox = list(c(0, -total)),
         isoy = list(c(total, 1.5*total))) %>% 
  unnest(c(isox, isoy)) %>% 
  filter(! entity %in% c("Asia"))

cube_line_3 <- totals %>% 
  distinct(entity, x, y, total) %>%
  rowwise() %>% 
  mutate(isox = list(c(0, total)),
         isoy = list(c(total, 1.5*total))) %>% 
  unnest(c(isox, isoy)) %>% 
  filter(! entity %in% c("Africa"))

pal <- c("#47607E", "#96A7B2", "#6788A5", "#D7664B", "#20527D")
font_add_google("Arvo", "A")

showtext_auto()

ggplot(totals) +
  aes(x = isox + x, y = isoy + y,
      group = entity, fill = entity,
      color = entity) +
  # Cubes
  geom_polygon(size = 1) +
  geom_path(data = cube_line_1, size = 1) +
  geom_path(data = cube_line_2, size = 1) +
  geom_path(data = cube_line_3, size = 1) +
  # Continent labels
  geom_text(aes(x = x - 20, y = y + label_y + 44,
                label = entity),
            color = "black",
            fontface = "bold",
            hjust = 1, stat = "unique",
            family = "A",
            size = 5) +
  geom_text(aes(x = x - 20, y = y + label_y + 30,
                label = str_c(round(total, 1), " tonnes/ha")),
            color = "black",
            hjust = 1, stat = "unique",
            family = "A",
            size = 4) +
  geom_segment(aes(x = x - 20, y = y + label_y + 21,
                   xend = x - 20, yend = y + label_y - 21),
               size = 0.15, color = "black") +
  geom_segment(aes(y = y + label_y + 21, yend = y + label_y + 21,
                   x = x - 20, xend = x - 30),
               size = 0.15, color = "black") +
  annotate("text", x = -100, y = -90, label = "2018", size = 10, family = "A") +
  annotate("text", x = -100, y = -110, label = toupper("Total key crop yield"), size = 5, family = "A") +
  scale_fill_manual(breaks = c("Americas", "Europe", "Africa", "Asia", "Oceania"), values = pal) +
  scale_color_manual(breaks = c("Americas", "Europe", "Africa", "Asia", "Oceania"), values = darken(pal)) +
  theme_void() +
  theme(legend.position = "none") +
  # NULL
  ggsave("2020/reference/week_36/cubes.pdf", width = 8, height = 7, device = cairo_pdf)
 
