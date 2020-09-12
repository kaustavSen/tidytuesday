library(tidyverse)
library(janitor)
library(colorspace)
library(ggforce)
library(showtext)
library(here)

# Data
key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')

# Top 3 crops by continent 2018
continents_2018 <- 
  key_crop_yields %>% 
  clean_names() %>% 
  filter(is.na(code)) %>%
  filter(entity == "Africa" | entity == "Americas" | entity == "Asia" | entity == "Europe" | entity == "Oceania") %>% 
  filter(year == 2018) %>% 
  rename_with(., ~ str_remove(.x, "_tonnes_per_hectare")) %>% 
  pivot_longer(cols = wheat:bananas, names_to = "crop") %>% 
  group_by(entity) %>% 
  mutate(
    total = sum(value, na.rm = TRUE),
    pct = value/total,
    ent_n = cur_group_id()
  ) %>% 
  slice_max(pct, n = 3) %>% 
  ungroup() %>% 
  group_by(entity) %>% 
  mutate(crop_n = row_number())

# fonts
font_add_google("Roboto", "f1")
font_add_google("Roboto", "f1b", bold.wt = 700)
font_add_google("Arvo", "A")

showtext_auto()

# palette
pal <- c("#47607E", "#96A7B2", "#6788A5", "#D7664B", "#20527D")

totals_alt <- 
  continents_2018 %>% 
  distinct(entity, total) %>% 
  # Area of hexagon -> A = 3 * sqrt(3) / 2 * r^2
  mutate(r_sq = total * 2/(3*sqrt(3)),
         r = sqrt(r_sq),
         entity = factor(entity, levels = c("Asia", "Americas", "Europe", "Oceania", "Africa"))) %>% 
  bind_cols(x0 = c(4, -0.5, 15, 8, 12.5),
            y0 = c(1.9, 10, 8, 7, 1.5)) 

plot <- 
  ggplot(totals_alt) +
  geom_regon(aes(x0 = x0, y0 = y0, r = r,
                 angle = 0, sides = 6,
                 color = entity, fill = entity), 
             size = 1.1) +
  geom_text(aes(x = x0 - 0.5, y = y0 + r + 1.5, label = entity),
            family = "f1b", color = "black",
            fontface = "bold",
            size = 4.5, hjust = 1) +
  geom_text(aes(x = x0 - 0.5, y = y0 + r + 0.8, label = paste0(round(total, 1), " tonnes/ha")),
            family = "f1b", color = "black",
            size = 3.5, hjust = 1) +
  geom_segment(aes(x = x0 - 1.5, y = y0 + r + 0.4,
                   xend = x0 - 0.5, yend = y0 + r + 0.4),
               size = 0.15) +
  geom_segment(aes(x = x0 - 0.5, y = y0 + r + 0.4,
                   xend = x0 - 0.5, yend = y0 + r * 0.6),
               size = 0.15) +
  annotate("text", x = -1.5, y = -3, 
           label = "2018", size = 18,
           family = "A", fontface = "bold") +
  annotate("text", x = -1.5, y = -5, 
           label = toupper("Total key crop yield"), 
           size = 6.5, family = "A") +
  scale_fill_manual(breaks = c("Americas", "Europe", "Africa", "Asia", "Oceania"), 
                    values = pal) +
  scale_color_manual(breaks = c("Americas", "Europe", "Africa", "Asia", "Oceania"), 
                     values = darken(pal)) +
  coord_equal() +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(20, 20, 20, 20),
        plot.background = element_rect(fill = "#EFE1C7",
                                       color = "#EFE1C7"))

# Using cowplot to make sure entire background is colored.
# When using coord_fixed, top and bottom margins seem to get missed.
cowplot::ggdraw(plot) +
  theme(plot.background = element_rect(fill = "#EFE1C7",
                                       color = "#EFE1C7")) +
  ggsave(here("2020", "reference", "week_36", "hex_alt.pdf"), height = 8, width = 8)

pdftools::pdf_convert(here("2020", "reference", "week_36", "hex_alt.pdf"),
                      filenames = here("2020", "reference", "week_36", "hex_alt.png"),
                      dpi = 100)
