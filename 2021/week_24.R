library(tidyverse)
library(ggfx)
library(ggtext)
library(packcircles)
library(systemfonts)
library(patchwork)

fishing <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv')

font <- system_fonts() %>% filter(family == "Font Awesome 5 Free")
register_font(
  name = "Font Awesome",
  plain = font$path[1]
)

fishing %>% 
  count(lake, sort = T)

fishing %>% 
  filter(lake == "Michigan") %>% 
  count(species, sort = T)

fishing %>% 
  count(species, sort = T)

fishing %>% 
  select(lake, species, year, values) %>% 
  mutate(
    species = fct_lump(species, n = 2)
  ) %>%
  filter(lake == "Michigan", species != "Other") %>% 
  group_by(lake, species, year) %>% 
  summarise(values = sum(values, na.rm = TRUE)) %>% 
  ggplot(aes(year, values)) +
  geom_line() +
  facet_wrap(~species)

king_salmon <- 
  fishing %>% 
  filter(species == "Chinook Salmon", region == "MI State Total") %>% 
  count(year, wt = values) %>% 
  mutate(
    fill = if_else(n <= lead(n), "#457b9d", "#a8dadc"),
    year_next = lead(year),
    nmax = max(n)
  )

p_main <- ggplot(king_salmon, aes(year, n)) +
  as_reference(
    geom_ribbon(aes(xmin = year, xmax =year, ymin = 0, ymax = n)),
    id = "bg"
  ) +
  with_blend(
    geom_rect(aes(xmin = year, xmax = year_next, ymin = 0, 
                  ymax = nmax,fill = fill), alpha = 0.8),
    bg_layer = "bg",
    blend_type = "in"
  ) +
  geom_line(color = "#1d3557") +
  geom_richtext(aes(x = 2005, y = 47, 
                    label = "The <span style='color: #457b9d'>RISE</span> and <span style='color: #a8dadc'>FALL</span> of King Salmon Prodcution<br>in the Great Lakes"),
                family = "Limelight", size = 7, label.color = NA, fill = NA, lineheight = 1.2) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_fill_identity() +
  labs(x = "", y = "Production Amounts (in '000 pounds)",
       caption = "**Data:** Great Lakes Database | **Plot:** Kaustav Sen") +
  coord_cartesian(expand = c(0,0), clip = "off") +
  theme_minimal(base_family = "Inter", base_size = 16) +
  theme(
    plot.margin = margin(10, 25, 10, 15),
    plot.caption.position = "plot",
    plot.caption = element_markdown(size = 14, hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.line = element_line(size = 0.5),
    axis.title.y.left = element_text(size = 11, face = "bold", 
                                     margin = margin(r = 10), color = "grey40")
  )

set.seed(100)

p_inset <- 
  king_salmon %>% 
  filter(year %in% c(1992, 1995, 1998, 2012)) %>% 
  rowwise() %>% 
  mutate(
    fish_total = list(circleProgressiveLayout(rep(1, nmax))),
    fish_present = list(
      c(rep("#1d3557", n), rep("grey85", nmax - n)) %>% sample()
    )
  ) %>% 
  unnest(cols = c("fish_total", "fish_present")) %>% 
  ggplot(aes(x, y)) +
  geom_text(aes(label = "\uf578", color = fish_present), size = 4, family = "Font Awesome") +
  scale_color_identity() +
  facet_wrap(~year) +
  theme_void() +
  theme(
    plot.margin = margin(15, 15, 15, 15),
    strip.text = element_text(family = "Limelight", size = 12, margin = margin(b = 10)),
    panel.spacing = unit(1, "lines")
  )

p_final <- p_main + inset_element(p_inset, 0.4, 0.1, 0.9, 0.75)

ragg::agg_png(here::here("2021", "plots", "week_24.png"), width = 12, height = 7, res = 150, units = "in")
print(p_final)
dev.off()
