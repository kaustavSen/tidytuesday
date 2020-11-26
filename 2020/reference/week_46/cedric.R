library(tidyverse)
library(rnaturalearth)
library(ggtext)
library(ggsci)
library(showtext)

df_mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')

df_countries <- 
  ne_countries(scale = 110, returnclass = "sf") %>% 
  as_tibble()

df_mobile_sub <-
  df_mobile %>% 
  filter(year < max(year)) %>% 
  left_join(df_countries, by = c("code" = "iso_a3")) %>% 
  group_by(entity, year) %>% 
  slice(1) %>%
  ungroup %>% 
  add_count(entity) %>% 
  filter(n == max(n), !is.na(subregion)) %>% 
  dplyr::select(entity, year, mobile_subs, continent.x, continent.y, subregion, n) %>% 
  group_by(continent.x, subregion, year) %>% 
  summarize(mobile_subs = mean(mobile_subs, na.rm = TRUE)) %>% 
  arrange(year, continent.x, subregion) %>% 
  group_by(continent.x, year) %>% 
  mutate(
    id = row_number(), 
    alpha = id / max(id),
    subregion = str_replace(subregion, "\\band\\b", "&")
  ) %>% 
  ungroup

df_mobile_end <-
  df_mobile_sub %>% 
  filter(year == max(year)) %>% 
  group_by(continent.x) %>% 
  mutate(end_cont = mean(mobile_subs[which(year == 2016)])) %>% 
  group_by(subregion) %>% 
  mutate(end_sub = mobile_subs[which(year == 2016)]) %>% 
  arrange(-end_cont, -end_sub) %>% 
  ungroup %>% 
  mutate(id_sort = row_number()) %>% 
  dplyr::select(subregion, id_sort, end_cont, end_sub)

df_mobile_fct <-
  df_mobile_sub %>% 
  left_join(df_mobile_end, by = c("subregion")) %>% 
  mutate(
    continent.x = fct_reorder(continent.x, -end_cont),
    subregion = fct_reorder(subregion, id_sort),
    continent_rev = fct_rev(continent.x)
  )

theme_set(theme_void(base_family = "Roboto Condensed", base_size = 9))

theme_update(
  axis.text.x = element_text(color = "grey60", margin = margin(t = 4)),
  axis.ticks.x = element_line(color = "grey60"),
  axis.ticks.length.x = unit(.4, "lines"),
  legend.position = "none",
  panel.grid = element_blank(),
  plot.margin = margin(35, 25, 15, 35),
  plot.background = element_rect(fill = "grey98", color = "grey98"),
  plot.caption = element_text(family = "Roboto", color = "grey60", 
                              size = 8, margin = margin(t = 30, r = 50))
) 

font_add_google("Roboto Condensed")
font_add_google("Roboto")
font_add_google("Arvo")

showtext_auto()

plot <- ggplot(
  df_mobile_fct,
  aes(
    x = year,
    y = mobile_subs,
    group = subregion,
    alpha = -alpha
  )
) +
  geom_area(
    aes(fill = continent.x),
    position = "stack",
    size = 0
  ) +
  geom_area(
    data = df_mobile_fct %>% group_by(continent.x, year) %>%
      summarise(mobile_subs = sum(mobile_subs)),
    aes(
      year, mobile_subs,
      group = continent.x,
      color = continent.x,
      color = after_scale(colorspace::darken(color, 0.2, space = "HLS"))
    ),
    inherit.aes = FALSE,
    position = "stack",
    fill = "transparent",
    size = 0.9
  ) +
  annotate(
    geom = "rect",
    xmin = 2016, xmax = Inf,
    ymin = -Inf, ymax = Inf,
    fill = "grey98",
    size = 0
  ) +
  geom_richtext(
    data = df_mobile_fct %>% filter(year == 2014) %>% 
      group_by(continent_rev, year) %>% 
      summarise(mobile_subs = sum(mobile_subs)),
    aes(
     year, 
     mobile_subs,
     label = continent_rev,
     vjust = mobile_subs / 130
    ),
    inherit.aes = FALSE,
    position = "stack",
    size = 6.5,
    family = "Arvo",
    fontface = "bold",
    color = "white",
    label.color = NA,
    fill = NA
  ) +
  annotate(
    "text",
    x = 2014,
    y = 1840,
    label = "Regions of: ",
    color = "grey85",
    size = 3.5,
    family = "Arvo"
  ) +
  geom_richtext(
    data = df_mobile_fct %>% filter(year == 2016),
    aes(year, mobile_subs, group = subregion,
        label = glue::glue("<b style='font-size:9pt;'>{subregion}</b><br>{round(mobile_subs, 1)} per 100 persons"),
        color = continent.x,
        hjust = 0,
        vjust = 0.9),
    inherit.aes = FALSE,
    position = "stack",
    family = "Roboto Condensed",
    size = 2.3,
    label.color = NA,
    fill = NA
  ) +
  geom_richtext(
    data = tibble(year = 1998.7, mobile_subs = 1650),
    aes(
      year,
      mobile_subs,
      label = glue::glue("<b style='font-size:20pt;font-family:Arvo;'>Nowadays, in many regions of the world<br>there are more mobile subscriptions than people</b><br><br><br><span style='font-size:9pt'>In the last decades, the number of mobile devices has grown rapidly across the world.<br>In 2016, in 13 out of 20 regions of the world the number of subscriptions exceeded more than one<br>mobile per person, but different countries and regions have varying rates of adoption.")
    ),
    inherit.aes = FALSE,
    position = "stack",
    family = "Roboto",
    size = 2.8,
    color = "grey40",
    label.color = NA,
    fill = NA,
    lineheight = 1.6
  ) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(1990, 2019.5),
    breaks = 1990:2016
  ) +
  scale_y_continuous(
    expand = c(0.007, 0.007)
  ) +
  scale_color_uchicago() +
  scale_fill_uchicago() +
  scale_alpha(range = c(0.5, 1)) +
  labs(caption = "Visualization by Cédric Scherer  •  Data by OurWorldInData.org")

path <- here::here(2020, "reference", "week_46", "cedric")
ggsave(paste0(path, ".pdf"), plot, width = 12, height = 9.5, device = cairo_pdf)