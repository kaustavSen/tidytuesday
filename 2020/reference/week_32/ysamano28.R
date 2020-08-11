library(tidyverse)
library(patchwork)

tuesdata <- tidytuesdayR::tt_load(2020, week = 32)

energy_types <- tuesdata$energy_types

my_theme <- theme_ybn_w(base_size = 8,
                        title_hjust = 0.5,
                        subtitle_hjust = 0.5,
                        subtitle_margin_b = 20,
                        plot_margin = margin(10, 10, 10, 10),
                        axis_grid = F,
                        axis_text = F,
                        axis_title = F) +
  theme(legend.position = "none",
        strip.text = element_blank())


crear_coord <- function(conventional, nuclear, renewable) {
  
  punto_1 = conventional
  punto_2 = nuclear/(nuclear + renewable)
  
  tibble(grupo = rep(c("Conventional \nthermal", "Nuclear", "Renewable"), each = 4),
         num_point = rep(1:4, 3),
         x = c(0, punto_1, punto_1, 0,
               punto_1, 1, 1, punto_1,
               punto_1, 1, 1, punto_1),
         y = c(0, 0, 1, 1,
               0, 0, punto_2, punto_2,
               punto_2, punto_2, 1, 1)
  )
}

total_type <- energy_types %>% 
  filter(level != "Level 2") %>% 
  mutate(country_name = case_when(country == "UK" ~ "United Kingdom",
                                  TRUE ~ country_name),
         type_2 = case_when(type == "Conventional thermal" ~ "conventional_thermal",
                            type == "Nuclear" ~ "nuclear",
                            TRUE ~ "renewable")) %>% 
  group_by(country, country_name, type_2) %>% 
  summarise(total_2018 = sum(`2018`)) %>% 
  mutate(percent_2018 = total_2018/sum(total_2018)) %>% 
  select(-total_2018) %>% 
  pivot_wider(names_from = type_2, values_from = percent_2018)

data_graph <- 
  total_type %>% 
  mutate(table = list(crear_coord(conventional_thermal, nuclear, renewable))) %>% 
  unnest(table) %>% 
  mutate(label_country = str_wrap(country_name, width = 10),
         label_x = case_when(country %in% c("BA", "MK", "UK") ~ -0.93,
                             TRUE ~ -0.8),
         label_y = case_when(country %in% c("BA", "MK", "UK") ~ 0.98,
                             TRUE ~ 0.85),
         x_rot = x*cos(pi/4) - y*sin(pi/4),
         y_rot = x*sin(pi/4) + y*cos(pi/4))


#LEGEND

legend <- 
  tibble::tribble(
  ~label,    ~x,  ~y,
  "Conventional \nthermal",     2, 0.7,
  "Renewable",     4, 0.7,
  "Nuclear",     6, 0.7
)



p1 <- 
  ggplot(data_graph %>% 
           filter(country_name == "Spain")) +
  geom_polygon(aes(x_rot,
                   y_rot,
                   group = grupo,
                   fill = grupo),
               color = "#FDFDFB") +
  geom_point(data = legend,
             aes(x,
                 y,
                 fill = label),
             color = "#FDFDFB",
             size = 8,
             shape = 23) +
  geom_text(data = legend,
            aes(x + 0.35,
                y,
                label = label),
            size = 2.5,
            # family = "Roboto Condensed Light",
            hjust = 0) +
  annotate("segment",
           x = 0.1,
           xend = 0.7471,
           y = -0.05,
           yend = 0.6071,
           colour = "#110F1A",
           size = 0.2) +
  annotate("segment",
           x = 0.7571,
           xend = 0.1,
           y = 0.7871,
           yend = 1.4442,
           colour = "#110F1A",
           size = 0.2) +
  annotate("text",
           x = 0.7,
           y = 0.25,
           label = "100%",
           size = 2) +
  annotate("text",
           x = 0.7,
           y = 1.2,
           label = "100%",
           size = 2) +
  labs(title = "How European Countries \nGenerated Electricity in 2018",
       subtitle = "Share of Total by Production Type") +
  scale_fill_manual(values = c("#BF8E88", "#7A8990", "#42454D")) +
  scale_x_continuous(limits = c(-1.5, 8)) +
  scale_y_continuous(limits = c(-0.2, 1.5)) +
  coord_fixed() +
  my_theme


#BODY

p2 <- 
  ggplot(data_graph) +
  geom_polygon(aes(x_rot,
                   y_rot,
                   group = grupo,
                   fill = grupo),
               color = "#FDFDFB") +
  geom_text(aes(x = label_x,
                y = label_y,
                label = factor(label_country)),
            size = 2.5,
            # family = "Roboto Condensed Light",
            color = "#110F1A",
            angle = 45,
            hjust = 0) +
  facet_wrap( ~ country_name, ncol = 6) +
  labs(caption = "Data: Eurostat | Design: Yobanny Samano (@ysamano28)") +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_continuous(limits = c(0, 1.8)) +
  scale_fill_manual(values = c("#BF8E88", "#7A8990", "#42454D")) +
  coord_fixed() +
  my_theme


patch <- p1 / p2 + plot_layout(heights = c(0.2, 1.2))


ggsave(filename = "2020/week_32/European_Energy_production.png", 
       plot = patch, 
       height = 230, 
       width = 150, 
       units = "mm",
       type = "cairo")
