library(tidyverse)
library(rnaturalearth)
library(tidygraph)
library(ggraph)
library(ggimage)
library(ggtext)
library(glue)
library(showtext)
library(here)

font_add(
  "Atkinson Hyperlegible",
  regular = here(2020, "fonts", "Atkinson-Hyperlegible-Regular-102.otf"),
  bold = here(2020, "fonts", "Atkinson-Hyperlegible-Bold-102.otf"),
)

women_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-08/women.csv')

countries <- 
  rnaturalearth::ne_countries(returnclass = "sf") %>% 
  as_tibble() %>%
  select(name, continent) %>% 
  mutate(
    name = case_when(
      name == "United States" ~ "US",
      name == "United Kingdom" ~ "UK",
      name == "United Arab Emirates" ~ "UAE",
      name == "Dem. Rep. Korea" ~ "South Korea",
      name == "Ireland" ~ "Republic of Ireland",
      name == "Dem. Rep. Congo" ~ "DR Congo",
      TRUE ~ name
    )
  ) %>% 
  add_row(
    name = c("Iraq/UK", "Hong Kong", "Exiled Uighur from Ghulja (in Chinese, Yining)", "Northern Ireland", "Wales, UK", "Singapore"),
    continent = c("Asia", "Asia", "Asia", "Europe", "Europe", "Asia")
  )

women <- women_raw %>% 
  left_join(countries, by = c("country"=  "name")) %>% 
  filter(name != "Unsung hero") %>% 
  mutate(
    continent = case_when(
      continent == "North America" ~ "North\nAmerica",
      continent == "South America" ~ "South\nAmerica",
      TRUE ~ continent
    ),
    category_continent = as.character(glue("{category}_{continent}"))
  )

nodes <- 
  tibble(
  node = c("root", unique(women$category), unique(women$category_continent))
  ) %>% 
  mutate(
    levels = case_when(
      node == "root" ~ 1,
      node %in% unique(women$category) ~ 2,
      node %in% unique(women$category_continent) ~ 3,
      TRUE ~ 4
    )
  ) %>% 
  left_join(
    count(women, category, continent, name = "number") %>% 
      mutate(category_continent = as.character(glue("{category}_{continent}"))),
    by = c("node" = "category_continent")
  ) %>% 
  mutate(
    continent = factor(continent, levels = c("Africa", "Asia", "Europe", "North\nAmerica", "South\nAmerica", "Oceania")),
    continent = fct_rev(continent)
  ) %>% 
  arrange(levels, category, continent)

edges_level_1 <- women %>% 
  distinct(category) %>% 
  mutate(from = "root") %>% 
  rename(to = category)

edges_level_2 <- 
  women %>%
  distinct(category, category_continent) %>% 
  arrange(category, category_continent) %>%
  select(from = category, to = category_continent)

color_edges <- tibble(
  category = c("Identity", "Knowledge", "Creativity" , "Leadership"),
  color = c("#99B898", "#019875", "#FF847C", "#C0392B")
)

edges <- 
  bind_rows(edges_level_1, edges_level_2) %>% 
  left_join(color_edges, by = c("to" = "category")) %>% 
  left_join(color_edges, by = c("from" = "category")) %>% 
  mutate(color = coalesce(color.x, color.y)) %>% 
  select(-color.x, -color.y)

graph_data <- tbl_graph(nodes, edges)

img <- here(2020, "img", "week_50", "Unsung hero.png")

showtext_auto()

ggraph(graph_data, layout = "partition") +
  geom_edge_diagonal(aes(color = color), alpha = 0.5) +
  geom_node_text(aes(x = x, y = y, label = continent, filter = levels == 3, color = category), size = 10, family = "Atkinson Hyperlegible", hjust = 1, vjust = 0.5, lineheight = 0.9) +
  geom_node_text(aes(label = node, filter = levels == 2, color = node), size = 13, family = "Atkinson Hyperlegible", vjust = 0.5, fontface = "bold") +
  geom_node_point(aes(filter = levels == 2, color = node), size = 120, alpha = 0.40) +
  geom_node_point(aes(filter = levels == 2, color = node), size = 130, shape = 1) +
  geom_node_range(aes(y = y + 0.02, yend = y + 1.5 * number/max(nodes$number, na.rm = TRUE), x = x, xend = x, filter = levels == 3, color = category), size = 12) +
  geom_node_text(aes(x = x, y = y + 1.5 * number/max(nodes$number, na.rm = TRUE), label = number, filter = levels == 3, color = category), nudge_y = 0.025, size = 10, family = "Atkinson Hyperlegible", fontface = "bold", hjust = 0, vjust = 0.5) +
  geom_image(data = filter(nodes, levels == 1), aes(x = 10, y = 1.32, image = img), size = 0.09, asp = 1.8) + 
  scale_color_manual(values = c("Identity" = "#99B898", "Knowledge" = "#019875", "Creativity" = "#FF847C", "Leadership" = "#C0392B")) +
  scale_edge_color_identity() +
  labs(
    title = "<b>BBC's 100 women of 2020</b>",
    subtitle = "The chart below shows the number of women from each continent across the four categories",
    caption = "**Data:** BBC | **Plot:** Kaustav Sen"
  ) +
  coord_flip() +
  theme(
    plot.margin = margin(30, 30, 30, 30),
    plot.title = element_markdown(family = "Atkinson Hyperlegible", size = 100, color = "#2A363B", hjust = 0.5),
    plot.subtitle = element_text(family = "Atkinson Hyperlegible", size = 40, margin = margin(t = 20), hjust = 0.5),
    plot.caption = element_markdown(family = "Atkinson Hyperlegible", size = 25, color = "grey40"),
    panel.background = element_rect(fill = "white", color = "white"),
    legend.position = "none"
  ) +
  ggsave(here(2020, "plots", "week_50.pdf"), height = 22, width = 22*1.8, device = cairo_pdf)

pdftools::pdf_convert(
  pdf = here(2020, "plots", "week_50.pdf"),
  filenames = here(2020, "plots", "week_50.png"),
  dpi = 72
)
