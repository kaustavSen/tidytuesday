library(tidyverse)
library(ggsci)
library(scales)
library(patchwork)
library(here)
library(showtext)

tues_data <- tidytuesdayR::tt_load(2021, week = 10)

youtube <- tues_data$youtube

font_add_google("Staatliches", "header")
font_add_google("Roboto Condensed", "text")
font_add_google("IBM Plex Mono", "number")

df_longer <- 
  youtube %>% 
  pivot_longer(cols = funny:use_sex, names_to = "trait", values_to = "present", values_ptypes = list("present" = integer())) %>% 
  select(year, brand, trait, present)
  
showtext_auto()

heatmap <- 
  df_longer %>% 
  group_by(year, trait) %>% 
  summarise(total = sum(present)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(prop = total / sum(total)) %>% 
  ggplot(aes(year)) +
  geom_tile(aes(y = trait, fill = prop), color = "white", size = 0.5) +
  scale_y_discrete(labels = c("Animals", "Celebrity", "Danger", "Funny", "Patriotic", "Show Product Quickly", "Use Sex")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_material("cyan", guide = guide_colorsteps(position = "bottom"), labels = percent_format(accuracy = 1)) +
  labs(caption = "Data: FiveThirtyEight | Plot: Kaustav Sen") +
  theme_minimal() +
  theme(
    plot.margin = margin(0, 20, 20, 20),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(family = "number", size = 15),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width = unit(2, "cm"),
    axis.title = element_blank(),
    axis.text.x = element_text(family = "number", size = 15),
    axis.text.y = element_text(family = "text", size = 15),
    plot.caption = element_text(family = "text", size = 13, hjust = 0.5)
  )

ggsci::pal_material("cyan")(10) %>% show_col()

bar_plot <-
  df_longer %>% 
  group_by(year) %>% 
  summarise(total = sum(present)) %>% 
  ggplot(aes(year, total)) +
  geom_col(fill = "#B2EBF2FF") +
  geom_rect(aes(xmin = year - 0.45, xmax = year + 0.45, ymin = total - 5, ymax = total), fill = "#00838EFF") +
  geom_text(aes(y = total - 2.5, label = total), color = "#DFF7F9FF", family = "number", size = 6.5, fontface = "bold") +
  scale_x_continuous(expand = c(0, 0)) +
  labs(
    title = "Evolution of Superbowl Commercials",
    subtitle = "The bars represent the number of ads per year between 2000-2020. The heatmap shows the distribution of themes prevalent in the commercials."
  ) +
  theme_void() +
  theme(
    plot.title = element_text(family = "header", size = 40, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(family = "text", size = 20, hjust = 0.5, margin = margin(b = 15)),
    plot.margin = margin(t = 20)
  )

bar_plot / heatmap + plot_layout(heights = c(0.6, 0.4)) +
  ggsave(here(2021, "plots", "week_10.png"), width = 15, height = 8, dpi = 150)
