# Credit: MaiaPelletier (https://github.com/MaiaPelletier/tidytuesday/blob/master/R/2020_Week32_EUEnergy.R)

library(tidytuesdayR)
library(dplyr)
library(tidyr)
library(ggplot2)
# library(mythemes)
library(here)

# theme_set(theme_maia())

extrafont::loadfonts("win")

tuesdata <- tt_load(2020, week = 32)

energy_types <- tuesdata$energy_types
country_totals <- tuesdata$country_totals

clean_energy2 <- 
  energy_types %>% 
  filter(level == "Level 1") %>% 
  select(country, country_name, type, amount = `2018`) %>% 
  mutate(country_name = case_when(
    country == "EL" ~ "Greece",
    country == "UK" ~ "United Kingdom",
    TRUE ~ country_name
  )) %>% 
  group_by(country) %>% 
  mutate(total = sum(amount)) %>% 
  filter(type != "Conventional thermal") %>% 
  mutate(
    clean_total = sum(amount),
    `%clean` = clean_total/total,
    `%type` = amount/total,
    over50perc = ifelse(`%clean` > 0.5, "Over 50% clean energy", "NA")
  ) %>% 
  ungroup() %>% 
  arrange(country_name)

clean_energy2 %>% 
  ggplot(aes(reorder(country_name, -`%clean`), `%clean`)) +
  geom_hline(yintercept = 1, color = "grey70") +
  geom_hline(yintercept = 0, color = "grey70") +
  geom_segment(
    aes(x = reorder(country_name, -`%clean`), 
        xend = reorder(country_name, -`%clean`), 
        y = 0, 
        yend = 1.1),
    color = "grey75",
    size = 0.5,
    lty = 3
  ) +
  geom_segment(
    aes(x = reorder(country_name, -`%clean`), 
        xend = reorder(country_name, -`%clean`), 
        y = 0, 
        yend = `%clean`),
    color = "grey40"
  ) +
  geom_point(aes(size = `%clean`, fill = over50perc),
             shape = 21,
             color = "grey40",
             alpha = 0.8) +
  labs(
    x = NULL,
    y = NULL,
    title = "Clean energy in EU Countries 2018",
    subtitle = "Clean energy = Energy not produced by conventional thermal methods",
    caption = "@MaiaPelletier | #TidyTuesday | Data: Eurostat"
  ) +
  scale_fill_manual(values = c("#f0f0f0", "grey40"), 
                    breaks = c("Over 50% clean energy"),
                    labels = c("Over 50% clean"),
                    name = NULL) +
  scale_size(range = c(1, 4)) +
  scale_y_continuous(limits = c(-0.15, 1.1)) +
  guides(size = "none") +
  coord_polar(clip = "off") +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 8),
    plot.margin = margin(0, 50, 0, 50),
    plot.title = element_text(hjust = 0.5, 
                              size = 14,
                              face = "plain",
                              family = "Arial",
                              margin = margin(10, 0, 10, 0)),
    plot.subtitle = element_text(hjust = 0.5, 
                                 size = 8,
                                 face = "italic"),
    plot.caption = element_text(size = 6, 
                                margin = margin(0, 0, 8, 0)),
    axis.text.y = element_blank(),
    axis.text.x = element_text(hjust = -1, size = 7),
    legend.position = c(0.80, 0.7),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(2, 8, 2, 2),
    legend.background = element_blank(),
    legend.box.background = element_rect(color = "grey65", fill = "#F0F0F0"),
    legend.key = element_blank()
  ) +
  NULL
  ggsave(here("images", "progress", "imgs_week32", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), type = 'cairo')
