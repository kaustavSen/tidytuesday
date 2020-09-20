library(tidyverse)
library(ggbump)
library(showtext)

tuesdata <- tidytuesdayR::tt_load(2020, week = 31)

penguins <- tuesdata$penguins

pal1 <- c("#FF8C01", "#A034F0", "#4CA4A4")
pal2 <- c(pal1, as.character(prismatic::clr_darken(pal1, shift = 0.3)))
pal2 <- pal2[c(1,4,2,5,3,6)]

font_add_google("Ranchers")
font_add_google("Oswald")

data <- 
  penguins %>% 
  filter(!is.na(sex)) %>% 
  select(-year, -island) %>% 
  mutate(
    id = row_number(),
    sex = str_to_title(sex),
    comb = paste(species, sex)
  ) %>% 
  pivot_longer(cols = bill_length_mm:body_mass_g, values_to = "value", names_to = "variable") %>% 
  mutate(
    variable = fct_relevel(variable, "flipper_length_mm","bill_length_mm","bill_depth_mm","body_mass_g"),
    variable = fct_relabel(variable, ~str_replace_all(., "_", " ") %>% 
                             str_replace("(.*) (.+)$", "\\1 (\\2)") %>% 
                             str_to_sentence())
  ) %>% 
  group_by(variable) %>% 
  mutate(
    st = (value - min(value)) / (max(value) - min(value))
  )

summ_data <- 
  data %>% 
  group_by(species, sex, comb, variable) %>% 
  summarise(stat = median(value),
            stat_st = median(st))

showtext_auto()

ggplot() +
  aes(x = variable, color = comb, group = comb) +
  geom_bump(
    data = data,
    aes(y = st, group = id),
    size = 2,
    alpha = 0.07,
    lineend = "round"
  ) +
  geom_bump(
    data = summ_data,
    aes(y = stat_st),
    size = 2
  ) +
  geom_label(
    data = summ_data,
    aes(y = stat_st, fill = comb, label = stat),
    size = 5,
    color = "white",
    family = "Ranchers",
    label.size = NA
  ) +
  scale_y_continuous(
    limits = c(0,1),
    breaks = c(0,1),
    labels = c("Min.", "Max."),
    sec.axis = dup_axis()
  ) +
  scale_x_discrete(
    position = "top"
  ) +
  facet_wrap(~species) +
  scale_color_manual(values = pal2) +
  scale_fill_manual(values = pal2) +
  labs(
    title = "Palmer Penguins",
    subtitle = "Median by sex of physical measures for the three different species of penguins in the Palmer Archipelago, Antarctica.",
    caption = "@AtMissing · Palmer Archipelago (Antarctica) penguin data by Gorman, Williams and Fraser, 2014"
  ) +
  theme(
    plot.title = element_text(
      family = "Ranchers",
      size = 40,
      hjust = 0.5,
      margin = margin(t = 20, b = 5)
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      family = "Oswald",
      margin = margin(t = 5, b = 10)
    ),
    plot.caption = element_text(
      hjust = 0.5,
      family = "Oswald",
      margin = margin(t = 20, b = 10)
    ),
    strip.placement = "outside",
    strip.text = element_text(
      family = "Ranchers",
      size = 20,
      color = "white"
    ),
    strip.background = element_rect(
      fill = "black"
    ),
    axis.text.x = element_text(
      family = "Oswald"
    ),
    panel.background = element_rect(
      fill = "#ECEBE4"
    ),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "none"
  ) +
  ggsave(here::here("2020", "reference", "week_31", "mnaR99.pdf"), 
         height = 10, width = 20, device = cairo_pdf)
  
