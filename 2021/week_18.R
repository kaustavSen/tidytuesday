library(tidyverse)
library(packcircles)
library(ragg)
library(ggtext)
library(here)

tues_data <- tidytuesdayR::tt_load(2021, week = 18)
departures <- tues_data$departures

fired <- 
  departures %>% 
  filter(!is.na(departure_code), fyear %in% 2010:2018) %>% 
  mutate(depature_reason = if_else(departure_code %in% c(3, 4), "fired", "normal")) %>% 
  count(fyear, depature_reason) %>% 
  pivot_wider(names_from = depature_reason, values_from = n) %>% 
  mutate(total = fired + normal, 
         prop_fired = fired / total)

colors <- c("#ff6b6b", "grey85")

set.seed(100)

p <- 
  fired %>% 
  rowwise() %>% 
  mutate(
    df = list(map_df(total, ~circleProgressiveLayout(rep(1, .x)))),
    colors = list(sample(colors, size = total, replace = TRUE, 
                         prob = c(prop_fired, 1-prop_fired)))
  ) %>% 
  unnest(c(df, colors)) %>% 
  mutate(people = sample(letters, size = 2661, replace = TRUE)) %>% 
  ggplot() +
  geom_text(aes(x = x, y = y, label = people, color = colors), 
            family = "WeePeople", size = 7, show.legend = FALSE) +
  scale_color_identity() +
  labs(
    title = "Bossman gone: <span style='color: grey'>CEO Departures</span>",
    subtitle = "Each person in the plots below represents a CEO who was dismissed from the job.<br />
    Those highlighted in **<span style='color: #ff6b6b'>red</span>** are involuntary dismissals due to job performance or legal violations.",
    caption = "**Data:** Gentry et al. (2021) | **Icons:** WeePeople | **Plot:** Kaustav Sen"
  ) +
  facet_wrap(~fyear, nrow = 3) +
  theme_void(base_size = 12, base_family = "Roboto Condensed") +
  theme(
    plot.margin = margin(20, 30, 20, 30),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_markdown(family = "Limelight", hjust = 0.5, size = rel(3)),
    plot.subtitle = element_markdown(hjust = 0.5, size = rel(1.7), 
                                     margin = margin(t = 15, b = 15), lineheight = 1.2),
    plot.caption = element_markdown(hjust = 0.5, size = rel(1.4),
                                    margin = margin(t = 20), color = "grey40"),
    strip.text = element_text(size = rel(1.6), color = "white", face = "bold", 
                              margin = margin(t = 2, b = 2)),
    strip.background = element_rect(fill = "#264653", color = "#264653"),
    panel.spacing = unit(2, "lines"),
    panel.border = element_rect(fill = NA, color = "grey70", size = 0.5)
  )

agg_png(here(2021, "plots", "week_18.png"), width = 12, height = 16, 
        units = "in", res = 320)
print(p)
dev.off()
