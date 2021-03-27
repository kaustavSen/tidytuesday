library(tidyverse)
library(ggforce)
library(ggtext)
library(colorspace)
library(systemfonts)

tues_data <- tidytuesdayR::tt_load(2021, week = 13)

register_variant(
  name = "Montserrat Custom",
  family = "Montserrat",
  features = font_feature(ligatures = "discretionary", kern = 1, tnum = 1)
)

unvotes <- tues_data$unvotes

g7_countries <- c("United States", "United Kingdom", "Japan", "Italy", "France", "Germany", "Canada")

# Thanks to Georgios for these codes! 
# Source: https://github.com/gkaramanis/tidytuesday/blob/master/2020-week52/data/mapglyphs-mapping.csv
map_icons <- tibble(
  country = g7_countries,
  icon_code = c("\ue600", "\ue687", "\ue6f7", "\ue682", "\ue683", "\ue684", "\ue641")
)

g7_df <-
  unvotes %>% 
  filter(country %in% g7_countries) %>% 
  count(country, vote) %>% 
  group_by(country) %>% 
  mutate(
    prop = n / sum(n),
    vote = factor(vote, levels = c("yes", "no", "abstain")),
    max_vote = if_else(prop == max(prop), vote, NA_integer_),
    y0 = case_when(
      country %in% c("Italy", "France") ~ 1.5,
      country %in% c("Germany", "Canada") ~ -1.5,
      TRUE ~ 0
    ),
    x0 = case_when(
      country == "United States" ~ 0,
      country == "United Kingdom" ~ 3,
      country == "Japan" ~ 6,
      country == "Italy" ~ 1.5,
      country == "France" ~ 4.5,
      country == "Germany" ~ 1.5,
      country == "Canada" ~ 4.5
    )
  ) %>% 
  fill(max_vote, .direction = "updown") %>% 
  left_join(map_icons, by = "country") %>% 
  arrange(country, vote) 

p <- ggplot(g7_df) +
  geom_mark_rect(aes(x = -0.2, y = -0.75, label = "US is the only G7 nation which voted 'no' the most"), label.family = "Montserrat Custom", label.fontsize = 9, label.width = 50, label.lineheight = 0.8, label.colour = "#1a535c", label.fill = "grey88", con.type = "elbow", con.cap = unit(0, "mm"), expand = unit(0, "mm")) +
  geom_textbox(aes(x = 3, y = 2, label = "France abstained from voting about 30% of the time which was the highest"), width = unit(35, "mm"), family = "Montserrat Custom", fontface = "bold", fill = "grey88", color = "#1a535c", lineheight = 0.9, size = 3, box.r = unit(0, "mm"), box.size = 0) +
  geom_segment(aes(x = 3.45, xend = 3.45, y = 1.69, yend = 2.29), size = 0.6) +
  geom_segment(aes(x = 3.45, xend = 4, y = 2, yend = 2), size = 0.6) +
  geom_arc_bar(aes(x0 = x0, y0 = y0, r0 = 0.7, r = 1, amount = prop, fill = vote, color = after_scale(darken(fill, 0.1))), size = 1.05, stat = "pie") +
  geom_circle(aes(x0 = x0, y0 = y0, r = 0.7, fill = max_vote, fill = after_scale(lighten(fill, 0.6))), color = NA) +
  geom_text(aes(x = x0, y = y0 - 0.45, label = country, color = max_vote), family = "Montserrat Custom", fontface = "bold", size = 3.5) + 
  geom_text(aes(x = x0, y = y0 - 0.3, label = icon_code, color = max_vote), family = "MapGlyphs", size = 30) + 
  scale_fill_manual(values = c("#4ecdc4", "#ff6b6b", "#f7fff7")) +
  scale_color_manual(values = c("#4ecdc4", "#ff6b6b", "#f7fff7")) +
  labs(
    title = "How do the G7 nations vote at the UN?",
    subtitle = "The charts below show the proption of total votes which were <span style='color: #4ecdc4'>**yes**</span>, <span style='color: #ff6b6b'>**no**</span> or <span style='color: #f7fff7'>**abstain**</span>.<br />The map icons are coloured based on the most common vote.",
    caption = "**Data:** Harvard Dataverse | **Plot:** Kaustav Sen"
  ) +
  theme_void(base_size = 14, base_family = "Montserrat Custom") +
  theme(
    legend.position = "none",
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", hjust = 0.5, size = rel(2)),
    plot.subtitle = element_markdown(hjust = 0.5, size = rel(0.9), lineheight = 1.2),
    plot.caption = element_markdown(hjust = 0.5, size = rel(0.7), color = "grey60"),
    plot.margin = margin(10, 20, 10, 20),
    plot.background = element_rect(fill = "grey90", color = NA)
  )

# Export plot as svg and then convert to png using Inkscape
svglite::svglite(here::here(2021, "plots", "week_13.svg"), width = 14, height = 8)
print(p)
dev.off()