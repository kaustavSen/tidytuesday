library(tidyverse)
library(ggrepel)
library(here)

tues_data <- tidytuesdayR::tt_load(2021, week = 22)

drivers <- 
  tues_data$drivers %>% 
  mutate(top_players = fct_collapse(factor(position), 
                                    Penev = "1", 
                                    MR = "2",
                                    Abney317 = "3",
                                    Dan = "4",
                                    other_level = "Others"))

top_players <- 
  drivers %>%
  # For the top 4 players show the entire time-line
  mutate(records = replace_na(records, 0)) %>% 
  filter(top_players != "Others") %>% 
  group_by(player) %>% 
  arrange(year, player) %>% 
  mutate(
    cum_records = cumsum(records)
  ) 

others <- 
  drivers %>% 
  # For all other players only show years in which they have set records
  drop_na() %>% 
  filter(top_players == "Others") %>%
  group_by(player) %>%
  arrange(year, player) %>% 
  mutate(
    cum_records = cumsum(records)
  ) 

colors <- wesanderson::wes_palette("Darjeeling1", n = 4)

top_players %>% ungroup() %>% summarise(range(year))

ggplot(mapping = aes(year, cum_records, group = player)) +
  geom_step(data = others, color = "grey80",  alpha = 0.7, size = 1.1) +
  geom_step(data = top_players, aes(color = player), size = 1.1) +
  geom_text_repel(
    data = filter(top_players, year == 2021),
    aes(label = player, color = player),
    family = "Aquarius",
    fontface = "bold",
    size = 3.5
  ) +
  annotate("text", x = 1997, y = 340, label = "Number of\nrecords", size = 2.5,
           color = "grey70", family = "Aquarius", fontface = "bold", hjust = 0) +
  annotate("text", x = 1999.5, y = 280, 
           label = str_wrap("Alex Penev's domination during the late 90's has been unparalleled and he has the honour of achieving the most number of world records.", width = 35),
           family = "Aquarius", size = 3, hjust = 0, lineheight = 0.95, color = colors[4]) +
  annotate("text", x = 2017.5, y = 225, 
           label = str_wrap("Matthias Rustemeyer, a.k.a MR set out to achieve the ultimate crown - holding all 32 world records at the same time. He even came pretty close to achieving this but sadly fell short by just one record.", width = 45),
           family = "Aquarius", size = 3, hjust = 1, lineheight = 0.95, color = colors[3]) +
  scale_x_continuous(name = "", expand = expansion(mult = 0.01)) +
  scale_y_continuous(name = "", expand = expansion(mult = 0.01), 
                     sec.axis = dup_axis()) +
  scale_color_manual(values = colors) +
  labs(
    title = "<span style='color: #FF0000'>**Mario Kart 64:**</span> <span style='color: #F2AD00'>All Time Record Holders</span>",
    caption = "**Data:** Mario Kart World Records | **Plot:** Kaustav Sen"
  ) +
  coord_cartesian(clip = "off") +
  ggthemes::theme_clean(base_size = 14, base_family = "Aquarius") +
  theme(
    plot.margin = margin(15, 25, 15, 25),
    plot.background = element_rect(fill = "grey97", color = "grey97"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = ggtext::element_markdown(hjust = 0.5, size = rel(1.3), margin = margin(b = 15)),
    plot.caption = ggtext::element_markdown(hjust = 0.5, size = rel(0.6), color = "grey70"),
    legend.position = "none"
  ) +
  ggsave(here("2021", "plots", "week_22.png"), width = 10, height = 7, dpi = 320)