library(tidyverse)
library(ggfx)
library(ggforce)

# The final plot makes use of installed system fonts.
# Since I am working on a windows machine, have to run the following command
# to load the fonts into R
# extrafont::loadfonts(device = "win", quiet = TRUE)

tues_data <- tidytuesdayR::tt_load(2021, week = 12)

games <- tues_data$games

# Top FPS games taken from Steam's website: 
# https://store.steampowered.com/tags/en/FPS/#p=0&tab=ConcurrentUsers
# Ignored games which have been launched only recently (last year or so).
top_fps_games <- c("PLAYERUNKNOWN'S BATTLEGROUNDS", 
                   "Counter-Strike: Global Offensive", 
                   "Tom Clancy's Rainbow Six Siege",
                   "Team Fortress 2")

games_fps <-
  games %>% 
  filter(gamename %in% top_fps_games) %>% 
  group_by(gamename) %>% 
  mutate(
    year_month = lubridate::ymd(paste0(year, month, 01, sep = "-")),
    peak_dist = peak / max(peak)
  ) %>% 
  select(gamename, year_month, peak_dist) %>% 
  ungroup() %>% 
  # This has been done to ensure that the full range of dates is present for all
  # the four games. For games lauched after the minimum date, NA will get introduced.
  complete(year_month, nesting(gamename)) %>% 
  mutate(date_numeric = as.numeric(factor(year_month)))

fps_games_text = c(
  "PUBG's popularity skyrocketed soon after its introduction in early 2017. However, since then it this fad has reduced significantly.",
  "CS:GO started off slow but overtime has maintained a stable and loyal fan base on Steam.",
  "Rainbow Six Siege's popularity on Steam has shown a steady increase over time since it's launch in early 2016.",
  "Team Fortress 2 has maintained it's reputation of being a fan favourite on Steam."
)

annotations <- tibble(
  gamename = top_fps_games,
  text = str_wrap(fps_games_text, width = 40)
)

# Steps to get the (x,y) coordinates for rifle:
# 1. Get SVG image from the Noun Project: https://thenounproject.com/
# 2. Upload this onto "Coordinator" website: https://spotify.github.io/coordinator/
# 3. Download the .csv generated from the website! 
# 4. Don't forget to attribute the SVG image used :)

rifle_shape <- 
  read_csv(here::here(2021, "assets", "rifle_shape.csv")) %>% 
  mutate(
    # The plotted image was upside down and hence this translation.
    y = max(y) - y,
    # The below translations were done to convert the range of (x,y) coordinates
    # same as the range of dates.
    old_range_x = max(x) - min(x),
    new_range_x = 103,
    new_x = 1 + (x - min(x)) * new_range_x / old_range_x,
    old_range_y = max(y) - min(y),
    new_range_y = 0.98,
    new_y = 0.5 + (y - min(y)) * new_range_y / old_range_y
  ) 

# The rifle trigger needs to be separated from the rifle body.
# Otherwise, the SVG plotted will not look right.
# The x and y boundary conditions were deduced by visually inspecting the plotted figure.

rifle_trigger <- rifle_shape %>% 
  filter(y >= 65, y <= 110, x >= 250, x <= 310)

rifle_body <- rifle_shape %>% 
  anti_join(rifle_trigger)

ggplot() +
  as_reference(
    geom_bspline_closed0(data = rifle_body, aes(new_x, new_y)),
    id = "rifle"
  ) +
  with_blend(
    geom_tile(data = games_fps, aes(x = date_numeric, y = 1, fill = peak_dist), show.legend = FALSE),
    bg_layer = "rifle",
    blend_type = "in"
  ) +
  geom_bspline_closed0(data = rifle_trigger, aes(new_x, new_y), fill = "#161a1d") +
  geom_hline(yintercept = 1.6, size = 0.75, color = "#0b090a") +
  geom_label(data = tibble(x = c(1, 49, 97), y = 1.6, label = c("2012\nJULY", "2016\nJULY", "2020\nJULY")), aes(x = x, y = y, label = label), family = "Staatliches", size = 4, color = "#f5f3f4", fill = "#161a1d", label.size = NA, lineheight = 0.9) +
  geom_text(data = annotations, aes(x = 63, y = 0.8, label = text), hjust = 0, lineheight = 0.9, size = 4, family = "Oswald", color = "#f5f3f4") +
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  ggsci::scale_fill_material("deep-orange", na.value = "grey80") +
  facet_wrap(~gamename) +
  labs(
    title = "Face Off: First Person Shooter games on Steam",
    subtitle = str_wrap("The plots below show how the popularity of some of the top selling FPS games have varied over time on the game streaming service, Steam. A darker hue represents a peak in popularity measured as the number of players playing the game at the same time. The gray shading means no data implying the game was not yet available on Steam.", width = 150),
    caption = "Data: Steam | Plot: Kaustav Sen | Rifle image by Vectorstall from the Noun Project"
  ) +
  theme_void() +
  theme(
    text = element_text(size = 14, color = "#f5f3f4"),
    plot.title.position = "plot",
    plot.title = element_text(family = "Staatliches", hjust = 0.5, size = rel(2.5), margin = margin(b = 10)),
    plot.subtitle = element_text(family = "Oswald", hjust = 0.5, size = rel(1), margin = margin(b = 20)),
    plot.caption = element_text(family = "Staatliches", color = "#b1a7a6", hjust = 0.5, size = rel(0.9), margin = margin(t = 20)),
    plot.margin = margin(20, 20, 20, 20),
    plot.background = element_rect(fill = "#0b090a", color = NA),
    panel.spacing = unit(1, "lines"),
    panel.background = element_rect(fill = "#161a1d", color = NA),
    strip.background = element_rect(fill = "#e5383b", color = NA),
    strip.text = element_text(size = rel(1.2), family = "Staatliches", margin = margin(5, 5, 5, 5))
  ) +
  ggsave(here::here(2021, "plots", "week_12.png"), height = 8, width = 16, device = ragg::agg_png())