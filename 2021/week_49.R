library(tidyverse)

matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-30/matches.csv')

comment_1 <- str_wrap('The period 1996-99 saw a reduction in the "home advantage" with the away team winning more than 50% of the time in 1999', width = 40)
comment_2 <- str_wrap('Overtime the "home advantage" seems to have converged to about a +10% advantage', width = 40)
comment_3 <- str_wrap('2000 was an interesting year where every 2 out of 3 matches were won by the home team', width = 30)

plot_data <- 
  matches %>% 
  filter(team1_away_or_home == "home" | team2_home_away == "home") %>% 
  select(team1, team2, winner, team1_away_or_home, team2_home_away, match_date) %>% 
  mutate(
    winner_team = if_else(winner == team1, "team1", "team2"),
    outcome = if_else(
      (winner_team == "team1" & team1_away_or_home == "home") |
      (winner_team == "team2" & team2_home_away == "home"),
      "home_wins",
      "away_wins"
    ),
    match_year = parse_number(str_extract(match_date, "[0-9]{4}$"))
  ) %>% 
  count(match_year, outcome) %>% 
  group_by(match_year) %>% 
  mutate(prop = n / sum(n))

ggplot(plot_data, aes(match_year, prop)) +
  geom_col(aes(fill = outcome), alpha = 0.9, width = 0.95) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "white", size = 0.5) +
  geom_hline(yintercept = 0.6, linetype = "dashed", color = "white", size = 0.5) +
  annotate("text", x = 2005.2, y = 0.55, label = "+10% Advantage", family = "Inter",
           size = 3, fontface = "bold", hjust = 1, color = "black") +
  annotate("segment", x = 2005.25, xend = 2005.25, y = 0.5, yend = 0.6, 
           arrow = arrow(ends = "both", length = unit(1, "mm"), type = "closed")) +
  geom_hline(yintercept = 0, size = 0.6) +
  annotate("text", x = 1995.7, y = 0.11, label = "HOME WINS", color = "white", fontface = "bold",
           family = "Inter", size = 3, angle = 90) +
  annotate("text", x = 1995.7, y = 0.9, label = "AWAY WINS", color = "white", fontface = "bold",
           family = "Inter", size = 3, angle = 90) +
  annotate("text", x = 1996.6, y = 0.70, label = comment_1, hjust = 0, size = 3,
           family = "Inter", lineheight = 1.1) +
  annotate("text", x = 2003.5, y = 0.64, label = comment_2, hjust = 0.5, size = 3,
           family = "Inter", lineheight = 1.1) +
  annotate("text", x = 2000.75, y = 0.8, label = comment_3, hjust = 0, size = 3,
           family = "Inter", lineheight = 1.1) +
  annotate("segment", x = 2000, xend = 2000, y = 0.663, yend = 0.75) +
  annotate("segment", x = 2000, xend = 2000.7, y = 0.75, yend = 0.8) +
  geom_text(
    data = tibble(match_year = 1995.5,
                  prop = seq(0, 1, 0.25),
                  label = c("0  ", " 25  ", " 50  ", "75  ", "100%")),
    aes(label = label), nudge_x = -0.1, nudge_y = 0.02, hjust = 1,
    family = "Inter", fontface = "bold", size = 3.5, color = "grey60"
  ) +
  scale_fill_manual(values = c("home_wins" = "#3d5a80", "away_wins" = "#ee6c4d")) +
  scale_x_continuous(breaks = c(1996, 2000, 2005), expand = expansion(0.02)) +
  labs(
    title = "The home advantage",
    subtitle = "Historically it has been seen that in cricket matches, the home team has a slightly higher chance of winning",
    caption = "**Data:** ESPN Cricinfo | **Plot:** Kaustav Sen"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "Inter", base_size = 14) +
  theme(
    plot.margin = margin(10, 10, 10, 20),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(face = "bold", size = rel(1.5)),
    plot.subtitle = element_text(size = rel(0.8), color = "grey40"),
    plot.caption = ggtext::element_markdown(size = rel(0.7), hjust = 0.5, color = "grey60"),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(vjust = 9, size = rel(0.9), face = "bold")
  )
ggsave("2021/plots/week_49.png", width = 10, height = 6, dpi = 300)
