library(tidyverse)
library(countrycode)
library(NatParksPalettes)
library(ggtext)
library(gggrid)

systemfonts::register_font(
  "heading font",
  plain = "/home/kaustav/.fonts/Inter-Black.ttf"
)

color_pal <- natparks.pals("DeathValley", n = 5, type = "discrete")

technology <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv')

cellular_telephone_df <- 
  technology |> 
  filter(label %in% c("Fixed telephone subscriptions", "Cellular subscriptions"), variable != "telephone_canning_wdi") |>  
  select(-variable) |>
  pivot_wider(names_from = label, values_from = value) |> 
  janitor::clean_names() |> 
  mutate(across(ends_with("tions"), ~ replace_na(.x, 0))) |> 
  mutate(cell_exceeds_tele = cellular_subscriptions > fixed_telephone_subscriptions) |> 
  group_by(iso3c) |> 
  filter(cell_exceeds_tele) |> 
  slice_min(year) |> 
  ungroup()

plot_data <- 
  cellular_telephone_df |> 
  mutate(country_name = countrycode(iso3c, origin = "iso3c", destination = "country.name"),
         region = countrycode(iso3c, origin = "iso3c", destination = "continent")) |> 
  count(year, region)

plot_data |> 
  count(region)

label_oceania <- textGrob(
  "Oceania",
  x = unit(0.48, "npc"),
  y = unit(-6, "mm"),
  hjust = 0.5,
  gp = gpar(fontfamily = "Inter", fontface = "bold", fontsize = 2.5 * .pt, col = color_pal[5])
)

segment_oceania <- segmentsGrob(
  x0 = unit(0.48, "npc"),
  x1 = unit(0.48, "npc"),
  y0 = unit(-4.8, "mm"),
  y1 = unit(0, "mm"),
  gp = gpar(lty = "dashed", col = color_pal[5],  size = 0.4)
)

ggplot(plot_data) +
  geom_col(aes(year, n, fill = region), alpha = 0.8) +
  annotate("segment", x = 1984, xend = 1984, y = 3.6, yend = 1.2, size = 0.4, linetype = "dashed", color = color_pal[2]) +
  annotate("segment", x = 1985, xend = 1985, y = 8.5, yend = 2.2, size = 0.4, linetype = "dashed", color = color_pal[4]) +
  annotate("segment", x = 1996, xend = 1996, y = 19, yend = 15, size = 0.4, linetype = "dashed", color = color_pal[1]) +
  annotate("segment", x = 1996, xend = 1999.5, y = 15, yend = 15, size = 0.4, linetype = "dashed", color = color_pal[1]) +
  annotate("segment", x = 2004.5, xend = 2003.5, y = 16, yend = 16, size = 0.4, linetype = "dashed", color = color_pal[3]) +
  annotate("segment", x = 2000.5, xend = 2002.5, y = 25, yend = 25, size = 0.4, linetype = "dashed", color = "grey60") +
  annotate("segment", x = 1989, xend = 1989, y = 14.2, yend = 25, size = 0.4, linetype = "dashed", color = "grey60") +
  annotate("segment", x = 1989, xend = 1990, y = 25, yend = 25, size = 0.4, linetype = "dashed", color = "grey60") +
  annotate("richtext", x = 1981, y = 5, label = "<span style='color: #C5692D'>**Americas**</span><br>USA was the frontrunner<br>in mobile phone adoption",
           family = "Inter", size = 2.5, hjust = 0, color = "grey40", fill = "grey98", alpha = 0.95, label.size = unit(0, "mm")) +
  annotate("richtext", x = 1983, y = 10, label = "<span style='color: #68434E'>**Europe**</span><br>On the other side of the<br>Atlantic, UK and Norway<br>led the cellular revolution",
           family = "Inter", size = 2.5, hjust = 0, color = "grey40", fill = "grey98", label.size = unit(0, "mm")) +
  annotate("richtext", x = 1992, y = 20, label = "<span style='color: #8C2B0E'>**Africa**</span><br>Cellular adoption peaked around the<br>late-90s and early 2000s",
           family = "Inter", size = 2.5, hjust = 0, color = "grey40", fill = "grey98", label.size = unit(0, "mm")) +
  annotate("richtext", x = 2004.5, y = 16, label = "<span style='color: #132F5B'>**Asia**</span><br>From 2000 to 2004 is the period<br>when post Asian countries saw<br>mobile phone usage exceed<br>landline usage",
           family = "Inter", size = 2.5, hjust = 0, color = "grey40", fill = "grey98", label.size = unit(0, "mm")) +
  annotate("richtext", x = 1995, y = 25.5, label = "<span style='color: #4d4d4d; font-size: 13px'>**The two peaks**</span><br>1989 and 2003 were two years which saw signifcant countries<br>achieving mass adoption of cellular technology. Interestingly, in<br>both these years <span style='color: #132F5B'>Asian</span> and <span style='color: #8C2B0E'>African</span> countires played a pivotal role.",
           family = "Inter", size = 2.5, hjust = 0.5, color = "grey40", fill = "grey98", label.size = unit(0, "mm")) +
  grid_panel(label_oceania) +
  grid_panel(segment_oceania) +
  theme_minimal(base_family = "Inter", base_size = 16) +
  scale_y_continuous(name = "# of countries", limits = c(0, 30), expand = c(0, 0)) +
  scale_x_continuous(name = "", limits = c(1980, NA), expand = c(0.02, 0)) +
  scale_fill_manual(values = natparks.pals("DeathValley", n = 5, type = "discrete")) +
  coord_cartesian(clip = "off") +
  labs(
    title = "The inflextion year for mobile phone usage",
    subtitle = "Country wise distribution of the year in which mobile phone usage exceeded landline telephone usage for the first time.\nA further grouping by continent brings out how this metric correlates with neighbouring countries.",
    caption = "**Data:** data.nber.org | **Plot:** Kaustav Sen"
  ) +
  theme(
    plot.background = element_rect(color = "grey98", fill = "grey98"),
    plot.margin = margin(10, 20, 10, 20),
    plot.title.position = "plot",
    plot.title = element_text(family = "heading_font", face = "bold", size = rel(1.2), color = "grey30"),
    plot.subtitle = element_text(size = rel(0.75), color = "grey50", lineheight = 1.2, margin = margin(b = 15)),
    plot.caption.position = "plot",
    plot.caption = element_markdown(hjust = 0.5, color = "grey70", size = rel(0.5), margin = margin(t = -5)),
    axis.title.y = element_text(size = rel(0.55), color = "grey40", face = "bold", hjust = 1),
    axis.text = element_text(size = rel(0.6), color = "grey60"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
ggsave("2022/plots/week_29.png", width = 10, height = 6, dpi = 300)
