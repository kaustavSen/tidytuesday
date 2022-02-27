library(data.table)
library(ggplot2)

freedom <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')

df <- freedom[, .N, keyby = .(year, CL, PR)]

fwrite(df, file = "2022/data/week_08.csv")

freedom[, .N, keyby = .(year, CL, PR)][year %in% c(2000, 2010)] |> 
  ggplot(aes(CL, PR)) +
  geom_point(aes(size = N, color = CL)) +
  scale_x_reverse() +
  scale_y_reverse() +
  scale_size(range = c(4, 12)) +
  scale_color_gradient(low = "#1dbde6", high = "#f1515e") +
  facet_wrap(~year) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Civil Liberties",
    y = "Political Rights"
  ) +
  theme_minimal(base_family = "JetBrains Mono", base_size = 12) +
  theme(
    plot.margin = margin(10, 20, 20, 10),
    legend.position = "none",
    plot.background = element_rect(fill = "grey97", color = "grey97"),
    axis.title = element_text(hjust = 1, face = "bold", size = rel(0.8), color = "grey70"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(family = "Roboto Condensed", face = "bold",
                              size = rel(1.8), color = "steelblue"),
    panel.spacing.x = unit(1, "line")
  )
