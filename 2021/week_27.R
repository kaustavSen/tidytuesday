library(tidyverse)
library(lubridate)
library(ggridges)
library(ggtext)
library(wesanderson)
library(grid)

tues_data <- tidytuesdayR::tt_load(2021, week = 27)
animal_rescue <- tues_data$animal_rescues

time_labels <- c("Midnight", "Early Dawn", "Morning", "Late Afternoon", "Night")
animal_colors <- tibble(animal= c("Bird", "Cat", "Dog"), 
                        color = wes_palettes$FantasticFox1,
                        label = glue::glue("<span style=color:'{color}'>{animal}</sapn>"))

rescue_time_dist <- 
  animal_rescue %>% 
  filter(animal_group_parent %in% c("Cat", "Bird", "Dog"), 
         cal_year == 2020,
         special_service_type_category != "Other animal assistance") %>% 
  mutate(
    date_time_of_call = dmy_hm(date_time_of_call),
    time_hour = hour(date_time_of_call)
  ) %>% 
  left_join(animal_colors, by = c("animal_group_parent" = "animal")) %>% 
  mutate(
    animal_group_parent = glue::glue("<span style=color:'{color}'>{animal_group_parent}</sapn>"),
    animal_group_parent = factor(animal_group_parent, levels = animal_colors$label[c(2, 3, 1)])
  )

p <- 
  ggplot(rescue_time_dist, aes(time_hour, animal_group_parent, fill = color)) +
  geom_density_ridges(alpha = 0.8, color = "grey90", size = 0.2) +
  scale_x_continuous(limits = c(0, 23), breaks = c(0, 5, 10, 15, 20), 
                     labels = time_labels, guide = guide_axis(n.dodge = 2)) +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.1))) +
  scale_fill_identity() +
  labs(
    title = "Help on the way - London Fire Brigade Animal Rescues over 2020",
    caption = "**Data:** London.gov | **Plot:** Kaustav Sen"
  ) +
  facet_wrap(~special_service_type_category) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "Roboto Condensed", base_size = 16) +
  theme(
    plot.background = element_rect(fill = "grey98", color = "grey98"),
    plot.title.position = "plot",
    plot.title = element_text(family = "Larrikin", hjust = 0.5, color = "#E58601"),
    plot.caption.position = "plot",
    plot.caption = element_markdown(hjust = 0.5, margin = margin(t = 20), size = rel(0.7), color = "grey60"),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.y = element_markdown(vjust = -4, face = "bold"),
    axis.text.x = element_text(size = rel(0.8)),
    panel.spacing.x = unit(10, "mm"),
    strip.text = element_text(face = "bold", color = "grey60")
  )

text_dogs_heights <- textGrob(label = str_wrap("Seems like dogs need the most rescuing from heights during afternoons probably from their mid-day explorations", 30),
                              x = unit(0.35, "npc"),
                              y = unit(0.76, "npc"),
                              just = "left", 
                              gp = gpar(family = "Roboto", fontsize = 9, 
                                        lineheight = 1.1, col = "#3B9AB2"))

text_birds_cats_heights <- textGrob(label = str_wrap("No surprise that cats and birds do not need to be rescued from heights often!", 50),
                              x = unit(0.5, "npc"),
                              y = unit(0.49, "npc"),
                              just = "center", 
                              gp = gpar(family = "Roboto", fontsize = 9, 
                                        lineheight = 1.1, col = "#3B9AB2"))

text_birds_cats_water <- textGrob(label = str_wrap("Cats get stuck in water mainly during early morning while brids during early night", 50),
                              x = unit(0.8, "npc"),
                              y = unit(0.8, "npc"),
                              just = "center", 
                              gp = gpar(family = "Roboto", fontsize = 9, 
                                        lineheight = 1.1, col = "#3B9AB2"))

ragg::agg_png("2021/plots/week_27.png", width = 12, height = 6, units = "in", res = 150)
grid.newpage()
plot(p, newpage = FALSE)
grid.draw(text_dogs_heights)
grid.draw(text_birds_cats_heights)
grid.curve(x1 = unit(0.45, "npc"), y1 = unit(0.7, "npc"), 
           x2 = unit(0.52, "npc"), y2 = unit(0.65, "npc"),
           angle = 45, curvature = 0.3,
           arrow = arrow(length=unit(2.5, "mm"), angle = 20, type = "closed"),
           gp = gpar(col = "#46ACC8", fill = "#46ACC8"))
grid.curve(x1 = unit(0.4, "npc"), y1 = unit(0.48, "npc"), 
           x2 = unit(0.5, "npc"), y2 = unit(0.27, "npc"),
           angle = 45, curvature = 0.3,
           arrow = arrow(length=unit(2.5, "mm"), angle = 20, type = "closed"),
           gp = gpar(col = "#DD8D29", fill = "#DD8D29"))
grid.curve(x1 = unit(0.4, "npc"), y1 = unit(0.48, "npc"), 
           x2 = unit(0.49, "npc"), y2 = unit(0.42, "npc"),
           angle = 45, curvature = 0.1,
           arrow = arrow(length=unit(2.5, "mm"), angle = 20, type = "closed"),
           gp = gpar(col = "#E2D200", fill = "#E2D200"))
grid.curve(x1 = unit(0.69, "npc"), y1 = unit(0.8, "npc"), 
           x2 = unit(0.75, "npc"), y2 = unit(0.51, "npc"),
           angle = 10, curvature = 0.6,
           arrow = arrow(length=unit(2.5, "mm"), angle = 20, type = "closed"),
           gp = gpar(col = "#E2D200", fill = "#E2D200"))
grid.curve(x1 = unit(0.89, "npc"), y1 = unit(0.77, "npc"), 
           x2 = unit(0.89, "npc"), y2 = unit(0.42, "npc"),
           angle = 10, curvature = 0.6,
           arrow = arrow(length=unit(2.5, "mm"), angle = 20, type = "closed"),
           gp = gpar(col = "#DD8D29", fill = "#DD8D29"))
grid.draw(text_birds_cats_water)
dev.off()

# Post tweet
rtweet::post_tweet(
  status =
"#TidyTuesday Week 27: Animal Rescues. Explored at what times of the day dogs, cats and brids
need the most 'rescuing'. Also tried by hand at some post-hoc annotations using {grid}.
#RStats code: https://github.com/kaustavSen/tidytuesday/blob/master/2021/week_27.R",
  media = here::here(2021, "plots", "week_27.png"),
  media_alt_text =
"Three density plots showing distribution of animal rescues by time of day.
Left most panel shows animal rescue below ground. Center panel shows animal rescue from height and
right most panel shows animal rescue from water.
Some insights from the plot:
1. Dogs need the most rescuing from heights during afternoons probably from their mid-day explorations.
2. No surprise that cats and birds do not need to be rescued from heights often.
3. Cats get stuck in water mainly during early morning while birds during early night.
Data comes from london.gov website."
)
