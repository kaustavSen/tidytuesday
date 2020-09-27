library(tidyverse)
library(ggtext)
library(showtext)
library(patchwork)

font_add_google("Fira Sans", "Fira sans")

df_hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')

df_hotels

## colors
cols <- c("#02cfc6", "#ff6547", "#ffbc00", "#1f8eff")
col8 <- c(rep("grey52", 4), cols)   ## -> colored text in the two lower rows
col5 <- c(cols, "grey80")   ## -> grey text in the two lower rows also change code below

plot_data <- 
  df_hotels %>% 
  mutate(
    parents = if_else(children > 0 | babies > 0, 1, 0),
    parents_detailed = case_when(
      children > 0 ~ "parents",
      babies > 0 ~ "young_parents",
      TRUE ~ "no_parents"
    ),
    meal_num = if_else(meal %in% c("HB", "FB"), 1, 0),
    not_canceled = if_else(is_canceled == 0, 1, 0)
  ) %>% 
  select(parents_detailed, not_canceled, stays_in_week_nights,
         meal_num, total_of_special_requests) %>% 
  pivot_longer(cols = not_canceled:total_of_special_requests,
               names_to = "variable",
               values_to = "value") %>% 
  group_by(parents_detailed, variable) %>% 
  summarise(
    yes = sum(value > 0) / n(),
    no = sum(value == 0) / n(),
    lab = glue::glue("{round(yes * 100, 0)}%")
  ) %>% 
  pivot_longer(
    cols = c(yes, no),
    names_to = "group",
    values_to = "value"
  ) %>% 
  group_by(parents_detailed, variable) %>% 
  mutate(
    ymax = cumsum(value),
    ymin = c(0, head(ymax, n = -1)),
    fil = glue::glue("{group}.{variable}"),
    col = glue::glue("{group}.{variable}"),
    col = if_else(parents_detailed == "young_parents" & group == "yes", unclass(col), "z"),  
    text = case_when(
      variable == "not_canceled" ~ "did not cancel the\nhotel booking",
      variable == "stays_in_weekend_nights" ~ "stayed over\nthe weekend",
      variable == "meal_num" ~ "booked at least\n1 warm meal",
      variable == "total_of_special_requests" ~ "had at least\n1 special request",
    ),
    lab_col = if_else(group == "yes" & parents_detailed == "young_parents", unclass(lab), NA_character_),
    lab_alt = if_else(group == "yes" & parents_detailed != "young_parents", unclass(lab), NA_character_),
    text = if_else(parents_detailed == "young_parents" & group == "yes", text, NA_character_)
  ) %>% 
  group_by(variable) %>% 
  mutate(max = if_else(group == "yes", max(ymax[ymax != max(ymax)]), NA_real_)) %>% 
  ungroup() %>% 
  mutate(variable = factor(variable,
                           levels = c("not_canceled", 
                                      "stays_in_weekend_nights", 
                                      "meal_num", 
                                      "total_of_special_requests")),
         parents_detailed = factor(parents_detailed, 
                                   levels = c("young_parents",
                                              "parents",
                                              "no_parents"),
                                   labels = c("Of guests\nwho traveled:\n\n\n\nwith babies...",
                                              "\n\n\n\n\nwith children...",
                                              "\n\n\n\n\nwithout children...")
         )
  )

showtext_auto()

theme_set(theme_minimal(base_family = "Fira sans"))
theme_update(plot.background = element_rect(fill = "#6d7069", 
                                            color = "#6d7069"),
             panel.background = element_rect(fill = NA, color = NA),
             panel.grid = element_blank(),
             axis.ticks = element_blank(),
             axis.text = element_blank(),
             axis.title = element_blank(),
             strip.text.x = element_blank(),
             strip.text.y.left = element_text(size = 25,
                                              color = "grey70",
                                              face = "bold",
                                              vjust = .85,
                                              hjust = 0,
                                              angle = 0),
             plot.title = element_text(size = 56,
                                       color = "grey95",
                                       face = "bold",
                                       hjust = .5,
                                       margin = margin(30, 0, 20, 0)),
             plot.subtitle = element_text(size = 28,
                                          color = "grey82",
                                          face = "plain",
                                          hjust = .5,
                                          lineheight = 1.05,
                                          #margin = margin(0, 0, 80, 0)),
                                          margin = margin(30, 0, 80, 0)),
             plot.caption = element_text(size = 20,
                                         color = "grey70",
                                         face = "bold",
                                         hjust = .5,
                                         margin = margin(40, 0, 0, 0)),
             plot.margin = margin(30, 60, 30, 60),
             panel.spacing.y = unit(0, "pt"))

plot <- 
  ggplot(plot_data) +
  aes(xmax = 3, xmin = 2.4,
      ymax = ymax, ymin = ymin) +
  geom_rect(color = "grey52",
            fill = "grey52")+
  geom_rect(
    aes(fill = fil,
        alpha = parents_detailed),
    color = "#6d7069",
    size = 1.5
  ) +
  geom_rect(
    aes(xmax = 2.964, xmin = 2.436,
        ymax = max - 0.0025, ymin = max - 0.0045,
        fill = fil),
    color = NA
  ) +
  geom_text(
    aes(x = 0.8, y = 0,
        label = lab_col,
        color = col),
    family = "Fira sans",
    fontface = "bold",
    size = 25
  ) +
  geom_text(aes(x = 0, y = 0, 
                label = lab_alt,
                color = col), 
            alpha = .6,
            family = "Fira sans",
            fontface = "bold",
            size = 20) +
  geom_text(aes(x = 0, y = 0.5, 
                label = text,
                color = col),
            family = "Fira sans",
            fontface = "bold",
            size = 8,
            lineheight = .8,
            vjust = 1) +
  facet_grid(parents_detailed ~ variable, switch = "y") +
  coord_polar(theta = "y") +
  scale_x_continuous(limits = c(0, 3)) +
  scale_color_manual(values = col5, guide = F) +
  scale_fill_manual(values = col8, guide = F) +
  scale_alpha_manual(values = c(1, 0.6, 0.36), guide = F) +
  labs(subtitle = "Most guests that traveled with babies did not cancel their hotel booking, stayed over the weekend\nand had at least one special request. These guests also booked more often at least lunch or dinner.\nThe data comprises ~12,000 hotel bookings & cancelations in the period from July 2015 to August 2017.",
      caption = "Visualization by Cédric Scherer  •  Data by Antonio, Almeida & Nunes 2019 (doi: 10.1016/j.dib.2018.11.126)"
      )

path <- here::here(2020, "reference", "week_07", "cedric")

ggsave(paste0(path, ".pdf"), plot, width = 24, height = 18.84,
       device = cairo_pdf)

pdftools::pdf_convert(paste0(path, ".pdf"),
                      filenames = paste0(path, ".png"),
                      dpi = 320)
