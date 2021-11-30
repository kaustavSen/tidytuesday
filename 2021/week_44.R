library(tidyverse)
library(lubridate)

ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

subset_events <- ultra_rankings %>% 
  left_join(race, by = "race_year_id") %>% 
  filter(rank == 1) %>% 
  count(event, race, sort = TRUE) %>% 
  filter(n >= 9) %>% 
  mutate(event_race = paste0(event, "_", race)) %>% 
  pull(event_race)

week_44_data <- 
  ultra_rankings %>% 
  left_join(race, by = "race_year_id") %>% 
  mutate(event_race = paste0(event, "_", race)) %>% 
  filter(rank == 1, event_race %in% subset_events) %>% 
  mutate(year = year(date))

write_csv(week_44_data, "2021/data/week_44_data.csv")

ggplot(week_44_data, aes(year, time_in_seconds)) +
  geom_line() +
  geom_point() +
  facet_wrap(~event_race) +
  theme_minimal()

