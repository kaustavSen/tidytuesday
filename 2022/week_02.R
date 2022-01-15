library(tidyverse)

colony <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')

plot_data <- colony %>% 
  select(year, state, colony_added, colony_lost) %>% 
  drop_na() %>% 
  group_by(year, state) %>% 
  summarise(across(.fns = sum), .groups = "drop") %>% 
  pivot_longer(cols = starts_with("colony"), names_to = "type")

write_csv(plot_data, "2022/data/week_02.csv")
