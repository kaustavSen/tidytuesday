library(tidyverse)

news_orgs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-05/news_orgs.csv')

plot_data <- 
  news_orgs |> 
  filter(!is.na(year_founded), year_founded < 2021) |> 
  count(year_founded)
  
ggplot(plot_data, aes(year_founded, n)) +
  geom_line()

write_csv(plot_data, "2022/week_14/plot_data.csv")
