library(tidyverse)
#install.packages("GGally")
library(GGally)
#install.packages("ggimage")

footballData <- read.csv("./england-premier-league-players-2018-to-2019-stats.csv")

footballData %>% names()

footballData %>% 
  ggplot(aes(x= age 
             )) + geom_bar(fill = "#d64820", color = "black") +
  labs(title = "Average Ages of Player In Premier League", 
       x = "Age", 
       y = "# of Players")


