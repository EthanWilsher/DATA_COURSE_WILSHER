#Week 3
 
#downloaded rectangles

df <- read.csv("./Data/rectangles.csv")
#fix the character column
df$width <- as.numeric(df$width)
#area?
df$area <- df$width * df$length

read.csv

write.csv(df,"./Data/rectangles_clean.csv")

#to load a package if already installed

library(tidyverse)

df <- read_csv("./Data/rectangles.csv")
glimpse(df)

#pipe operator or the %>% means the output of the left function becomes the thing 
#for the right function to analyze
mean(df$length)
df$length %>% mean() %>% 
  length() %>% 
  class() 

df %>%
  names() %>%
  nchar()

df %>% names()

#fix width collumn

df$width <- df$width %>% as.numeric()

df %>%
  ggplot(aes(x=length,y=width)) + 
  geom_point() + 
  geom_smooth()

iris

iris %>% names()
iris$Species %>% unique()

iris %>%
  ggplot(aes(x=Species,y=Petal.Length,color=Species)) +
  geom_boxplot() +
  geom_point() +
  geom_jitter(width = .1,alpha=.25) 

iris %>% 
  ggplot(aes(x=Petal.Length,y=Petal.Width,color=Species)) + 
  geom_point(color="#42f5d1") +
  geom_smooth(method = "lm") + 
  theme_classic()

iris[iris$Species != "Virginica",] 
