library(tidyverse)
library(gganimate)
library(ggplot2)
dat <- read_csv("../../Data/BioLog_Plate_Data.csv")

colnames(dat)[6] <- "24"
colnames(dat)[7] <- "48"
colnames(dat)[8] <- "144"
# CLean Data set and make column specifying wether water or not
dat <- dat %>% 
  pivot_longer(cols = c("24", "48", "144"), names_to = "Time",
               values_to = "Absorbance")
dat <- dat %>% mutate(Type = dat$`Sample ID`)

dat[c(1:864, 2593:3456), 8] = "Water"
dat[c(865:2593), 8] = "Soil"

#Generate plot that matches the one on page


dat %>% 
  ggplot(aes(x=Dilution, y= Absorbance, color = Type)) + 
  geom_line() +
  facet_wrap(~ Substrate)

#Generates animated plot

dat %>% 
  ggplot(aes(x= time(), y = mean(Absorbance))) +
  transition_time(time = 0:150) +
  ease_aes('linear')
