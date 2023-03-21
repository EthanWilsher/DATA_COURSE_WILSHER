library(tidyverse)
library(palmerpenguins)
library(ggimage)
library(purrr)
library(gganimate)
library(plotly)

install.packages("ggpubr")

img <- png::readPNG("./62992631362__36FE81AB-92D6-498F-8EBE-CFB004872B91.jpg")

df2 <- 
  data.frame(species = c("Adelie", "Chinstrap", "Gentoo"),
                       body_mass_g=rep(2e05,3))
  
penguins %>%
  ggplot(aes(x=species, y = body_mass_g )) +
  geom_col(color="Red") +
  geom_image(data = df2, 
             aes(image="https://media.tenor.com/jUMex_rdqPwAAAAd/among-us-twerk.gif"),
             size=.15)

p <- penguins %>%
  ggplot(aes(x=flipper_length_mm, y=body_mass_g, color = species)) +
  geom_point()

penguins %>% names()

p +
  transition_time(year) +
  labs(title = 'Year:{frame_time}')

ggplotly(p) 

penguins$chonky <- penguins$body_mass_g > 5000

penguins$highlight <- NA
penguins$highlight[101] <- "blue"

penguins %>%
  ggplot(aes(x=flipper_length_mm,
    y=body_mass_g,
    color = chonky)) +
  geom_point() +
  geom_hline(yintercept = 5000, color="red",linetype=2) +
  annotate("text",x=190,y=5200,label="chonky") +
  labs(color ="CHONKY") +
  scale_color_manual(values = c("black","red")) +
  geom_point(data = penguins[101,color = "Orange"]) +
  theme(legend.title = element_text(angle=180))

#ggmap gives access to google maps


