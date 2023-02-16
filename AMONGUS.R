library(tidyverse)
library(ggimage)
library(purrr)
library(gganimate)
library(plotly)
library(GGally)
library(ggpubr)
library(magick)
library(ggplot2)


susbooty<- data.frame(x = rnorm(10),
                      y = rnorm(10),
                      images = sample("./susbooty.png"))
susface <- png::readPNG("./susface.png")
rocksus <- png::readPNG("./rocksus.png")
redsus <- png::readPNG("./amongusred.png")

Sussy <- read.csv("./AMONGUS.csv")


  ggplot(data = Sussy, 
         aes(x = SUS, y = VENTED, color = AMOGUS)) +
  annotation_custom(grid::rasterGrob(image = susface,
                                     height = 1,
                                     width = 1)) +
  geom_point() +
  labs(title = "OMG IT WAS RED HE SO SUS", ) +
  theme(axis.title.x = element_text(face = "bold",
                                    size = 100,
                                    color = "red", 
                                   
                                    angle = 180),
        axis.ticks = element_line(color = "black", 
                                           linetype = "blank",
                                           linewidth = 20,
                                           size = 99),
        axis.title.y.left = element_text(face = "italic",
                                         color = "#5c4016",
                                         size = 69,
                                         hjust = 1,
                                         angle = 95),
        legend.background = element_rect(fill = "green"),
        legend.title = element_text(color = "yellow",
                                    size = 69),
        plot.background = element_rect(fill = "#b3fc08" ,
                                       colour = "yellow",
                                       linewidth = 10,
                                      ),
        legend.text = element_text(size = 69,
                                   color = "#f037fa"),
        title = element_text(color = "#fa9805",
                             size = 18))
  
?element_text
  