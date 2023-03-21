# playing with ggplot 
library(tidyverse)
#install.packages("palmerpenguins")
library(palmerpenguins)
#install.packages("GGally")
library(GGally)
install.packages("ggimage")
install.packages("skimr")

penguins %>% names()

#find your outcome variables

#        flipper_length_mm

#what predictors variable?
#         island, species, sex, year

penguins$year %>% unique()

ggpairs(penguins)

?ggplot

penguins %>%
  ggplot(aes(x=island,
             y=flipper_length_mm,
             fill=species)) + #set 'global' mapping # which is variable
  geom_violin()

penguins$sex %>% unique()
p <- 
  penguins[!is.na(penguins$sex),] %>%
  ggplot(aes(x=flipper_length_mm, fill=species)) + 
  geom_density(alpha=.5) + 
  facet_wrap(~sex*island )
class(p)
# data
#how to map variables (column Names) to parts of a plot
# what to draw?
#facets?
#scales?
#customize theme

p +
  theme_bw()

penguins[!is.na(penguins$sex),] %>% 
  ggplot(aes(x=sex, y=body_mass_g, fill= species)) +
  geom_violin() +
  geom_smooth(method ="lm",se=FALSE) +
  facet_wrap(~species)+
  theme_void()

p <- penguins %>%
  ggplot(aes(x=bill_length_mm,
             y=body_mass_g,
             #size=flipper_length_mm,
             color=species)) +
  geom_point(alpha = .25) +
  #geom_line(size=1,aes(linetype=sex),color="black")
  geom_smooth(method="lm",se=FALSE)

p +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold.italic")) +
  labs(x="Bill Length (mm)", 
       y= "Body Mass (g)",
       color= "Species",
       title = "Penguin Body Mass",
       subtitle = "thiccems",
       caption = "Data from palmer penguins R package") +
  scale_color_manual(values = c("#fccd49", "#8d42f5", "#c24217"))

#make nifty penguin plot
p2 <- penguins %>% 
  ggplot(aes(x=sex, y=body_mass_g, fill = species)) + 
  geom_boxplot() +
  labs(x="SEX!",
       y="Bodyodyodyody mASS (g)",
       fill = "Species") +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 0,
                                   vjust = .5,
                                   face = "italic"),
          axis.text.y = element_text(size=69,
                                     face="bold",
                                     angle = 36,
                                     color = "lightblue"),
        plot.background = element_rect(fill = "lightpink",
                                       color = "lightyellow",
                                       linewidth = 15),
        panel.grid.major.x = element_line(color = "darkgreen",
                                          linetype = 4,
                                          lineend = 'round',
                                          linewidth = 2),
        strip.background = element_rect(fill = "#af90f5", color = "#f590f3"))

ggsave(p2, filename ="uglyplot.png", height =6, width = 6)

p2 +
  scale_y_log10()+
  coord_polar() +
  theme(strip.text = element_text(face ='bold' , size =12),
        legend.text = element_text(face = 'italic'), 
        legend.background = element_rect(fill = "darkred"),
        legend.position = "bottom")

library(ggimage)

penguins %>%
  ggplot(aes(x=body_mass_g,y=flipper_length_mm, image = "jigglupuff")) +
  geom_pokemon()


#WEEK 5

df <- penguins[!is.na(penguins$body_mass_g),]
adelie <- penguins[penguins$species == "Adelie",]
Chinstrap <- penguins[penguins$species == "Chinstrap",]
Gentoo <- penguins[penguins$species == "Gentoo",]

adelie$body_mass_g %>% mean(na.rm=TRUE)
Chinstrap$body_mass_g %>% mean(na.rm=TRUE)
Gentoo$body_mass_g %>% mean(na.rm=TRUE)

means <- numeric()

for(i in unique(penguins$species) %>% as.character()){
  ss <- penguins[penguins$species == i,]
  means[i] <- ss$body_mass_g %>% mean(na.rm=TRUE)
}

means

#dplyr verbs:
#group_by() / summarize()
#arrange
#filter (filters rows!)

#filter penguins where sex is not NA and where penguin body mass is greater than 4000

penguins %>%
  filter(!is.na(sex), body_mass_g > 4000, year == 2007)

penguins$sex %>% unique()

skimr::skim(penguins)
test <- 
penguins %>%
  filter(!is.na(sex)) %>%
  group_by(species,sex) %>%
  summarize(GeneralThiccness = mean(body_mass_g,na.rm=TRUE),
            ThiccestLad = max(body_mass_g, na.rm=TRUE)) %>%
  arrange(desc(ThiccestLad)) 

test %>% ggplot(aes(x=species,
             y=ThiccestLad,
             fill = sex)) +
  geom_col(position = "dodge")

#overwrite levels of a factor with new order
test$species <-  
factor(test$species, levels = c("Gentoo","Chinstrap","Adelie"))






