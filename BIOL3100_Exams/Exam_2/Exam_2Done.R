install.packages("tidyverse")
install.packages("janitor")
install.packages("easystats")
install.packages("modelr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggpubr")
library(tidyverse)
library(janitor)
library(easystats)
library(modelr)
library(ggplot2)
library(dplyr)
library(ggpubr)
#1. Read in the unicef data 

data <- read_csv("./unicef-u5mr.csv") %>% clean_names()

names(data) <- names(data) %>% 
  str_remove("u5mr_") 

#2. Get it into tidy format

clean <- data %>% 
  pivot_longer(cols = -c(country_name,continent,region), names_to = "years",
               values_to = "u5mr") %>% drop_na()
clean$years = as.numeric(unlist(clean$years))
clean$u5mr = as.double(unlist(clean$u5mr))

typeof(clean$u5mr)
typeof(clean$years)

#3. Plot each country’s U5MR over time, #4. Save this plot as LASTNAME_Plot_1.png 

png(file="./WILSHER_Plot_1.png",
    width=600, height=350)

clean %>% 
  ggplot(aes(x = years, y = u5mr, group = country_name)) + 
  geom_line() + 
  facet_wrap(vars(continent))

dev.off()

#5. Create another plot that shows the mean U5MR for all the
#countries within a given continent at each year
#6. Save that plot as LASTNAME_Plot_2.png

png(file="./WILSHER_Plot_2.png",
    width=600, height=350)

meanplot <- clean %>% group_by(years,continent) %>% dplyr::summarise(mean_u5mr = mean(u5mr))

meanplot %>% 
  ggplot(aes(x = years, y =  mean_u5mr, color = continent)) + 
  geom_line(size = 2)

dev.off()

#7. Create three models of U5MR 
names(clean)
mod1 <- glm(data=clean,
            formula = u5mr ~ years)
summary(mod1)
mod2 <- glm(data=clean,
            formula = u5mr ~ years + continent)
summary(mod2)
mod3 <- glm(data=clean,
            formula = u5mr ~ years * continent)
summary(mod3)

#8. Compare the three models with respect to their performance
compare_performance(mod1,mod2,mod3)
compare_performance(mod1,mod2,mod3) %>% plot()
#I think that the best model is model 3 because it includes both the trend for
#year and country and how they interact so instead of a flat rate it checks the interaction.


#9. Plot the 3 models’ predictions like so:

df1 <- add_predictions(clean, mod1) 

pred1 <- df1 %>% 
  ggplot(aes(x = years, y = pred, color = continent)) +
           geom_line() + ylim(0,250)

df2 <- add_predictions(clean, mod2)

pred2 <- df2 %>% 
  ggplot(aes(x = years, y = pred, color = continent)) + 
  geom_line() + ylim(0,250)

df3 <- add_predictions(clean, mod3)

pred3 <- df3 %>% 
  ggplot(aes(x = years, y = pred, color = continent)) + 
  geom_line() + ylim(0,250)

text <- "Model predictions"

tgrob <- text_grob(text,size = 20)

plot0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,3,0,0, "cm"))

ggarrange(
  plot0,NULL,NULL,pred1,pred2,pred3, labels = c("mod1","mod2","mod3") , align = "hv",
  common.legend = TRUE, legend = "right", ncol = 3, nrow = 2, heights = c(1,4),
  hjust = -1.75, vjust = 6
)


#10. Bonus Using your preferred model, predict what the U5MR would be 
#for Ecuador in the year 2020.
names(clean)

new_ecuador <- data.frame(country_name="Ecuador",
                          continent = "Americas",
                          region = "South America",
                          years = (1955:2020))

ecuadorpred1 <- add_predictions(new_ecuador, model = mod3)

ecuadorpred2 <- add_predictions(new_ecuador, model = mod1)

#model 1 actually turned out to be the best and gave a number of 11.44 close.

ecuadorpred2 <- add_residuals(
  filter(clean, country_name == "Ecuador"), 
  mod1, var = "resid")




