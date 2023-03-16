library(tidyverse)
library(easystats)
library(modelr)
library(caret)
# 1. loads the “/Data/mushroom_growth.csv” data set
mush <- read_csv("../../Data/mushroom_growth.csv")
names(mush)

# 2. creates several plots exploring relationships between the response and predictors
mush %>% ggplot(aes(x= Nitrogen, y = GrowthRate, color = Species)) +
  geom_smooth()

mush %>% ggplot(aes(x= Temperature, y = GrowthRate, color = Species)) +
  geom_smooth()

mush %>% ggplot(aes(x= Light, y = GrowthRate, color = Species)) +
  geom_smooth()

# 3. defines at least 4 models that explain the dependent variable “GrowthRate”
mod1 <- glm(data=mush,
            formula = GrowthRate ~ Species)
summary(mod1)
mod2 <- glm(data=mush,
            formula = GrowthRate ~ Species * Nitrogen) 
summary(mod2)
mod3 <- glm(data=mush,
            formula = GrowthRate ~ Species * Humidity * Temperature)
summary(mod3)
mod4 <- glm(data=mush,
            formula = GrowthRate ~ Species * Light * Temperature)
summary(mod4)

mod5 <- glm(data=mush,
            formula = GrowthRate ~ Species + Light + Temperature + Nitrogen + Humidity)
summary(mod5)

# 4. calculates the mean sq. error of each model

residuals(mod1)^2 %>% mean()

residuals(mod2)^2 %>% mean()

residuals(mod3)^2 %>% mean()

residuals(mod4)^2 %>% mean()

residuals(mod5)^2 %>% mean()
# 5. selects the best model you tried

compare_performance(mod1,mod2,mod3,mod4,mod5) %>% plot()

compare_performance(mod1,mod2,mod3,mod4,mod5) 

# 6. adds predictions based on new hypothetical values 
# for the independent variables used in your model
df <- data.frame(Nitrogen = 1:216, 
                 GrowthRate = 1:216 ,
                 Species = "P.ostreotus",
                 Light = 1:216,
                 Temperature = 1:216,
                 Humidity = "Low")

predict(mod5, newdata = df) 

# 7. plots these predictions alongside the real data

plot(x=predict(mod5), y= mush$GrowthRate, 
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values') + abline(a=0, b=1)
