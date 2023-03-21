#Tuesday 3/14
#can get data sets from links and websites
library(tidyverse)
library(curl)
library(easystats)
library(palmerpenguins)
library(modelr)

con <- curl("URL")
readLines(con)

names(penguins)
mod1 <- glm(data=penguins,
    formula = body_mass_g ~ sex + species + flipper_length_mm)
summary(mod1)

check_model(mod1)

#classification
#logistic regression
#needs to be a logical true false
#mutate adds a column row
#most of the time you must make true false columns to use this logistic regression. 
penguins <- penguins %>% 
  mutate(male = case_when(sex == "male" ~ TRUE, 
                          sex == "female" ~ FALSE)) 
names(penguins)

mod2 <- glm(data=penguins,
            formula = male ~ body_mass_g + flipper_length_mm + bill_length_mm + species, 
            family = "binomial")
summary(mod2)

add_predictions(penguins, mod2, type = "response") %>% 
  ggplot(aes(x=body_mass_g, y =pred, color = species)) +
          geom_smooth()

#little bit of machine learning

#building a glm for body_mass_g

mod <- glm(data=penguins,
           formula = body_mass_g ~ sex * species * island + flipper_length_mm + 
             bill_length_mm + bill_depth_mm)

df <- add_predictions(penguins, mod)

df <- add_residuals(df,mod)

performance(mod)

#make a new data frame:
new_penguin <- data.frame(sex="male",
                          species = "Gentoo",
                          island = "Biscoe",
                          bill_length_mm = 1000,
                          bill_depth_mm = 1000,
                          flipper_length_mm = 1)
add_predictions(new_penguin, model = mod)

df$body_mass_g %>%  max(na.rm = TRUE)

#testing data set gets set aside until the end
# training data set is for model building

penguins
set.seed(69)
testing <- sample(1:nrow(penguins),size = round(nrow(penguins)*.2))

test <- penguins[testing,]

train <- penguins[-testing,]

mod2 <- glm(data=train,
            formula = mod$formula)

add_residuals(test,mod2) %>% 
  pluck("resid") %>% .^2 %>% 
  mean(na.rm = TRUE) %>% 
  sqrt()

performance(mod2,newdata = test)
performance(mod)

rsq_mod2 <- c()
for( i in 1:100){
  testing <- sample(1:nrow(penguins),size = round(nrow(penguins)*.2))
  test <- penguins[testing,]
  train <- penguins[-testing,]
  mod2 <- glm(data=train,
              formula = mod$formula)
  rsq_mod2[i] <- rsquare(mod2,test)
}

data.frame(value=rsq_mod2) %>% 
  ggplot(aes(x=value)) +
  geom_density() +
  geom_vline(xintercept = mean(rsq_mod2), linetype=2,color ='red')
  

#tidy models is python based statistic 