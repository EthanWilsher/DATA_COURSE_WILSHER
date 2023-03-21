# t test compares the mean of x and y
#t.test(heightsofmale, heightsoffemale) 
#p value < .05 is "significant" strong enough evidence to reject the null.
# if you have a large p value you fail to reject the null smaller p value reject the null.
#2 kinds of errors to make
#   type 1 : error rate = 5% failing to detect reality 
#aov() analysis of variance for stats class
#corr.test() correlation test for stats class
#WE WILL USE glm() general linear model.
#glm(data = df,
# ~ = is a function of with y as the response and x as the predictor.
 #   formula = y or height ~ x or sex) 
#this function will give out estamed(coefficient) and a p value. 
# will make sexM = 52 (what number to add or takeaway from the intercept or what computer thinks
#all humans are)

library(tidyverse)
mpg %>% View

# response variable : cty

names(mpg)

mpg$class %>%  unique()

residuals(model)^2 %>% mean() %>% sqrt()

#residual mean standard error RMSE

mpg %>% 
  ggplot(aes(x = displ, y = cty, color = drv)) +
  geom_point() + geom_smooth(method = "glm") +
  facet_wrap(~cyl)

model <- glm(data=mpg,
    formula = cty ~ displ + cyl + drv)
summary(model)
# y = mx + b slope formula
#estimating using new data we make.
df <- data.frame(displ = 1:10, 
                 cyl = 5,
                 drv = "f")
predict(model, newdata = df)

#WEEK 8 PART 2
#linear regression models can have 2 continues variables or can have a binary feature. 

library(easystats)

mpg %>% names

mod1 <- glm(data = mpg,
            formula = cty ~ class)
mpg %>% 
  filter(class == "compact") %>% 
  pluck("cty") %>% 
  mean()

summary(mod1)

mod2 <- glm(data = mpg,
            formula = cty ~ class + displ)
summary(mod2)

mod3 <- glm(data = mpg,
            formula = cty ~ class * displ)
summary(mod3)

mod4 <- glm(data = mpg,
            formula = cty ~ class * displ * cyl)
summary(mod4)

mod5 <- glm(data=mpg,
            formula = cty ~ class * displ * cyl * drv)
summary(mod5)

rmse(mod1)
rmse(mod5)
compare_performance(mod1,mod2,mod3,mod4,mod5) %>% plot()
mpg$class %>% unique

library(MASS)
step <- MASS::stepAIC(mod5) #magic gives you the best formula
step$formula

mod6 <- glm(data=mpg,
            formula = step$formula)

compare_performance(mod1,mod2,mod3,mod4,mod5,mod6) %>% plot()

check_model(mod6)

mpg %>% 
  ggplot(aes(x=displ, y = cty, color = factor(cyl))) +
  geom_smooth(method = "glm")

