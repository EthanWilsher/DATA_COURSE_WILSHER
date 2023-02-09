
#Task number 1 Reading data into an R data frame

covData <- read.csv("./cleaned_covid_data.csv")

#Task number 2 Subset the data to just A states
# and make object A_states


A_states <-
  covData[grepl("^A", covData$Province_State, ignore.case = TRUE),]

#Task 3:Create a plot for deaths over time with seperate
#facet for each state

ggplot(A_states, aes(x=Last_Update, y=Deaths, colour = Province_State)) +
  geom_point() +
  geom_smooth(stat= "smooth",position ="identity", se = FALSE) +
  facet_wrap(~ Province_State)

#Task 4:Find peak of Case_Fatality_Ratio for each state
#make new data set



state_max_fatality_rate <- 
  covData %>%
  group_by(Province_State) %>%
  summarise(
    Province_State = unique(Province_State, na.rm = TRUE),
    Maximum_Fatality_Rate = max(Case_Fatality_Ratio, na.rm = TRUE)) %>% 
  arrange(desc(Maximum_Fatality_Rate))


#Task 5: Create plot from state_max_fatality_rate

state_max_fatality_rate$Province_State <-
  factor(state_max_fatality_rate$Province_State,
         levels = c(state_max_fatality_rate$Province_State))


state_max_fatality_rate %>%
  ggplot(aes(x=Province_State, y=Maximum_Fatality_Rate)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90))

#Task 6: Plot cumulative deaths for US over time

# cumuDeath2 <-
#   covData %>%
#   group_by(Last_Update) %>%
#   summarize(Cumulative_Deaths = Deaths) %>%
#   inner_join(,)

cumuDeath <- aggregate(Deaths~Last_Update,covData,cumsum)

cumuDeath$Deaths <- factor(cumuDeath$Deaths,
                           levels = c(cumuDeath$Deaths))



cumuDeath %>%
  ggplot(aes(x = Last_Update , y = Deaths)) +
  geom_point()

  









  



