#ok starting assignment reading in the data
library(tidyverse)
library(janitor)
library(ggplot2)

df <- read_csv("Utah_Religions_by_County.csv") %>% clean_names()
names(df)

#looking at the data frame it is a lot of names with values that can be made longer and clean
#on here just making the religions longer also added clean names
clean <- df %>% 
  pivot_longer(cols = -c(county , pop_2010, religious, non_religious), 
               names_to = "religion",
               values_to = "pct_members_by_county")

#ok looks clean enough lets see what we can find with it.
#ok what to look for in this data set. maybe check what county has the highest population

clean %>% 
  ggplot(aes(x = pop_2010, y = reorder(county, +pop_2010))) +
  geom_col() 


#see what the largest religion is without lds

filter(clean, religion != "lds" ) %>% 
  ggplot(aes(x = pct_members_by_county, 
             y = reorder(religion, +pct_members_by_county))) +
  geom_col() 

#this plot is really bad but I found it funny how it made the graph rainbow
filter(clean, religion != "lds" ) %>% 
  ggplot(aes(x = pct_members_by_county, 
             y = reorder(religion, +pct_members_by_county), 
             fill = county)) +
  geom_col() 
#see what counties have the lowest rate of lds and see what the next biggest religion is

filter(clean, religion != "lds" ) %>% 
  ggplot(aes(x = reorder(county, -pop_2010), y = pop_2010 , fill = religion)) +
  geom_col() + theme(axis.text.x = element_text(angle = 90))

#Address the questions:

#“Does population of a county correlate with the proportion of 
#any specific religious group in that county?”
#ok this was tough correlating these three but mostly finding the right plot
#& figuring how to do correlation of population and proportion and county. 
# ok so my thoughts are population and county then have the percentage of members with color showing religion.

clean %>% 
  ggplot(aes(x = reorder(county, -pop_2010),
             y = pct_members_by_county, fill = religion)) +
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90)) 
#there seems to be no correlation when it comes to population size and religious group

#“Does proportion of any specific religion in a given
#county correlate with the proportion of non-religious people?”

#im just gonna use the same graph I made above but change the decending number being the 
#percent of nonreligious people meaning the left most has the highest number of non-religious.

clean %>% 
  ggplot(aes(x = reorder(county, -non_religious),
             y = pct_members_by_county, fill = religion)) +
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90)) 

#from what it seems there is a correlation. 
# the less nonreligious people in a country the more likely that the area will be less diverse
#and that most of the people will be lds with the exception of carbon and tooele counties. 



