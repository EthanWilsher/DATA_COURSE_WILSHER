library(tidyverse)
library(janitor)
library(readxl)
janitor::clean_names()
df <- read_csv("./Data/Bird_Measurements.csv") %>% 
  clean_names()



df %>% 
  select(-ends_with("_n")) 

male <- df %>% 
  select(starts_with("m_"),
         species_number, species_name,
         english_name, clutch_size,
         egg_mass, mating_system, family) %>% 
  mutate(sex ="male")
female <- df %>% 
  select(starts_with("f_"),
         species_number, species_name,
         english_name, clutch_size,
         egg_mass, mating_system, family) %>% 
  mutate(sex = "female")
unsexed <- df %>% 
  select(starts_with("unsexed_"),
         species_number, species_name,
         english_name, clutch_size,
         egg_mass, mating_system, family) %>% 
  mutate(sex ="unsexed")

#CLEAN UP NAMES
names(male) <-
names(male) %>% 
  str_remove("^m_")

names(female) <-
names(female) %>% 
  str_remove("^f_")

names(unsexed) <- 
  names(unsexed) %>% 
  str_remove("^unsexed_")

#join them back together
clean <- male %>% 
  full_join(female) %>% 
  full_join(unsexed) %>%
  mutate(family = as.factor(family),
         mating_system = as.factor(mating_system),
         species_number = as.factor())

library(skimr)
df <- skim(clean) %>% 
  as.data.frame()

complete_enough() <- 
  df %>% 
  filter(complete_rate > .25)
complete_enough$skim_variable
clean %>% 
  select(complete_enough$skim_variable)

df2 <- read_csv("./Data/FacultySalaries_1995.csv")

#df2 %>% 
 # select(-contains("comp")) %>% 

#Week 7 Thursday

messy <- "./Data/messy_bp.xlsx"

messytest <- readxl::read_xlsx(messy, skip = 3, col_names = TRUE ) %>% clean_names()
bloodpressure <- messytest %>% select(starts_with("bp")) 

visit1 <- messytest %>% 
  select(pat_id,da) %>% mutate(visit=1)



names(messytest)

#str_remove("")


messytest <- messytest %>% 
  separate(col = bp_8, 
           into = c("Systolic 1", "Diosystolic 1"),
           sep = "/",
           convert = TRUE)
messytest <- messytest %>% 
  separate(col = bp_10, 
           into = c("Systolic 2", "Diosystolic 2"),
           sep = "/",
           convert = TRUE)
messytest <- messytest %>% 
  separate(col = bp_12, 
           into = c("Systolic 3", "Diosystolic 3"),
           sep = "/",
           convert = TRUE)

#TEACHER SOLUTION OF CLEAN

#CHECK THE VIDEO

#messy data set google data and clean it.

#glm(data = ) doing modeling







