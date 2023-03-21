#load packages
library(tidyverse)
library(readxl)

#look at example data
table1
table2
table3 #has two seperate variable in a column "rate"
table4a #Has two seperate pages and uses multiple years as columns.
table4b
table5 #has years split by century and year.

#tidy rule 1: every variable is a single column
# no column has more than 1 variable
#tidy rule 2: every row gets it's own observation
#   always rectangular (data sheet is squarelike)
#(Don't have 2 variables in a single column)

#tidyr::
#pivot wider is for when multiple variables are in a single column
table2 %>%
  pivot_wider(names_from = type,
              values_from = count)
#pivot longer is for when single variable is in multiple columns
table4a %>%
  pivot_longer(cols = -country, names_to = "year",
               values_to = "cases")
table4b %>%
  pivot_longer(cols = -country, names_to = "year",
               values_to = "population")
table3 %>% 
  separate(col = rate,
           into = c("cases", "population",),
           sep = "/",
           convert = TRUE) 
# it normally just searches for non alphanumeric and splits there but can 
#be specified

table5 %>%
  mutate(newyear = paste0(century,year) %>% as.numeric()) %>%
  select(-c(century, year)) %>%
  separate(rate, into = c("cases", "population"), convert = TRUE) %>%
  select(country,newyear,cases,population)

rent <- read.csv("./Data/wide_income_rent.csv") %>%
  pivot_longer(cols = -variable,names_to = "States") %>%
  pivot_wider(names_from = variable, values_from = value)

clean <- rent %>%
  pivot_longer(cols = -variable,names_to = "States") %>%
  pivot_wider(names_from = variable, values_from = value)

#save file path
chem <- "./exp_3_excel.xlsx"

#read in each seperate run
run1 <- readxl::read_xlsx(chem, range = "A8:B573", col_names = FALSE)
run2 <- readxl::read_xlsx(chem, range ="D8:E846", col_names = FALSE)

#make meaningful colum names
names(run1) <- c()
names(run2) <- c("time_s", "temp_c")

#add third column designating which run it came from

run1$run <- "run_1"
run2$run <- "run_2"

#combine the two runs 
df <- full_join(run1, run2)

#can save cleaned version as csv if wanted
write_csv(df,"./Data/exp_3_excel_cleaned.csv")

#control shift m is %>%

df %>% 
  ggplot(aes(x = time_s, y = temp_c, color = run)) +
  geom_point() + facet_wrap(countr)






