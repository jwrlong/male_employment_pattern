#American Institute for Boys and Men - Employment in California State Government


#___________________________SET UP FOR ANALYSIS_____________________________

#Loading library applications
library(tidyverse)
library(stats)

#Uploading cvs onto R software
employ <- read_csv("calhr_5102_statewide_2011-2021.csv")

#Renaming Relevant Variables for Analyses
employ <- employ%>%
  rename (date = "As Of Date",
          employ_category = "Employee Category",
          employ_subcategory = "Sub Category",
          gender = "Gender",
          race = "Identity Variable",
          count = "Record Count",
          title = "Class Title")

#Unique categories within each Variable
unique(employ$date)
unique(employ$race)
unique(employ$employ_category)
unique(employ$employ_subcategory)

#Transforming categorical variables for analyses (i.e., new race variable, new gender variable)
employ <- employ%>%
  mutate (nrace = case_when(grepl("Asian", race)~"Asian", grepl("Pacific Islander", race) ~ "Pacific Islander", TRUE ~ race))

unique(employ$nrace)

#Creating separate data sets for women and men
male_employ <- employ%>%
  filter(gender == "Male")

female_employ <- employ%>%
  filter(gender == "Female")

#____________________________DESCRIPTIVE STATISTICS_____________________________

# Number of Employees separated by Employment Category, Gender and Date
employ_total <- employ%>%
  group_by(employ_category, gender, date)%>%
  summarise(total_count = sum (count))%>%
  print (n = 48)

employ2021 <- employ%>%
  filter( date == "2021-12-31")%>%
  group_by(employ_category, gender)%>%
  summarise(total_count = sum (count))%>%
  print (n = 48)

write.csv(employ_total, "employ_total.csv")
write.csv(employ2021, "employ2021.csv")

# Number of Male employees within HEALTHCARE by race

malerace_employ2021 <- male_employ%>%
  filter(employ_category == "Healthcare Practitioners and Technical Occupations", date == "2021-12-31")%>%
  group_by(nrace)%>%
  summarise (total_count = sum (count))

write.csv(malerace_employ2021, "malerace_employ2021.csv")
  
#____________________________INFERENTIAL STATISTICS_____________________________

#Conducting a Negative Binomial Regression Analyses to determine whether the number of men in Healthcare Practitioners and Technical Occupations differed between 2011 and 2021

HEALTH <- employ%>%
  filter (employ_category == "Healthcare Practitioners and Technical Occupations", date == "2021-12-31" | date == "2011-12-01")

mean(HEALTH$count)
var(HEALTH$count) #mean<variance, this indicates a negative binomial regression analysis is the appropriate test

install.packages("MASS")
library(MASS)

negative_binomial_model <- glm.nb(count ~ gender + date, data = HEALTH)
summary(negative_binomial_model)
#p = .916

#Attempt to conduct a Negative Binomial Regression Analyses to determine whether the number of men in Healthcare Practitioners and Technical Occupations differed across race

RACE <- male_employ%>%
  filter (employ_category == "Healthcare Practitioners and Technical Occupations", date == "2021-12-31")

mean(RACE$count)
var(RACE$count) # mean < variance, this indicates a negative binomial regression analysis is the appropriate test

negative_binomial_model1 <- glm.nb(count ~ gender + nrace, data = RACE)
summary(negative_binomial_model1)

unique(male_employ$nrace)

#_______________________Other statistics I ran but did not include__________________

# Men and women in a stem feild across the years
male_employ%>%
  filter(category_employ == "Computer and Mathematical Occupations")%>%
  group_by(date)%>%
  summarise(total_count = sum (count))%>%
  arrange(desc(date))

female_employ%>%
  filter(category_employ == "Computer and Mathematical Occupations")%>%
  group_by(date)%>%
  summarise(total_count = sum (count))%>%
  arrange(desc(date))

#Comparing men and women in an educational field across the years
male_employ%>%
  filter(category_employ == "Education, Training, and Library Occupations")%>%
  group_by(date)%>%
  summarise(total_count = sum (count))%>%
  arrange(desc(date))

female_employ%>%
  filter(category_employ == "Education, Training, and Library Occupations")%>%
  group_by(date)%>%
  summarise(total_count = sum (count))%>%
  arrange(desc(date))

# Number of employees by gender across specific employ_subcategory
employ%>%
  filter (employ_subcategory == "Counselors")%>%
  group_by(gender, date)%>%
  summarise(totalcount = sum(count))%>%
  arrange(desc(date))%>%
  print(n = 22)

employ%>%
  filter (employ_subcategory == "Registered Nurses")%>%
  group_by(gender, date)%>%
  summarise(totalcount = sum(count))%>%
  arrange(desc(date))%>%
  print(n = 22)

employ%>%
  filter (employ_subcategory == "Human Resources Workers")%>%
  group_by(gender, year)%>%
  summarise(totalcount = sum(count))%>%
  arrange(desc(year))%>%
  print(n = 22)

employ%>%
  filter (employ_subcategory == "Human Resources Workers")%>%
  group_by(gender, year)%>%
  summarise(totalcount = sum(count))%>%
  arrange(desc(year))%>%
  print(n = 22)