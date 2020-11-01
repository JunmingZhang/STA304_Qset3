#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# setwd("C:/Users/Sammi-Jo/Desktop/PS3")
setwd(".")
# Read in the raw data (You might need to change this if you use a different dataset)
# raw_data <- read_dta("inputs/ns20200625/ns20200625.dta")
raw_data <- read_dta("../STA304/now/question_set/Qset3/raw_data/ns20200625/ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(interest,
         registration,
         vote_2016,
         vote_intention,
         vote_2020,
         ideo5,
         employment,
         foreign_born,
         gender,
         census_region,
         hispanic,
         race_ethnicity,
         household_income,
         education,
         state,
         congress_district,
         age)


#### What else???? ####
# Maybe make some age-groups?
# Maybe check the values?
# Is vote a binary? If not, what are you going to do?

reduced_data$inctot <- as.integer(reduced_data$inctot)
reduced_data$perwt <- as.numeric(reduced_data$perwt)

# create age category
reduced_data$age <- as.integer(reduced_data$age)
reduced_data = reduced_data %>%
  mutate(age_group = case_when (
    age >= 18 & age < 20 ~ '18 ~ 20',
    age >= 20 & age < 30 ~ '20 ~ 30',
    age >= 30 & age < 40 ~ '30 ~ 40',
    age >= 40 & age < 50 ~ '40 ~ 50',
    age >= 50 & age < 60 ~ '50 ~ 60',
    age >= 60 & age < 70 ~ '60 ~ 70',
    age >= 70 ~ '>= 70'
  ))

reduced_data<-
  reduced_data %>%
  mutate(vote_trump = 
           ifelse(vote_2020=="Donald Trump", 1, 0))

# focus those who can and will vote and those who support Democrates
# and Republicans only
reduced_data<-
  reduced_data %>%
  filter(registration=="Registered" &
           vote_intention=="Yes, I will vote" &
           age >= 18 &
           (vote_2016=="Donald Trump" |
            vote_2016=="Hillary Clinton") &
           (vote_2020=="Donald Trump" |
           vote_2020=="Joe Biden"))

# change all vairables to lowercase
reduced_data$hispanic = tolower(reduced_data$hispanic)
reduced_data$gender = tolower(reduced_data$gender)

# rename columns with common contents
reduced_data<-rename(reduced_data, sex=gender)
reduced_data<-rename(reduced_data, hispan=hispanic)
reduced_data<-rename(reduced_data, race=race_ethnicity)

# focus on Hispanic group since there are more and more Hispanic people born in USA
reduced_data<-reduced_data %>% 
  mutate(hispan = ifelse(hispan=="not hispanic" |
                           hispan=="mexican" |
                           hispan=="cuban" |
                           hispan=="puerto rican", hispan, "other"))

# unify the race of two datasets
reduced_data$race = tolower(reduced_data$race)
lst_asian_pacific = c("asian (chinese)", "asian (japanese)", "asian (filipino)",
              "asian (vietnamese)", "asian (korean)", "asian (asian indian)",
              "asian (other)", "pacific islander (guamanian)", "pacific islander (native hawaiian)",
              "pacific islander (other)", "pacific islander (samoan)")
reduced_data<-reduced_data %>%
  mutate(race=case_when(
    race == "black, or african american"~"black",
    race %in% lst_asian_pacific ~ "asian or pacific",
    race == "some other race"~"other",
    race == "american indian or alaska native" ~ "american indian or alaska native",
    race == "white" ~ "white"
  ))

# unify education degree
reduced_data <-
  reduced_data %>%
  mutate(education=case_when(
    education=="Completed some college, but no degree" |
      education=="College Degree (such as B.A., B.S.)" ~ "bachelor",
    education=="Other post high school vocational training" |
      education=="Associate Degree"~ "tertiary (not bachelor)",
    education=="Doctorate degree" |
      education=="Masters degree" |
      education=="Completed some graduate, but no degree" ~ "graduate",
    education=="High school graduate" |
      education=="Completed some high school"~"at most high school"
  ))

# del all NA if there is
reduced_data <- na.omit(reduced_data)

# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_data, "outputs/survey_data.csv")

