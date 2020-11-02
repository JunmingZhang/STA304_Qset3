#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
# setwd("C:/Users/Sammi-Jo/Desktop/PS3")
setwd(".")
# raw_data <- read_dta("inputs/usa_00002.dta.gz")
raw_data <- read_dta("../STA304/now/question_set/Qset3/raw_data/usa_00001.dta")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data %>% 
  select(region,
         stateicp,
         sex, 
         age, 
         race, 
         hispan,
         marst,
         bpl,
         citizen,
         educd,
         labforce,
         inctot,
         perwt)
rm(raw_data)
         

#### What's next? ####

## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)
head(reduced_data)

# reduced_data <- 
#   reduced_data %>%
#   count(age) %>%
#   group_by(age) 
# 
# reduced_data <- 
#   reduced_data %>% 
#   filter(age != "less than 1 year old") %>%
#   filter(age != "90 (90+ in 1980 and 1990)")

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

# rename columns with the same content
reduced_data<-rename(reduced_data, education=educd)
reduced_data<-rename(reduced_data, state=stateicp)

# choose those can vote, and those who can vote (citation 15)
reduced_data <-
  reduced_data %>% 
  filter(age >= 18 &
           citizen != "n/a" &
           citizen != "not a citizen")

# unify race of two datasets
reduced_data <-
  reduced_data %>% mutate(race = case_when(
    race=="other race, nec" |
      race=="two major races" |
      race=="three or more major races"~"other",
    race=="black/african american/negro"~"black",
    race=="chinese" |
      race=="japanese" |
      race=="other asian or pacific islander"~ "asian or pacific",
    race=="american indian or alaska native" ~ "american indian or alaska native",
    race=="white" ~ "white"
  ))

# unify the education of two datasets
edu_atmost_high = c("1 or more years of college credit, no degree",
                    "12th grade, no diploma", "regular high school diploma",
                    "no schooling completed", "grade 10", "grade 11",
                    "ged or alternative credential", "grade 9", "grade 8",
                    "grade 7", "grade 6", "grade 5", "grade 4", "grade 3",
                    "grade 2", "grade 1", "kindergarten")
edu_bachelor = c("bachelor's degree", "some college, but less than 1 year")
edu_grad = c("doctoral degree", "master's degree")
edu_other_tertiary = c("associate's degree, type not specified",
                       "professional degree beyond a bachelor's degree",
                       "nursery school, preschool")

# unify education of multiple datasets
reduced_data<-
  reduced_data %>% mutate(
    education=case_when(
      education %in% edu_atmost_high ~ "at most high school",
      education %in% edu_bachelor ~ "bachelor",
      education %in% edu_grad ~ "graduate",
      education %in% edu_other_tertiary ~ "tertiary (not bachelor)"
    )
  )

# unify the state by the state code
reduced_data<-
  reduced_data %>% mutate(
    state = case_when(
      state=="alabama" ~ "AL",
      state=="alaska" ~ "AK",
      state=="arizona" ~ "AZ",
      state=="arkansas" ~ "AR",
      state=="california" ~ "CA",
      state=="colorado" ~ "CO",
      state=="connecticut" ~ "CT",
      state=="delaware" ~ "DE",
      state=="district of columbia" ~ "DC",
      state=="florida" ~ "FL",
      state=="georgia" ~ "GA",
      state=="hawaii" ~ "HI",
      state=="idaho" ~ "ID",
      state=="illinois" ~ "IL",
      state=="indiana" ~ "IN",
      state=="iowa" ~ "IA",
      state=="kansas" ~ "KS",
      state=="kentucky" ~ "KY",
      state=="louisiana" ~ "LA",
      state=="maine" ~ "ME",
      state=="maryland" ~ "MD",
      state=="massachusetts" ~ "MA",
      state=="michigan" ~ "MI",
      state=="minnesota" ~ "MN",
      state=="mississippi" ~ "MS",
      state=="missouri" ~ "MO",
      state=="montana" ~ "MT",
      state=="nebraska" ~ "NE",
      state=="nevada" ~ "NV",
      state=="new hampshire" ~ "NH",
      state=="new jersey" ~ "NJ",
      state=="new mexico" ~ "NM",
      state=="new york"~ "NY",
      state=="north carolina" ~ "NC",
      state=="north dakota" ~ "ND",
      state=="ohio" ~ "OH",
      state=="oklahoma" ~ "OK",
      state=="oregon" ~ "OR",
      state=="pennsylvania" ~ "PA",
      state=="rhode island" ~ "RI",
      state=="south carolina" ~ "SC",
      state=="south dakota" ~ "SD",
      state=="tennessee" ~ "TN",
      state=="texas" ~ "TX",
      state=="utah" ~ "UT",
      state=="vermont" ~ "VT",
      state=="virginia" ~ "VA",
      state=="washington" ~ "WA",
      state=="west virginia" ~ "WV",
      state=="wisconsin" ~ "WI",
      state=="wyoming" ~ "WY"
    )
  )

# del all NA if there is
reduced_data <- na.omit(reduced_data)

# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data, "outputs/census_data.csv")



         