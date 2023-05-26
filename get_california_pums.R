# set directory
setwd("~/Desktop/")

# importing libraries
library(tidyverse)
library(tidycensus)

### Note 3/28/23 - don't use 2021 data bc of July 2021 ruling
####### Ruling says no new DACA apps :-(

###############################################################################
##### GETTING THE WHOLE STATE OF CALIFORNIA PUMS #####
##############################################################################
# view pums variables
View(pums_variables)

### GETTING VARIABLES OF INTEREST ###
selected_vars <- c("PUMA","WAGP","PERNP","FINCP","PINCP","HINCP","WKHP","COW",
                   "AGEP","RACBLK","RACASN","QTRBIR","NATIVITY","RACWHT",
                   "SEX","LANX","HHL","ENG","CIT","WAOB","POBP","MAR","HHT",
                   "ESR","HISP","ANC","YOEP","SCH","DECADE","SCHL","SCHG","POVPIP")

### CREATING FUNCTION TO CREATE DF ###
##### CIT == 5 HERE (NON US CITIZENS) #####
get_df <- function(year, variables) {
  get_pums(
    variables = variables,
    state = 06,
    year = year,
    survey = 'acs1',
    variables_filter = list(CIT = 5)
  )
}

### CREATING FUNCTION THAT GETS DF AND RETURNS CSV ###
create_csv <- function(year, variables) {
  california <- get_df(year = year, variables = variables)
  form = paste('non_cit_california_', year, ".csv", sep = "")
  write.csv(california, form)
}

# getting 2005-2021 data (except 2020)
for (i in 2005:2021) {
  if (i == 2020) {
    next
  }
  create_csv(year = i, variables = selected_vars)
}


########## creating function that adds survey year #################
add_year <- function(df, year) {
  df %>% 
    mutate(survey_year = year,
           .before=SERIALNO)
}

### Output new data to folder ###
create_csv <- function(df, year) {
  # adding year of survey
  newer_df <- add_year(df, year)
  # creating csv and outputting
  form = paste('non_cit_california_', year, ".csv",
               sep = "")
  write.csv(newer_df, form)
}

########### Getting new DFS ##############
for (year in 2005:2021){
  if (year == 2020) {
    next
  }
  file_name <- paste("non_cit_california_", year, ".csv",
                     sep = "")
  data <- read_csv(file_name)
  create_csv(data, year)
  if (year == 2021) {
    break
  }
}

##### Getting the sum of rows for each year ######
rows <- c()

for (year in 2005:2021){
  if (year == 2020) {
    next
  }
  file_name <- paste("non_cit_california_", year, ".csv",
                     sep = "")
  data <- read_csv(file_name)
  rows <- c(rows, nrow(data))
}

# total sum for 2005-2021 #
#### total is 724123 non-citizens ####
sum(rows)

###############################################################################
##### COMBINE INTO ONE LARGE DATASET #####
##############################################################################

# setting up empty df
combined_data <- data.frame()

# For loop to combine all datasets
for (year in 2005:2021) {
  if (year == 2020) {
    next
  }
  file_name <- paste0("non_cit_california_", year, ".csv")
  year_data <- read_csv(file_name)
  combined_data <- bind_rows(combined_data, year_data)
}

###### only 2005 - 2015 combined ########
####### going to save that as a csv ##########
# write_csv(combined_data, "non_cit_california_05_15.csv")

######### combining the rest ############
# setting up empty df
combined_data <- data.frame()

# For loop to combine rest datasets
for (year in 2016:2021) {
  if (year == 2020) {
    next
  }
  file_name <- paste0("non_cit_california_", year, ".csv")
  year_data <- read_csv(file_name)
  combined_data <- bind_rows(combined_data, year_data)
}
##### cannot combine bc columns are not the same data type #####


##### converting columns in 2016-2021 to same data types #######
# creating empty file name vector
non_us_cali_16 <- read_csv("non_cit_california_2016.csv")
non_us_cali_17 <- read_csv("non_cit_california_2017.csv")
non_us_cali_18 <- read_csv("non_cit_california_2018.csv")
non_us_cali_19 <- read_csv("non_cit_california_2019.csv")
non_us_cali_21 <- read_csv("non_cit_california_2021.csv")

non_us_cali_05_15 <- read_csv("non_cit_california_05_15.csv")


########## 2016 ############################
glimpse(non_us_cali_16)

# need to convert serialno to dbl
non_us_cali_16 <- non_us_cali_16 |> 
  mutate(SERIALNO = as.double(SERIALNO))

### merging 05-15 with 2016
non_us_cali_05_16 <- bind_rows(non_us_cali_05_15, non_us_cali_16)
# write_csv(non_us_cali_05_16, "non_cit_california_05_16.csv")

########## 2017 ############################
glimpse(non_us_cali_17)

non_us_cali_17 <- non_us_cali_17 |>
  mutate_all(as.numeric)

########## 2018 ############################
glimpse(non_us_cali_18)

non_us_cali_18 <- non_us_cali_18 |> 
  mutate_all(as.numeric)

########## 2019 ############################
glimpse(non_us_cali_19)

non_us_cali_19 <- non_us_cali_19 |> 
  mutate_all(as.numeric)

########## 2021 ############################
glimpse(non_us_cali_21)

non_us_cali_21 <- non_us_cali_21 |> 
  mutate_all(as.numeric)

########## merging 2017-2019 ###########
non_us_cali_17_21 <- bind_rows(non_us_cali_17, non_us_cali_18, non_us_cali_19,
                               non_us_cali_21)

###### saving 2017-2021 as a csv #######
# write_csv(non_us_cali_17_21, "non_us_cali_17_21.csv")

##################################################################
############## Cobining all data from 2005-2021 ##################
##################################################################

non_us_cali_05_16 <- read_csv("non_cit_california_05_16.csv")
non_us_cali_17_21 <- read_csv("non_us_cali_17_21.csv")

non_us_cali_05_21 <- bind_rows(non_us_cali_05_16, non_us_cali_17_21)
# write_csv(non_us_cali_05_21, "non_us_cali_2005_2021.csv")

# import data
df <- read_csv("non_us_cali_2005_2021.csv")
df <- df |>
  janitor::clean_names() |> 
  select(-c(1, 2))

# creating post daca dummy, birth_year, and age of entry columns
df <- df |> 
  mutate(post_daca = ifelse(survey_year >= 2013, 1, 0),
         birth_year = survey_year - agep,
         age_of_entry = yoep - birth_year)

# creating daca eligible dummy only for 2013-2019
df_05_19 <- df |> 
  filter(survey_year <= 2019) |> 
  mutate(daca_eligible = ifelse(
    birth_year >= 1981 & qtrbir >= 3 &
      yoep <= 2007 & age_of_entry < 16 &
      schl >= 16 & agep >= 15 & survey_year >= 2013, 1, 0
  ))

# creating daca eligible dummy for 2021
df_21 <- df |> 
  filter(survey_year == 2021) |> 
  mutate(daca_eligible = ifelse(
    birth_year >= 1981 & qtrbir >= 3 &
      yoep <= 2007 & age_of_entry < 16 &
      schl >= 16 & agep >= 16, 1, 0
  ))

# combining all dfs
df_05_21 <- bind_rows(df_05_19, df_21)

# adding some other variables
df_05_21 <- df_05_21 |> 
  mutate(high_school_and_equiv = ifelse(schl %in% c(16, 17), 1, 0),
         ged = ifelse(schl == 17, 1, 0),
         working = ifelse(wkhp > 1, 1, 0),
         unemployed = ifelse(cow == 9, 1, 0),
         self_employed = ifelse(cow %in% c(6, 7), 1, 0),
         attended_college = ifelse(schl >= 18, 1, 0),
         male = ifelse(sex == 1, 1, 0),
         married = ifelse(mar == 1, 1, 0),
         other_lang = ifelse(lanx == 1, 1, 0),
         daca_eligible_label = case_when(
           daca_eligible == 1 ~ "daca eligible"
         ),
         hisp_label = case_when(
           hisp == 1 ~ "not spanish/latino/hispanic",
           hisp == 2 ~ "Mexican",
           hisp == 3 ~ "Puerto Rican",
           hisp == 4 ~ "Cuban",
           hisp == 5 ~ "Dominican",
           hisp == 6 ~ "Costa Rican",
           hisp == 7 ~ "Guatemalan",
           hisp == 8 ~ "Honduran",
           hisp == 9 ~ "Nicaraguan",
           hisp == 10 ~ "Panamanian",
           hisp == 11 ~ "Salvadoran",
           hisp == 12 ~ "Other Central American",
           hisp == 13 ~ "Argentinean",
           hisp == 14 ~ "Bolivian",
           hisp == 15 ~ "Chilean",
           hisp == 16 ~ "Colombian",
           hisp == 17 ~ "Ecuadorian",
           hisp == 18 ~ "Paraguayan",
           hisp == 19 ~ "Peruvian",
           hisp == 20 ~ "Uruguayan",
           hisp == 21 ~ "Venezuelan",
           hisp == 22 ~ "Other South American",
           hisp == 23 ~ "Spaniard",
           hisp == 24 ~ "All Other Spanish/Hispanic/Latino"
         ),
         years_of_education = case_when(
           schl %in% c(2, 3, 4) ~ 1,
           schl == 5 ~ 2,
           schl == 6 ~ 3,
           schl == 7 ~ 4,
           schl == 8 ~ 5,
           schl == 9 ~ 6,
           schl == 10 ~ 7,
           schl == 11 ~ 8,
           schl == 12 ~ 9,
           schl == 13 ~ 10,
           schl == 14 ~ 11,
           schl %in% c(16, 17) ~ 12,
           schl %in% c(18, 19) ~ 13,
           schl == 20 ~ 14,
           schl == 21 ~ 15,
           schl == 22 ~ 16,
           schl %in% c(23, 24) ~ 17
         ),
         years_living_in_us = survey_year - yoep,
         under_31_now = ifelse(
           (birth_year == survey_year - 32 & qtrbir >= 3) |
             birth_year >= survey_year - 31, 1, 0
         ))

# creating daca eligible before 2013
df_pre_daca <-  df_05_21 |> 
  filter(between(survey_year, 2005, 2012)) |> 
  mutate(daca_eligible = ifelse(
    under_31_now == 1 & qtrbir >= 3 & age_of_entry < 16 &
      schl >= 16 & years_living_in_us >= 5, 1, 0
  ))

# looking at daca_eligible distribution before 2013
df_pre_daca |> 
  group_by(survey_year, daca_eligible) |> 
  summarise(n = n())

# combining data sets
df_05_21 <- bind_rows(df_05_21, df_pre_daca)

# adding interaction
df_05_21 <- df_05_21 |> 
  mutate(interaction_daca_eligible_post_daca = daca_eligible * post_daca)

# looking at daca eligible distribution all years
daca_distribution <- df_05_21 |> 
  group_by(survey_year, daca_eligible) |> 
  summarise(n = n()) |> 
  pivot_wider(names_from = daca_eligible, values_from = n) |> 
  rename(non_eligible = '0',
         eligible = '1')


# writing a csv
# write_csv(df_05_21, "clean_non_cit_cali_2005_2021.csv")


