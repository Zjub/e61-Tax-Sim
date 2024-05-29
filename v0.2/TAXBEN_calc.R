## Pulls in functions and policies to generate income and tax measures ----
# Last update: 8/03/2023
# Last updated by: Matt Nolan
# Authors: Matt Nolan

# Potential replacements for a function call when comfortable denoted with a XXXX

### The below function takes a series of function calls provided by the user and provides: i) income and ETR profiles by hour, ii) income and ETR profiles by earnings. Everything is calculated on an "annualised" basis given a certain set of policy parameters (which is inaccurate).

#TAXBEN_calc <- function(){ # Needs to be a function of all the options we have in the shiny
remotes::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")

if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  data.table,
  plotly,
  theme61)

##################################### Load in Files 

library(theme61) # Having some specific issues pulling this package in.
# Include functions ----
scripts <- list.files(pattern = "calc_")
for (script in scripts){
  source(script)
}

# Import parameters for the given quarter ----

policy_date1 <- "Q1_2023" # XXXX Replace this with a function call.

policy_parameters_1 <- policy_parameters(policy_date1)

# Assign policy parameters from the policy file.
for (i in 1:nrow(policy_parameters_1)){
  assign(policy_parameters_1$Parameter[i],policy_parameters_1$Value[i])
}


###################################Add defaults for initial testing [this is written as calls below] ----
  partnered <- 1
  partner_earnings <- 10000
  over_60 <- 0
  living_alone <- 0
  Have_dep <- 1
  Main_carer_dep <- 1
  child_ages <- c(1,9)

  Numb_dep <- 0
  young_child <- 0

  # Individual characteristics
  Carer <- 0
  Disability <- 0

  # Housing status
  Home_owner <- 0
  Rent <- 900 # Fortnightly rent amount

  # Labour market conditions
  wage <- 35
  max_hours <- 60

  # Meets conditions to access benefit on a personal level
  ben_eligibility <- 1

  # Citation
  cite <- "The world"


## Setup the individuals characteristics based on user inputs ----
# Define family characteristics
  partnered <- as.numeric(partnered)
  partner_earnings <- partner_earnings
  over_60 <- as.numeric(over_60)
  living_alone <- as.numeric(living_alone)
  Have_dep <- as.numeric(Have_dep)
  child_age <- child_ages
  if (Main_carer_dep == 0){
    child_age <- 99
  }

  Numb_dep <- length(child_age[child_age < 20])
  young_child <- min(child_age)

  PPeligible <- ifelse((young_child <= 6 | (young_child <= 8 & partnered == 0)) & Main_carer_dep == 1,1,0)

  # Individual characteristics
  Carer <- 0
  Disability <- 0

  # Housing status
  Home_owner <- 0
  Rent <- Rent # Fortnightly rent amount

  # Labour market conditions
  wage <- wage
  max_hours <- max_hours

  # Meets conditions to access benefit on a personal level
  ben_eligibility <- 1
  
  #### HECS Debt (Set to 0 if no HECS Debt)

  HECSDebt <- 50000
  
  #### Set to 0 to turn HECS and medicare levy off. 
  
  HECS_on <- 1
  Medicare_levy_on <- 1
  RA_Abate <- 1 
  
  max_private_earnings <- 90
  
  
  ################################################ Produce charts 
  
  #### 1 = income on x axis
  #### 0 = hours on x axis 

  calc_income_or_hours(1)
 