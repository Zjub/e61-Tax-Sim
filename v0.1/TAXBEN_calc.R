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

# Set up the income tax scale
tax_brackets <- c(0, tax_free, tax_threshold_1, tax_threshold_2, tax_threshold_3)
tax_scale <- c(0, tax_rate_1, tax_rate_2, tax_rate_3, tax_rate_4)

# Benefit elements [currently only JSP]
JSP_S_ND_arate <- c(0,JSP_S_ND_arate_1,JSP_S_ND_arate_2)
JSP_S_ND_athresh <- c(0,JSP_S_ND_athresh_1,JSP_S_ND_athresh_2)

#Add defaults for initial testing [this is written as calls below] ----
  partnered <- 0
  partner_earnings <- 0
  over_60 <- 0
  living_alone <- 0
  Have_dep <- 1
  Main_carer_dep <- 1
  child_ages <- c(12,14)

  Numb_dep <- 2
  young_child <- 12

  # Individual characteristics
  Carer <- 0
  Disability <- 0

  # Housing status
  Home_owner <- 0
  Rent <- 500 # Fortnightly rent amount

  # Labour market conditions
  wage <- 25
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


  ## Set up hourly income plot and text ----

  net_income_calculator_hourly <- function(wage_rate) {
    net_income_profile_hourly <- numeric((max_hours +1))
    for (hours in 0:max_hours) {
      net_income_profile_hourly[hours + 1] <- calc_net_income(wage_rate, hours)[["net_income"]]
    }
    return(net_income_profile_hourly)
  }

  net_incomes_data_hourly <- net_income_calculator_hourly(wage)

  incomesdf_hourly <- data.frame(Hours = seq(0,max_hours,by=1), "Take home income" = net_incomes_data_hourly) %>% mutate("Work Income" = wage*Hours*52) %>% pivot_longer(!Hours,names_to = "variable",values_to = "value")

  plot_income_hourly <- ggplot(incomesdf_hourly,aes(x=Hours,y=value/1000,colour=variable)) + geom_line() + theme_e61() + labs_e61(
    title = "Net Income",
    subtitle = paste0("Annual income, period = ",policy_date1),
    y = "(000s)",
    x = "Hours worked",
    sources = c("e61", cite)
  ) + scale_y_continuous_e61(labels = scales::dollar,limits=c(0,max(incomesdf_hourly$value/1000)*1.2)) +
    plot_label("Work Income",y= 0.9*max(incomesdf_hourly$value/1000),x= 10,colour = palette_e61(2)[2]) +
    plot_label("Disposable Income",y= 0.8*max(incomesdf_hourly$value/1000),x= 10,colour = palette_e61(2)[1]) +
    geom_hline(yintercept = 0)

  plot_income_hourly
  
  int_plot_income_hourly <- ggplotly(plot_income_hourly)
  
  int_plot_income_hourly
  
  #plot_gross_inc_components # Make this benefits + family benefits + wage income initially.

  ## Calculate tax concepts and text ----

  # Finalise distinction before moving on - I think the only file that needs adjustment is the calc_net_income, and the others are the same whether we call hours or incomes - as they transform a level of income in both cases.
  net_income_calculator_inc <- function(tax_brackets,tax_scale) {
    net_income_profile_inc <- numeric((Max_inc + 1))
    for (i in 0:Max_inc) {
      net_income_profile[i + 1] <- calc_net_income(work_income = i*1000,tax_brackets,tax_scale)[["net_income"]]
    }
    return(net_income_profile)
  }





#}
