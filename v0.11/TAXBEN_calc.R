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


#Add defaults for initial testing [this is written as calls below] ----
  partnered <- 1
  partner_earnings <- 80000
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
  
  #### Set to 0 to turn HECS repayments off 
  
  HECS_on <- 0
  
  ## Set up hourly income plots ----

  calc_hours_charts()
  
  #### Set on Income Rather than Wage. 
  
  #### Up to how much private earnings do we want to consider
  
  max_private_earnings <- 200000
  
    
  net_income_calculator_PI <- function(max_private_earnings) {
    # Generate the earnings vector
    earnings_vector <- seq(0, max_private_earnings, by = 100)
    
    # Initialize the dataframe to store earnings and their corresponding net incomes
    net_income_profile_PI <- data.frame(earnings = numeric(0), net_income = numeric(0))
    
    # Loop through each earnings value
    for (earnings in earnings_vector) {
      # Calculate net income for the current earnings
      net_income <- calc_net_income_private(earnings, HECS_on)$net_income
      
      # Append the earnings and net income to the dataframe
      net_income_profile_PI <- rbind(net_income_profile_PI, data.frame(earnings = earnings, net_income = net_income))
    }
    
    return(net_income_profile_PI)
  }
  
  net_income_profile_PI <- net_income_calculator_PI(max_private_earnings)
  
  plot_income_PI <- ggplot(net_income_profile_PI, aes(x=earnings / 1000 , y = net_income / 1000)) + geom_line() + theme_e61() + labs_e61(
    title = "Net Income",
    subtitle = paste0("Annual income, period = ",policy_date1),
    y = "(000s)",
    x = "Private Income (000s)",
    sources = c("e61", cite)) 
  
  plot_income_PI
  
  int_plot_income_PI <- ggplotly(plot_income_PI)
  
  int_plot_income_PI
  
  
  
  
  
  #### Fully Detailed Calculation/breakdown
  
  highly_detailed_variable_calculator_private <- function(max_private_earnings) {
    # Initialize an empty data frame to store results for each hour
    results_df <- data.frame(net_income = numeric(0), income_tax = numeric(0),
                             work_income = numeric(0), JSP = numeric(0), PP_Pay = numeric(0), 
                             RA = numeric(0), ES = numeric(0),
                             taxable_benefit = numeric(0), gross_income = numeric(0),
                             gross_fam_income = numeric(0), net_fam_a_income = numeric(0), 
                             net_fam_b_income = numeric(0), HECS_payment <- numeric(0))
    
    # Generate the earnings vector
    earnings_vector <- seq(0, max_private_earnings, by = 100)
    
    
    for (earnings in earnings_vector) {
      # Calculate net income and other variables for the given number of hours
      calc_results <- calc_net_income_detailed_private( earnings, HECS_on)
      
      # Append the results to the data frame
      results_df <- rbind(results_df, cbind(as.data.frame(t(unlist(calc_results)))))
    }
    results_df$income_tax <- results_df$income_tax * -1 
    results_df$HECS_payment <- results_df$HECS_payment * -1 
    results_df$taxable_benefit <- results_df$taxable_benefit * 26
    return(results_df)
  }
  
  incomes_data_PI_highly_detailed <- highly_detailed_variable_calculator_private(max_private_earnings)
  incomes_data_PI_highly_detailed$private_income <- incomes_data_PI_highly_detailed$work_income
  
  all_incomes_PI_long <- pivot_longer(incomes_data_PI_highly_detailed, 
                                   cols = c(income_tax, RA, JSP, ES, PP_Pay,  net_fam_a_income, net_fam_b_income, HECS_payment, private_income),
                                   names_to = "income_type", 
                                   values_to = "amount")
  
  # Plotting
  plot_income_highly_detailed_PI <- ggplot() +
    geom_area(data = all_incomes_PI_long, aes(x = work_income / 1000, y = amount / 1000, fill = income_type), position = "stack")  +
    geom_line(data = incomes_data_PI_highly_detailed, aes(x = work_income / 1000, y = net_income / 1000, colour = "Net Income"), size = 2) + 
    scale_colour_manual(values = c("Net Income" = "black")) +
    labs_e61(title = "Net Tax and Transfers based upon private income",
             x = "Private Income (000s)",
             y = "$",
             fill = "Income Type",
             colour = "") + add_baseline()  + theme(legend.position = "bottom") + scale_fill_e61()
  
  plot_income_highly_detailed_PI
  
  
 
  ##### Family Household income 
  
  
  calc_net_income_detailed_private( partner_earnings, HECS_on)$work_income
  
  
  
  
  
  
  
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
