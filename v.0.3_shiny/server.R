#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  data.table,
  plotly,
  theme61, shiny, shinycssloaders)


scripts <- list.files(pattern = "calc_")
for (script in scripts){
  source(script)
}

basis_text <- "hours"
basis_text2 <- "hour worked"

# Define server logic required to draw a histogram
function(input, output, session) {
  observeEvent(input$num_dependents, {
    # Generate child age inputs dynamically based on the number of dependents
    output$child_age_inputs <- renderUI({
      num <- input$num_dependents
      if (num == 0) {
        return(NULL)
      } else {
        lapply(1:num, function(i) {
          numericInput(paste0("child_age_", i), paste0("Dependent ", i, " Age"), value = 0, min = 0, max = 19)
        })
      }
    })
  })
  
 
 
  
   
  observeEvent(input$num_tax_brackets, {
    # Generate tax rate and threshold inputs dynamically based on the number of tax brackets
    output$tax_brackets_inputs <- renderUI({
      num <- input$num_tax_brackets
      if (num == 0) {
        return(NULL)
      } else {
        lapply(1:num, function(i) {
          default_rate <- if (i == 1) 0.16 else if (i == 2) 0.30 else if (i == 3) 0.37 else if (i == 4) 0.45 else 0
          default_threshold <- if (i == 1) 18200 else if (i == 2) 45000 else if (i == 3) 135000 else if (i == 4) 190000 else 0
          tagList(
            numericInput(paste0("tax_rate_", i), paste("Tax Rate", i), value = default_rate, min = 0, max = 1),
            numericInput(paste0("tax_threshold_", i), paste("Tax Threshold", i), value = default_threshold, min = 0)
          )
        })
      }
    })
  })
  
  
 # observeEvent(input$num_abatement_thresholds, {
    # Generate abatement rate and threshold inputs dynamically based on the number of abatement thresholds
 #   output$abatement_inputs <- renderUI({
 #     num <- input$num_abatement_thresholds
 #     if (num == 0) {
 #       return(NULL)
#      } else {
#        lapply(1:num, function(i) {
#          
#          default_rate <- if (i == 1) 0.5 else if (i == 2) 0.6 else 0
#          default_threshold <- if (i == 1) 150 else if (i == 2) 256 else 0
#          tagList(
#            numericInput(paste0("abatement_threshold_", i), paste("Abatement Threshold", i), value = default_rate, min = 0),
#            numericInput(paste0("abatement_rate_", i), paste("Abatement Rate", i), value = default_threshold, min = 0, max = 1)
#          )
#        })
#      }
 #   })
#  })
  
  
  observeEvent(c(input$update_input), {
  # Import parameters for the given quarter ----
    output$Title_1 <- renderUI({
      HTML("<b>How do I interpret these charts?</b>")
    })
    output$interpretation_text <- renderText({
            paste("The above charts show the impact of labour market participation on an individual's earnings.",
            "The left hand chart is an earnings schedule, which has", basis_text,
            "on the x axis, and components of income and taxes on the Y axis.",
            "You can hover your mouse over different colours within the chart to see",
            "what income component it relates to and how it changes in response to more labour market engagement.
            The right-hand chart shows the workers Effective Marginal Tax Rates (EMTR).")
    })
  
    output$Title_2 <- renderUI({
      HTML("<b>What is an EMTR?</b>")
    })    
    output$interpretation_text_2 <- renderText({
      paste("A worker's Effective Marginal Tax Rate (EMTR) is the percentage of
      each additional dollar earned that is taken away through taxes and reduced benefits.", 
            "In this context, it reflects, for an additional",
      basis_text2,
        "what proportion of income earned will be withdrawn. You can hover
      over the different elements on the right-hand chart to see whether income withdrawn
      is due to tax paid or benefits lost. A high EMTR discourage further labor market engagement
      by reducing the financial incentive to earn additional income. An EMTR above 1 implies that
      the worker loses more than 100% of their income earned through marginal labour market
      engagement. It is important to note that, in reality, workers do not have the capacity to perfectly target
      their labour market engagement (for instance choosing the exact amount of 
      labour market income to earn or hours to work per week). This may mean 
      that rather than working an additional hour, a worker may choose to 
      engage in the labour market for another shift per week, or work another day."   )
    }) 
    
    output$Title_3 <- renderUI({
      HTML("<b>What is the black line (Net Income)?</b>")
    })   
    
    output$interpretation_text_3 <- renderText({"The black line on the left hand chart
      is Net Income, which equals all of the worker's income (labour income + transfers) 
      minus their taxes paid. In other words, it is sum of all the components above the 
      x axis (0) minus all of the components below the x axis."
    }) 
    
    output$Title_4 <- renderUI({
      HTML("<b>What is the grey line (Average Tax Rate)?</b>")
    })   
    
    output$interpretation_text_4 <- renderText({"The grey line on the left hand chart 
      is the average effective tax rate paid by the person. This is the average rate the 
      individual is taxed and have their transfers withdrawn, relative to no labour market
      engagement."
    }) 
    

  policy_date1 <- "Q3_2024" 
  
  policy_parameters_1 <- policy_parameters(policy_date1)
  
  # Assign policy parameters from the policy file.
  for (i in 1:nrow(policy_parameters_1)) {
    eval(parse(text = paste0(policy_parameters_1$Parameter[i], " <<- ", policy_parameters_1$Value[i])))
  }
  
  
  ###################################Add defaults for initial testing [this is written as calls below] ----
  partnered <<- ifelse(input$partnered == TRUE, 1, 0)
  partner_earnings <<- ifelse(partnered == 1, input$partner_income, 0)
  over_60 <<-0
  living_alone <<-0
  Have_dep <<- ifelse(input$num_dependents > 0, 1, 0)
  Main_carer_dep <<-1
  num_dependents <- input$num_dependents
  if (num_dependents == 0) {
    child_ages <- integer(0)  # Return a zero-length integer vector if num_dependents is 0
  } else {
    child_ages <- sapply(1:num_dependents, function(i) {
      input[[paste0("child_age_", i)]]
    })
  }
  
  Numb_dep <<- input$num_dependents
 
  # Individual characteristics
  Carer <<-0
  Disability <<-0
  
  # Housing status
  Home_owner <<- ifelse(input$renter == TRUE, 0, 1)
  Rent <<- ifelse(input$renter == TRUE, input$weekly_rent * 2, 0) # Fortnightly rent amount
  
  
  # Meets conditions to access benefit on a personal level
  ben_eligibility <<-1
  
  # Citation
  cite <-"The world"
  
  
  ## Setup the individuals characteristics based on user inputs ----
  # Define family characteristics
  partnered <<-as.numeric(partnered)
    living_alone <<-as.numeric(living_alone)
  Have_dep <<-as.numeric(Have_dep)
  child_age <<-child_ages
  if (Main_carer_dep == 0){
    child_age <<-99
  }
  
  Numb_dep <<-length(child_age[child_age < 20])
  young_child <<-min(child_age)
  
  PPeligible <<-ifelse((young_child <= 6 | (young_child <= 8 & partnered == 0)) & Main_carer_dep == 1,1,0)
  
  
  # Labour market conditions
  wage <<- ifelse(input$basis == "hours", input$wages, 25)
  max_hours <<-ifelse(input$basis == "hours", input$max_hours, input$max_income)
  
  # Meets conditions to access benefit on a personal level
  ben_eligibility <<-1
  
 
  HECS_on <<- ifelse(input$hecs_debt == TRUE, 1, 0)
  HECSDebt <<- ifelse(HECS_on == 1, input$total_hecs_debt, 0)
  Medicare_levy_on <<- ifelse(input$medicare_levy == TRUE, 0, 1)
  RA_Abate <<-1 
  
  BTO_on <<- 1 
  
  #### note the below should be an INTEGER and 
  #### represents the value in thousands (000s)
  
  max_private_earnings <<- ifelse(input$basis == "income",  input$max_income / 1000, 100)
  
  ################################################ Edit policy parameters 
  
  edited_tax_brackets <<- 0
  
  if(input$edit_tax_brackets == TRUE){
  for (i in 1:input$num_tax_brackets) {
    assign(paste0("tax_rate_", i), input[[paste0("tax_rate_", i)]], envir = .GlobalEnv)
    assign(paste0("tax_threshold_", i), input[[paste0("tax_threshold_", i)]], envir = .GlobalEnv)
  }
    tax_free <<- ifelse(input$num_tax_brackets > 0, tax_threshold_1, 0)
    num_brackets <<- input$num_tax_brackets
    edited_tax_brackets <<- ifelse(input$edit_tax_brackets == TRUE, 1, 0)
    LITO_on <<- ifelse(input$turn_off_LITO == TRUE , 0, 1)
    BTO_on <<- ifelse(input$turn_off_BTO == TRUE, 0 , 1 )
  }

  
  #######################################################################
  
  if(input$edit_job_seeker == TRUE){
    
    JSP_C_D_pay <<- input$job_seeker_couple
    JSP_C_ND_pay <<- input$job_seeker_couple
    
    JSP_S_D_pay <<- input$job_seeker_single
    JSP_S_ND_pay <<- input$job_seeker_single
    
    PP_C_pay <<- input$PP_couple
    PP_S_pay <<- input$PP_single
    
 #   if(input$num_abatement_thresholds == 0){
  #    
  #    JSP_S_ND_athresh_1 <<- Inf
  #    JSP_S_ND_athresh_2 <<- Inf
      
  #  }
   
  #  if(input$num_abatement_thresholds == 1){
  #    JSP_S_ND_athresh_1 <<- input$abatement_threshold_1
  #    JSP_S_ND_athresh_2 <<- Inf
      
      
  #    JSP_S_ND_arate_1 <<- input$abatement_rate_1
  #  } 
 #   
    
  }
  
  
  
  
  #### Create a function to loop through all of the variables and produce output for them. 
  highly_detailed_variable_calculator <- function(wage_rate, step_count) {
    # Initialize an empty data frame to store results for each hour
    results_df <- data.frame(hours = integer(0), net_income = numeric(0), income_tax = numeric(0),
                             work_income = numeric(0), JSP = numeric(0), PP_Pay = numeric(0), 
                             RA = numeric(0), ES = numeric(0),
                             taxable_benefit = numeric(0), gross_income = numeric(0),
                             gross_fam_income = numeric(0), net_fam_a_income = numeric(0), 
                             net_fam_b_income = numeric(0), HECS_payment <- numeric(0), 
                             medicare_levy <- numeric(0))
    
    for (hours in seq(0, max_hours, by = step_count)) {
      
      # Calculate net income and other variables for the given number of hours
      calc_results <- calc_net_income_detailed(wage_rate, hours, HECS_on, Medicare_levy_on )
      
      # Append the results to the data frame
      results_df <- rbind(results_df, cbind(hours, as.data.frame(t(unlist(calc_results)))))
    }
    results_df$income_tax <- results_df$income_tax * -1 
    results_df$HECS_payment <- results_df$HECS_payment * -1 
    results_df$taxable_benefit <- results_df$taxable_benefit * 26
    return(results_df)
  }
  
  
 
  
  
  if (input$basis == "hours") {
  #####################################################################################################
  #### Earnings Chart - By hours worked. 
 
  #### Looping through hours worked, based upon wage, from 0 to max_hours, in 
  #### incriments of 0.1, to allow for a reasonably smooth EMTR curve. 
  
  
  
  max_hours <- round(max_hours)
  incomes_data_hourly_highly_detailed <- highly_detailed_variable_calculator(wage, 0.1)
  
  all_incomes_long <- pivot_longer(incomes_data_hourly_highly_detailed, 
                                   cols = c(income_tax, work_income, RA, JSP, ES, PP_Pay, 
                                            net_fam_a_income, net_fam_b_income,
                                            HECS_payment, medicare_levy),
                                   names_to = "income_type", 
                                   values_to = "Amount")
  
  all_incomes_long$income_type <- as.factor(all_incomes_long$income_type)
  
  
  #### Relabel variables into their actual names. 
  
  levels(all_incomes_long$income_type) <- c("Energy Supplement", "HECS Payment", "Income Tax", 
                                            "Job Seeker Payment", "Medicare Levy", 
                                            "Family Tax Benefit A", 
                                            "Family Tax Benefit B", "Parenting Payment", 
                                            "Commonwealth Rent Assistance", "Work Income")
  
  all_incomes_long$'Income Type' <- all_incomes_long$income_type
  
  
  ### Two decimal places is good. 
  
  all_incomes_long <- all_incomes_long %>%
    mutate(Amount = round(Amount, 2))
  
  incomes_data_hourly_highly_detailed$'Net Income' <-  incomes_data_hourly_highly_detailed$net_income 
  
  # Creating the text string for the title of the chart 
  text_string <- paste("Income schedule for a", 
                       ifelse(partnered == 1, "partnered", "unpartnered"), 
                       ifelse(Home_owner == 1, "home owner", "renter")) 
  
  # Append Parenting Payment Benefit eligibility for the title of the chart 
  if (PPeligible == 1 & Numb_dep > 0) {
    text_string <- paste(text_string, "eligible for the Parenting Payment")
  } else if (PPeligible == 0 & Numb_dep > 0) {
    text_string <- paste(text_string, ", not eligible for the Parenting Payment")
  }
  
  max_net_income <- ( max(incomes_data_hourly_highly_detailed$work_income, na.rm = TRUE) -
                        min(incomes_data_hourly_highly_detailed$income_tax, na.rm = TRUE) + 
                        max(incomes_data_hourly_highly_detailed$net_fam_a_income, na.rm = TRUE)
                      + max(incomes_data_hourly_highly_detailed$net_fam_b_income, na.rm = TRUE) + 
                        max(incomes_data_hourly_highly_detailed$PP_Pay, na.rm = TRUE) + 
                        max(incomes_data_hourly_highly_detailed$JSP, na.rm = TRUE)) / 1000
  
  min_amount <- (min(incomes_data_hourly_highly_detailed$income_tax, na.rm = TRUE) + 
                   min(incomes_data_hourly_highly_detailed$HECS_payment, na.rm = TRUE) + 
                   min(incomes_data_hourly_highly_detailed$medicare_levy, na.rm = TRUE)) / 1000
  
  # Plotting the Hours Schedule. 
  Hours_Schedule <- ggplot() +
    geom_col(data = all_incomes_long,
             aes(x = hours, y = Amount / 1000  , fill = `Income Type`), width = 0.5 ) +
    geom_line(data = incomes_data_hourly_highly_detailed, aes(x = hours, y = `Net Income` / 1000 ), 
              size = 2, col = "black") +
    labs_e61(title = text_string,
             x = "Hours worked",
             y = "$ (000's)",
             fill = "Income Type",
             colour = "") + add_baseline()  + scale_fill_e61() +
    scale_x_continuous(expand = c(0, Inf)) + # Remove default expansion for x-axis
    scale_y_continuous(expand = c(0, 0), limits =c(min_amount, max_net_income)) + 
    scale_fill_manual(values = c("Energy Supplement" = e61_tealdark, "HECS Payment" = e61_bluedark,
                                 "Income Tax" = e61_greydark, 
                                 "Job Seeker Payment" = e61_bluelight, "Medicare Levy" = e61_coraldark , 
                                 "Family Tax Benefit A" = e61_corallight, 
                                 "Family Tax Benefit B" = e61_maroondark, "Parenting Payment" = e61_orangedark, 
                                 "Commonwealth Rent Assistance" = e61_orangelight, "Work Income" = e61_teallight))
  
  
  
  output$plot <- renderPlotly({ Hours_Schedule })
  
  ######################################################################################
  ### EMTR Chart - hours worked. 
  
  
  incomes_data_hourly_highly_detailed <- highly_detailed_variable_calculator(wage, 1)
  
  
  incomes_changes <- incomes_data_hourly_highly_detailed %>%
    mutate(across(-hours, ~. - lag(., default = first(.))), .names = "change_{.col}") %>%
    mutate(across(starts_with("change_"), ~if_else(row_number() == 1, 0, .)))
  
  
  
  incomes_ratios <- incomes_changes %>%
    mutate(across(-c(hours, work_income, starts_with("change_"), .names),
                  ~if_else(work_income == 0, 0, ./work_income)))
  
  
  all_ratios_long <- pivot_longer(incomes_ratios, 
                                  cols = c(income_tax, RA, JSP, ES, PP_Pay,  net_fam_a_income, 
                                           net_fam_b_income, HECS_payment, medicare_levy),
                                  names_to = "income_type", 
                                  values_to = "amount")
  
  all_ratios_long$income_type <- as.factor(all_ratios_long$income_type)
  levels(all_ratios_long$income_type) <- c("Energy Supplement", "HECS Payment", "Income Tax", 
                                           "Job Seeker Payment", "Medicare Levy", 
                                           "Family Tax Benefit A", 
                                           "Family Tax Benefit B", "Parenting Payment", 
                                           "Commonwealth Rent Assistance", "Work Income")
  
  all_incomes_long$'Income Type' <- all_incomes_long$income_type
  
  all_incomes_long <- all_incomes_long %>%
    mutate(Amount = round(Amount, 2))
  
  incomes_data_hourly_highly_detailed$'Net Income' <- incomes_data_hourly_highly_detailed$net_income 
  
  # Creating the text string
  text_string <- paste("EMTRs for a", 
                       ifelse(partnered == 1, "partnered", "unpartnered"), 
                       ifelse(Home_owner == 1, "home owner", "renter"), 
                       "earning $", wage, "an hour, with", Numb_dep, "children,") 
  
  # Append Parenting Payment Benefit eligibility
  if (PPeligible == 1 & Numb_dep > 0) {
    text_string <- paste(text_string, "eligible for the Parenting Payment")
  } else if (PPeligible == 0 & Numb_dep > 0) {
    text_string <- paste(text_string, ", not eligible for the Parenting Payment")
  }
  
  # Append HECS Debt information
  if (HECS_on == 1) {
    text_string <- paste(text_string, "and", HECSDebt, "in HECS Debt.")
  }
  
  
  
  all_ratios_long$EMTR <- round(all_ratios_long$amount, 2)
  
  all_ratios_long$'Tax/Transfer' <- all_ratios_long$income_type
  
  all_ratios_long$hours <- as.numeric(all_ratios_long$hours)
  all_ratios_long$EMTR <- abs(all_ratios_long$EMTR)
  
  all_ratios_long$'Total EMTR' <- round(abs(all_ratios_long$net_income - 1), 2)
  all_ratios_long$'Total EMTR' <- ifelse(all_ratios_long$hours == 0,
                                         0, all_ratios_long$'Total EMTR')
  
  incomes_data_hourly_highly_detailed$'Average Tax Rate' <- 1 - round(incomes_data_hourly_highly_detailed$net_income /
                                                                        (incomes_data_hourly_highly_detailed$work_income +
                                                                           incomes_data_hourly_highly_detailed$net_income[1]), 2)
  
  # Plotting
  EMTR_Schedule  <- ggplot() +
    geom_col(data = all_ratios_long, aes(x = hours,
                                         y = EMTR, fill = `Tax/Transfer`),
             width = 2) +
    geom_line(data = all_ratios_long, aes(x = hours, y = `Total EMTR` ), 
              col = "black" ) + 
    geom_line(data = incomes_data_hourly_highly_detailed, aes( x = hours,
                                                                y = `Average Tax Rate`),
              col = "grey", linetype = "dashed") + 
    scale_colour_manual(values = c("Net Income" = "black")) +
    labs_e61(title = "EMTR from working an additional hour a week",
             x = "Hours Worked",
             y = "EMTR",
             fill = "Income Type",
             colour = "") + add_baseline()  + scale_fill_e61() + 
    geom_hline(yintercept = 1, linetype = "dashed", col = "red") + 
    scale_fill_manual(values = c("Energy Supplement" = e61_tealdark,
                                 "HECS Payment" = e61_bluedark, "Income Tax" = e61_greydark, 
                                 "Job Seeker Payment" = e61_bluelight, "Medicare Levy" = e61_coraldark , 
                                 "Family Tax Benefit A" = e61_corallight, 
                                 "Family Tax Benefit B" = e61_maroondark, "Parenting Payment" = e61_orangedark, 
                                 "Commonwealth Rent Assistance" = e61_orangelight))
  

  output$plot2 <- renderPlotly({ EMTR_Schedule })
  basis_text <- "hours"
  basis_text2 <- "hour worked"
  } else if (input$basis == "income") {
  
#################################################3
#########################################3
################################################
### Private income
   
  max_private_earnings <- round(max_private_earnings)
  wage <- 1000 / 52 
  max_hours <- max_private_earnings 
  
  incomes_data_private_highly_detailed <- highly_detailed_variable_calculator(wage, 0.1)
  
  all_incomes_long <- pivot_longer(incomes_data_private_highly_detailed, 
                                   cols = c(income_tax, work_income, RA, JSP, ES, PP_Pay, 
                                            net_fam_a_income, net_fam_b_income,
                                            HECS_payment, medicare_levy),
                                   names_to = "income_type", 
                                   values_to = "Amount")
  
  all_incomes_long$income_type <- as.factor(all_incomes_long$income_type)
  
  
  #### Relabel variables into their actual names. 
  
  levels(all_incomes_long$income_type) <- c("Energy Supplement", "HECS Payment", "Income Tax", 
                                            "Job Seeker Payment", "Medicare Levy", 
                                            "Family Tax Benefit A", 
                                            "Family Tax Benefit B", "Parenting Payment", 
                                            "Commonwealth Rent Assistance", "Work Income")
  
  all_incomes_long$'Income Type' <- all_incomes_long$income_type
  
  
  ### Two decimal places is good. 
  
  all_incomes_long <- all_incomes_long %>%
    mutate(Amount = round(Amount, 2))
  
  incomes_data_private_highly_detailed$'Net Income' <-  incomes_data_private_highly_detailed$net_income 
  
  # Creating the text string for the title of the chart 
  text_string <- paste("Income schedule for a", 
                       ifelse(partnered == 1, "partnered", "unpartnered"), 
                       ifelse(Home_owner == 1, "home owner", "renter")) 
  
  # Append Parenting Payment Benefit eligibility for the title of the chart 
  if (PPeligible == 1 & Numb_dep > 0) {
    text_string <- paste(text_string, "eligible for the Parenting Payment")
  } else if (PPeligible == 0 & Numb_dep > 0) {
    text_string <- paste(text_string, "ineligible for the Parenting Payment")
  }
  
  max_net_income <- max(incomes_data_private_highly_detailed$net_income, na.rm = TRUE) 
  - min(incomes_data_private_highly_detailed$income_tax, na.rm = TRUE)
  
  min_amount <- min(all_incomes_long$Amount, na.rm = TRUE) * 1.4
  
  all_incomes_long$'Private Income (000s)' <- all_incomes_long$hours 
  incomes_data_private_highly_detailed$'Private Income (000s)' <- incomes_data_private_highly_detailed$hours 
  all_incomes_long$Amount <- all_incomes_long$Amount / 1000
  incomes_data_private_highly_detailed$`Net Income` <- incomes_data_private_highly_detailed$`Net Income` / 1000
  
  # Plotting the Hours Schedule. 
  Hours_Schedule <- ggplot() +
    geom_col(data = all_incomes_long,
             aes(x = `Private Income (000s)`,
                 y = Amount , fill = `Income Type`), width = 0.5 ) +
    geom_line(data = incomes_data_private_highly_detailed,
              aes(x = `Private Income (000s)`, y = `Net Income`), 
              size = 2, col = "black") +
    labs_e61(title = text_string,
             x = "Private Income (000s)",
             y = "$ (000s)",
             fill = "Income Type",
             colour = "") + add_baseline()  + scale_fill_e61() +
    scale_x_continuous(expand = c(0, Inf)) + 
    scale_fill_manual(values = c("Energy Supplement" = e61_tealdark, "HECS Payment" = e61_bluedark,
                                 "Income Tax" = e61_greydark, 
                                 "Job Seeker Payment" = e61_bluelight, "Medicare Levy" = e61_coraldark , 
                                 "Family Tax Benefit A" = e61_corallight, 
                                 "Family Tax Benefit B" = e61_maroondark, "Parenting Payment" = e61_orangedark, 
                                 "Commonwealth Rent Assistance" = e61_orangelight, "Work Income" = e61_teallight)) 
  #+ # Remove default expansion for x-axis
  # scale_y_continuous(expand = c(0, 0), limits =c(min_amount, max_net_income)) 
  
  output$plot <- renderPlotly({ Hours_Schedule })
  
  
  
  
  ######################################################################################
  ### EMTR Chart - private income 
  
  
  incomes_data_private_highly_detailed <- highly_detailed_variable_calculator(wage, 1)
  
  
  incomes_changes <- incomes_data_private_highly_detailed %>%
    mutate(across(-hours, ~. - lag(., default = first(.))), .names = "change_{.col}") %>%
    mutate(across(starts_with("change_"), ~if_else(row_number() == 1, 0, .)))
  
  incomes_ratios <- incomes_changes %>%
    mutate(across(-c(hours, work_income, starts_with("change_"), .names),
                  ~if_else(work_income == 0, 0, ./work_income)))
  
  
  all_ratios_long <- pivot_longer(incomes_ratios, 
                                  cols = c(income_tax, RA, JSP, ES, PP_Pay,  net_fam_a_income, 
                                           net_fam_b_income, HECS_payment, medicare_levy),
                                  names_to = "income_type", 
                                  values_to = "amount")
  
  all_ratios_long$income_type <- as.factor(all_ratios_long$income_type)
  levels(all_ratios_long$income_type) <- c("Energy Supplement", "HECS Payment", "Income Tax", 
                                           "Job Seeker Payment", "Medicare Levy", 
                                           "Family Tax Benefit A", 
                                           "Family Tax Benefit B", "Parenting Payment", 
                                           "Commonwealth Rent Assistance", "Work Income")
  
  all_incomes_long$'Income Type' <- all_incomes_long$income_type
  
  all_incomes_long <- all_incomes_long %>%
    mutate(Amount = round(Amount, 2))
  
  incomes_data_private_highly_detailed$'Net Income' <- incomes_data_private_highly_detailed$net_income 
  
  # Creating the text string
  text_string <- paste("EMTRs for a", 
                       ifelse(partnered == 1, "partnered", "unpartnered"), 
                       ifelse(Home_owner == 1, "home owner", "renter"), 
                       "earning $", wage, "an hour, with", Numb_dep, "children,") 
  
  # Append Parenting Payment Benefit eligibility
  if (PPeligible == 1 & Numb_dep > 0) {
    text_string <- paste(text_string, "eligible for the Parenting Payment")
  } else if (PPeligible == 0 & Numb_dep > 0) {
    text_string <- paste(text_string, ", not eligible for the Parenting Payment")
  }
  
  # Append HECS Debt information
  if (HECS_on == 1) {
    text_string <- paste(text_string, "and", HECSDebt, "in HECS Debt.")
  }
  
  
  
  all_ratios_long$EMTR <- round(all_ratios_long$amount, 2)
  
  all_ratios_long$'Tax/Transfer' <- all_ratios_long$income_type
  
  all_ratios_long$'Private Income (000s)' <- as.numeric(all_ratios_long$hours)
  all_ratios_long$EMTR <- abs(all_ratios_long$EMTR)
  
  all_ratios_long$'Total EMTR' <- round(abs(all_ratios_long$net_income - 1), 2)
  all_ratios_long$'Total EMTR' <- ifelse(all_ratios_long$`Private Income (000s)` == 0,
                                         0, all_ratios_long$'Total EMTR')
  
  incomes_data_private_highly_detailed$'Average Tax Rate' <- 1 - round(incomes_data_private_highly_detailed$net_income /
                                                                         (incomes_data_private_highly_detailed$work_income + incomes_data_private_highly_detailed$net_income[1]), 2)
  
  incomes_data_private_highly_detailed$'Private Income (000s)' <- incomes_data_private_highly_detailed$hours
  
  # Plotting
  EMTR_Schedule  <- ggplot() +
    geom_col(data = all_ratios_long, aes(x = `Private Income (000s)`,
                                         y = EMTR, fill = `Tax/Transfer`),
             width = 2) +
    geom_line(data = all_ratios_long, aes(x = `Private Income (000s)`, y = `Total EMTR` ), 
              col = "black" ) + 
    geom_line(data = incomes_data_private_highly_detailed, aes( x = `Private Income (000s)`,
                                                                y = `Average Tax Rate`),
              col = "grey", linetype = "dashed") + 
    scale_colour_manual(values = c("Net Income" = "black")) +
    labs_e61(title = "EMTR from earning an additional $1000",
             x = "Private Income (000s)",
             y = "EMTR",
             fill = "Income Type",
             colour = "") + add_baseline()  + scale_fill_e61() + 
    geom_hline(yintercept = 1, linetype = "dashed", col = "red") + 
    scale_fill_manual(values = c("Energy Supplement" = e61_tealdark,
                                 "HECS Payment" = e61_bluedark, "Income Tax" = e61_greydark, 
                                 "Job Seeker Payment" = e61_bluelight, "Medicare Levy" = e61_coraldark , 
                                 "Family Tax Benefit A" = e61_corallight, 
                                 "Family Tax Benefit B" = e61_maroondark, "Parenting Payment" = e61_orangedark, 
                                 "Commonwealth Rent Assistance" = e61_orangelight))
  
  output$plot2 <- renderPlotly({ EMTR_Schedule })
  basis_text <- "income"
  basis_text2 <- "$1000 dollars of labour market income earned"
  }
  })
 
  
   
}
