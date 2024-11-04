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

###### Load in all the calc scripts to run the code
scripts <- list.files(pattern = "calc_")
for (script in scripts){
  source(script)
}

##### Set text which will be used throughout the text of the descriptions of charts
##### Hours is the intial/default setup of the calculator. So is loaded in first. This is 
##### Overriden later on if the user swaps to income. 

basis_text <- "hours"
basis_text2 <- "hour worked"


##########################################################################################
# INPUT FUNCTIONS 
# This section translates the inputs from the user into changes into the UI. 

##### Create a function which observes the number of dependents, and creates a box
##### for each dependent, and adds a descriptive with which child number they are. 

function(input, output, session) {
  observeEvent(input$num_dependents, {
    # Generate child age inputs dynamically based on the number of dependents
    output$child_age_inputs <- renderUI({
      num <- input$num_dependents
      if (num == 0) {
        return(NULL)
      } else {
        lapply(1:num, function(i) {
          numericInput(paste0("child_age_", i), paste0("Dependent ", i, " Age"),
                       value = 0, min = 0, max = 19)
        })
      }
    })
  })
  
 
 
##### Create a function whcih observes the number of tax brackets the user wants to have 
#### In the Australian tax system, and generates that many boxes. Sets the default to be
#### The current arrangements. This is currently hard coded, and may be updated over tiem. 
   
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
            numericInput(paste0("tax_rate_", i),
                         paste("Tax Rate", i), value = default_rate, min = 0, max = 1),
            numericInput(paste0("tax_threshold_", i),
                         paste("Tax Threshold", i), value = default_threshold, min = 0)
          )
        })
      }
    })
  })
  
  
##### Create the text that displays below the charts to describe the results. 
##### These are intended to be dynamic - i.e. the the text updates depending on
####  the model inputs. Note that at this stage the only dynamic text is "income" or "hours'

  
  
  observeEvent(c(input$update_input), {
  # Import parameters for the given quarter ----
    output$Title_1 <- renderUI({
      HTML("<b>How do I interpret these charts?</b>")
    })
    
###### Text 1 - how do you interpret the charts     
    output$interpretation_text <- renderText({
            paste("The above charts show the how the individual's earnings
            change in response to changes
                  in labour market engagement.",
            "The left hand chart is an earnings schedule, which has", basis_text,
            "on the x axis, and components of income and taxes on the Y axis.",
            "You can hover your mouse over different colours on the chart to see",
            "what component of our tax and transfer system it relates to,
            and how these values change as 
            the individual works more or less. 
            The right-hand chart shows how the worker's
            Effective Marginal Tax Rate (EMTR) changes
            in response to changes in their labour 
            market engagement.")
    })
  
    output$Title_2 <- renderUI({
      HTML("<b>What is an EMTR?</b>")
    })    
    
####### Text 2: What is an EMTR?    
    output$interpretation_text_2 <- renderText({
      paste("A worker's Effective Marginal Tax Rate (EMTR) is the percentage of
      each additional dollar earned that is taken away through taxes and reduced benefits.", 
            "In the above chart it reflects the proportion of
            income withdrawn for each additional",
      basis_text2,
        ". You can hover over the different elements on the right-hand
        chart to see whether the income
      withdrawn is due to taxes paid or benefits lost.
      A high EMTR discourages further labour market engagement by reducing
      the financial incentive to earn additional income.
      An extreme case for this is an EMTR above 100%, which
      implies that the worker loses more than their
      additional income earned through marginal labor market engagement.
      It is important to note that, in reality,
      workers cannot perfectly target their labor market engagement
      (for instance, choosing the exact amount of labor market
      income to earn or hours to work per week).
      This may mean that instead of working an additional hour,
      a worker may choose to engage in the labor market for another shift per week or
      work another day."   )
    }) 
    
    output$Title_3 <- renderUI({
      HTML("<b>What is the black line (Net Income)?</b>")
    })   
    
 ##### Text 3 - an explanation of the "Net Income" line.     
    output$interpretation_text_3 <- renderText({"The black line on the left hand chart
      is Net Income, which equals all of the worker's income (labour income + transfers) 
      minus their taxes paid. In other words, it is sum of all the components above the 
      x axis (0) minus all of the components below the x axis."
    }) 
    
    output$Title_4 <- renderUI({
      HTML("<b>What is the grey line (Average Tax Rate)?</b>")
    })   
##### Text 4 - an explanation of the average effective tax rate line.     
    output$interpretation_text_4 <- renderText({"The grey line on the left hand chart 
      is the average effective tax rate paid by the person. This is the average rate the 
      individual is taxed and have their transfers withdrawn, relative to no labour market
      engagement."
    }) 
    
##########################################################################################
### Calculator itself begins here. 
    
#### Load in relevant policy parameters 
  policy_date1 <<- "Q3_2024" 
  policy_year <<- as.numeric(substring(policy_date1, 4,7))
  policy_quarter <<- as.numeric(substring(policy_date1,2,2))
  policy_parameters_1 <- policy_parameters(policy_date1)
  
  # Assign policy parameters from the policy file.
  for (i in 1:nrow(policy_parameters_1)) {
    eval(parse(text = paste0(policy_parameters_1$Parameter[i], " <<- ", policy_parameters_1$Value[i])))
  }
  
  
#### Pull inputs from the UI. 
  partnered <<- ifelse(input$partnered == TRUE, 1, 0)
  partner_earnings <<- ifelse(partnered == 1, input$partner_income, 0)
    Have_dep <<- ifelse(input$num_dependents > 0, 1, 0)
  num_dependents <- input$num_dependents
  Home_owner <<- ifelse(input$renter == TRUE, 0, 1)
  Rent <<- ifelse(input$renter == TRUE, input$weekly_rent * 2, 0) # Fortnightly rent amount

  
  ### Create Vector for child ages.   
  if (num_dependents == 0) {
    child_ages <- integer(0)  # Return a zero-length integer vector if num_dependents is 0
  } else {
    child_ages <- sapply(1:num_dependents, function(i) {
      input[[paste0("child_age_", i)]]
    })
  }
  child_age <<-child_ages
  Numb_dep <<- input$num_dependents
  Numb_dep <<-length(child_age[child_age < 20])
  young_child <<-min(child_age)
  
  # Labour market characteristics 

  wage <<- ifelse(input$basis == "hours", input$wages, 25)
  max_hours <<-ifelse(input$basis == "hours", input$max_hours, input$max_income)
  max_private_earnings <<- ifelse(input$basis == "income",  input$max_income / 1000, 100)
  max_range <<- round(ifelse(input$basis == "hours", input$max_hours, input$max_income / 1000))
  # hard coded/ irrelevant characteristics at this stage. 
  Carer <<-0
  Disability <<-0
  Main_carer_dep <<-1
  over_60 <<-0
  living_alone <<-0
    ben_eligibility <<-1
    partial_cap <<- 0 
 
  ### Check for parenting payment eligibility 
  PPeligible <<-ifelse((young_child <= 6 | (young_child <= 8 & partnered == 0)) & 
                         Main_carer_dep == 1,1,0)
  
###### Edited Policy parameters ##########################################################
  
###### HECS DEBT  
  HECS_on <<- ifelse(input$hecs_debt == TRUE, 1, 0)
  HECSDebt <<- ifelse(HECS_on == 1, input$total_hecs_debt, 0)
  
##### medicare levy   
  Medicare_levy_on <<- ifelse(input$medicare_levy == TRUE, 0, 1)
  work_for_the_dole <<- 0
  living_alone <<- 1 
  SAPTO_on <<- 1 
##### Rent assistance abatement   
  RA_Abate <<-1 
    RA_Abate <<- ifelse(input$RA_Abate == TRUE, 0, 1)
    
#### BTO
    
  BTO_on <<- 1
  
#### INCOME TAX BRACKETS 
  
  edited_tax_brackets <<- 0
  
  if(input$edit_tax_brackets == TRUE){
  for (i in 1:input$num_tax_brackets) {
    assign(paste0("tax_rate_", i), input[[paste0("tax_rate_", i)]], envir = .GlobalEnv)
    assign(paste0("tax_threshold_", i), input[[paste0("tax_threshold_", i)]],
           envir = .GlobalEnv)
  }
    tax_free <<- ifelse(input$num_tax_brackets > 0, tax_threshold_1, 0)
    num_brackets <<- input$num_tax_brackets
    edited_tax_brackets <<- ifelse(input$edit_tax_brackets == TRUE, 1, 0)
    LITO_on <<- ifelse(input$turn_off_LITO == TRUE , 0, 1)
    BTO_on <<- ifelse(input$turn_off_BTO == TRUE, 0 , 1 )
    SAPTO_on <<- ifelse(input$turn_off_SAPTO == TRUE, 0, 1)
  }

  
#### Edited Jobseker and Parenting Payment 
  
  if(input$edit_job_seeker == TRUE){
    
    JSP_C_D_pay <<- input$job_seeker_couple
    JSP_C_ND_pay <<- input$job_seeker_couple
    
    JSP_S_D_pay <<- input$job_seeker_single
    JSP_S_ND_pay <<- input$job_seeker_single
    
    PP_C_pay <<- input$PP_couple
    PP_S_pay <<- input$PP_single
    
  }
  
# UBI Mode. Turn all abatements to Infinite.  
  if(input$UBI_mode == TRUE){
  
    #    
     JSP_S_ND_athresh_1 <<- Inf
     JSP_S_ND_athresh_2 <<- Inf
     PP_C_I_Threshold_1 <<- Inf
     PP_C_I_Threshold_2 <<- Inf
     PP_C_P_Threshold <<- Inf 
     PP_S_athresh_2 <<- Inf
     PP_S_athresh_3 <<- Inf
     PP_S_athresh_base <<- Inf
     PP_S_athresh_mult <<- Inf
  }
  
  
#########################################################################################
### Actually Running the results 
  
  
#### Create a function that runs the tax calculator, and produces a data.frame with 
#### All of the relevant outputs 
  
  calculator_function <- function(payment_amount, step_count) {
    # Define the range that the calculator is going to run over 
    
    step_seq <- seq(0, max_range, by = step_count)
    
    # results list to store 
    results_list <- vector("list", length(step_seq))
    
    # Use lapply to apply the function over the sequence of hours
    results_list <- lapply(seq_along(step_seq), function(i) {
      hours <- step_seq[i]
      calc_results <- calc_hub(payment_amount, hours, 
                                               HECS_on, Medicare_levy_on)
      c(hours = hours, as.numeric(unlist(calc_results)))
    })
    
    # Combine  into a data frame
    results_df <- do.call(rbind, results_list)
    colnames(results_df) <- c("hours", "net_income", "income_tax", "work_income", "JSP", 
                              "PP_Pay", "RA", "ES", "taxable_benefit", "gross_income", 
                              "gross_fam_income", "net_fam_a_income", "net_fam_b_income", 
                              "HECS_payment", "medicare_levy", "PA")
    
    # Post-process the results to make taxes negative, and the taxable benefit into 
    # annual 
    results_df[, "income_tax"] <- results_df[, "income_tax"] * -1 
    results_df[, "HECS_payment"] <- results_df[, "HECS_payment"] * -1 
    results_df[, "taxable_benefit"] <- results_df[, "taxable_benefit"]
    
    return(as.data.frame(results_df))
  }
  
  
  
  
  
############# HOURS WORKED ##############################################################3
  
  if (input$basis == "hours") {

#### Run the results 
  incomes_data_hourly <- calculator_function(wage, 0.1)

#### Make the results into a long format.   
  
  all_incomes_long <- pivot_longer(incomes_data_hourly, 
                                   cols = c(income_tax, work_income, RA, JSP, ES, PP_Pay, 
                                            net_fam_a_income, net_fam_b_income,
                                            HECS_payment, medicare_levy, PA),
                                   names_to = "income_type", 
                                   values_to = "Amount")
  
  all_incomes_long$income_type <- as.factor(all_incomes_long$income_type)
  
  
  #### Relabel variables into their actual names that will display in the chart.  
  
  levels(all_incomes_long$income_type) <- c("Energy Supplement", "HECS Payment", "Income Tax", 
                                            "Job Seeker Payment", "Medicare Levy", 
                                            "Family Tax Benefit A", 
                                            "Family Tax Benefit B", "Pharmacutical Allowance", "Parenting Payment", 
                                            "Commonwealth Rent Assistance", "Work Income")
  
  all_incomes_long$'Income Type' <- all_incomes_long$income_type
  
  
  ### Round the results to 2 decimal places  
  
  all_incomes_long <- all_incomes_long %>%
    mutate(Amount = round(Amount, 2))
  
  incomes_data_hourly$'Net Income' <-  incomes_data_hourly$net_income / 1000
  
 ##### Ensure the chart has the appropriate maximum range and minimum range 
  
  min_amount <- (min(incomes_data_hourly$income_tax, na.rm = TRUE) + 
                   min(incomes_data_hourly$HECS_payment, na.rm = TRUE) + 
                   min(incomes_data_hourly$medicare_levy, na.rm = TRUE)) / 1000  
  
  max_net_income <- ( max(incomes_data_hourly$work_income, na.rm = TRUE) + 
                        max(incomes_data_hourly$net_fam_a_income, na.rm = TRUE)
                      + max(incomes_data_hourly$net_fam_b_income, na.rm = TRUE) + 
                        max(incomes_data_hourly$PP_Pay, na.rm = TRUE) + 
                        max(incomes_data_hourly$JSP, na.rm = TRUE)+ 
                        max(incomes_data_hourly$PA, na.rm = TRUE)) / 1000 - min_amount
  

  
  
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
  
  
  # Plotting the Hours Schedule. 
  Hours_Schedule_full <- ggplot() +
    geom_col(data = all_incomes_long,
             aes(x = hours, y = Amount / 1000  , fill = `Income Type`), width = 0.5 ) +
    geom_line(data = incomes_data_hourly, aes(x = hours, y = `Net Income` ), 
              size = 2, col = "black") +
    labs_e61(title = text_string,
             x = "Hours worked",
             y = "$ (000's)",
             fill = "Income Type",
             colour = "") + add_baseline()  + scale_fill_e61() +
    scale_x_continuous(expand = c(0, Inf)) + # Remove default expansion for x-axis
    scale_y_continuous(expand = c(0, 0), limits =c(min_amount, max_net_income)) + 
    scale_fill_manual(values = c("Energy Supplement" = e61_tealdark,
                                 "HECS Payment" = e61_bluedark,
                                 "Income Tax" = e61_greydark, 
                                 "Job Seeker Payment" = e61_bluelight,
                                 "Medicare Levy" = e61_coraldark , 
                                 "Family Tax Benefit A" = e61_corallight, 
                                 "Family Tax Benefit B" = e61_maroondark,
                                 "Parenting Payment" = e61_orangedark, 
                                 "Commonwealth Rent Assistance" = e61_orangelight,
                                 "Work Income" = e61_teallight, 
                                 "Pharmacutical Allowance" = "forestgreen"))
  
  Hours_Schedule_simple <- ggplot()  +
    geom_line(data = incomes_data_hourly, aes(x = hours, y = `Net Income` ), 
              col = "black") +
    labs_e61(title = text_string,
             x = "Hours worked",
             y = "$ (000's)",
             fill = "Income Type",
             colour = "") + add_baseline() +
    scale_x_continuous(expand = c(0, Inf)) + # Remove default expansion for x-axis
    scale_y_continuous(expand = c(0, 0), limits =c(min_amount, max_net_income)) + 
    geom_abline(intercept = 0, slope = 1, linetype = "dashed")
  
  

  # Use renderPlotly to switch between them
  output$plot <- renderPlotly({ 
    plot_to_display <- if (input$Detailed_1) Hours_Schedule_full else  Hours_Schedule_simple
    ggplotly(plot_to_display)
  })
  
  ######################################################################################
  ### EMTR Chart - hours worked. 
  
  ### Increments of 1 rather than 0.1 
  
  incomes_data_hourly <- calculator_function(wage, 1)
   ### Calculate the changes in variables from working an additional hour. 
  incomes_changes <- incomes_data_hourly %>%
    mutate(across(-hours, ~. - lag(., default = first(.))), .names = "change_{.col}") %>%
    mutate(across(starts_with("change_"), ~if_else(row_number() == 1, 0, .)))
  
  
  
  incomes_ratios <- incomes_changes %>%
    mutate(across(-c(hours, work_income, starts_with("change_"), .names),
                  ~if_else(work_income == 0, 0, ./work_income)))
  
  
  all_ratios_long <- pivot_longer(incomes_ratios, 
                                  cols = c(income_tax, RA, JSP, ES, PP_Pay,  net_fam_a_income, 
                                           net_fam_b_income, HECS_payment, medicare_levy, PA),
                                  names_to = "income_type", 
                                  values_to = "amount")
  
  all_ratios_long$income_type <- as.factor(all_ratios_long$income_type)
  levels(all_ratios_long$income_type) <- c("Energy Supplement", "HECS Payment", "Income Tax", 
                                           "Job Seeker Payment", "Medicare Levy", 
                                           "Family Tax Benefit A", 
                                           "Family Tax Benefit B", "Pharmacutical Allowance",
                                           "Parenting Payment", 
                                           "Commonwealth Rent Assistance", "Work Income")
  
  all_incomes_long$'Income Type' <- all_incomes_long$income_type
  
  #### Round to 2 decimal places 
  
  all_incomes_long <- all_incomes_long %>%
    mutate(Amount = round(Amount, 2))
  
  all_ratios_long$EMTR <- round(all_ratios_long$amount, 2)
  
  ### Rename variables. 
  
  incomes_data_hourly$'Net Income' <- incomes_data_hourly$net_income 
  all_ratios_long$'Tax/Transfer' <- all_ratios_long$income_type
  
  all_ratios_long$hours <- as.numeric(all_ratios_long$hours)
  all_ratios_long$EMTR <- abs(all_ratios_long$EMTR)
  
  #### Calcualte Total EMTRS and Average Tax Rates. 
  
  #### Total EMTR is the change in net income from the working a previous hour. 
  
  all_ratios_long$'Total EMTR' <- round(abs(all_ratios_long$net_income - 1), 2)
  all_ratios_long$'Total EMTR' <- ifelse(all_ratios_long$hours == 0,
                                         0, all_ratios_long$'Total EMTR')
  
  #### Average Tax Rate 
  
  incomes_data_hourly$'Average Tax Rate' <- 1 - round(incomes_data_hourly$net_income /
                                                        (incomes_data_hourly$work_income +
                                                           incomes_data_hourly$net_income[1]), 2)
  
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

  
  # Plotting
  EMTR_Schedule_complex  <- ggplot() +
    geom_col(data = all_ratios_long, aes(x = hours,
                                         y = EMTR, fill = `Tax/Transfer`),
             width = 2) +
    geom_line(data = all_ratios_long, aes(x = hours, y = `Total EMTR` ), 
              col = "black" ) + 
    geom_line(data = incomes_data_hourly, aes( x = hours, y = `Average Tax Rate`),
              col = "grey", linetype = "dashed") + 
    scale_colour_manual(values = c("Net Income" = "black")) +
    labs_e61(title = "EMTR from working an additional hour a week",
             x = "Hours Worked",
             y = "EMTR",
             fill = "Income Type",
             colour = "") + add_baseline()  + scale_fill_e61() + 
    geom_hline(yintercept = 1, linetype = "dashed", col = "red") + 
    scale_fill_manual(values = c("Energy Supplement" = e61_tealdark,
                                 "HECS Payment" = e61_bluedark, 
                                 "Income Tax" = e61_greydark, 
                                 "Job Seeker Payment" = e61_bluelight,
                                 "Medicare Levy" = e61_coraldark , 
                                 "Family Tax Benefit A" = e61_corallight, 
                                 "Family Tax Benefit B" = e61_maroondark, 
                                 "Parenting Payment" = e61_orangedark, 
                                 "Commonwealth Rent Assistance" = e61_orangelight, 
                                 "Pharmacutical Allowance" = "forestgreen"))
  
  
  EMTR_Schedule_simple <- ggplot() +
    geom_line(data = all_ratios_long, aes(x = hours, y = `Total EMTR` ), 
              col = "black" ) + 
    geom_line(data = incomes_data_hourly, aes( x = hours, y = `Average Tax Rate`),
              col = "grey", linetype = "dashed") + 
    scale_colour_manual(values = c("Net Income" = "black")) +
    labs_e61(title = "EMTR from working an additional hour a week",
             x = "Hours Worked",
             y = "EMTR",
             fill = "Income Type",
             colour = "") + add_baseline()  + scale_fill_e61() + 
    geom_hline(yintercept = 1, linetype = "dashed", col = "red")
  
  output$plot2 <- renderPlotly({ 
    plot_to_display <- if (input$Detailed_2) EMTR_Schedule_complex else  EMTR_Schedule_simple 
    ggplotly(plot_to_display)
  })
 
 
  
#### Ensure that the text is correct to describe the charts. 
  
  basis_text <- "hours"
  basis_text2 <- "hour worked"
  
  
###### Income Basis #####################################################################  
  } else if (input$basis == "income") {
  
### Moving up in increments of 1000 dollas annually divided by 52 weeks in a year. 
  Step_up <- 1000 / 52 
  
  incomes_data_private <- calculator_function(Step_up, 0.1)
  
  all_incomes_long <- pivot_longer(incomes_data_private, 
                                   cols = c(income_tax, work_income, RA, JSP, ES, PP_Pay, 
                                            net_fam_a_income, net_fam_b_income,
                                            HECS_payment, medicare_levy, PA),
                                   names_to = "income_type", 
                                   values_to = "Amount")
  
  all_incomes_long$income_type <- as.factor(all_incomes_long$income_type)
  
  
  #### Relabel variables into their actual names. 
  
  levels(all_incomes_long$income_type) <- c("Energy Supplement", "HECS Payment", "Income Tax", 
                                            "Job Seeker Payment", "Medicare Levy", 
                                            "Family Tax Benefit A", 
                                            "Family Tax Benefit B", "Pharmacutical Allowance",
                                            "Parenting Payment", 
                                            "Commonwealth Rent Assistance", "Work Income")
  
  ### Rename other variables for plotting 
  
  all_incomes_long$'Income Type' <- all_incomes_long$income_type
  incomes_data_private$'Net Income' <-  incomes_data_private$net_income 
  
  
  all_incomes_long <- all_incomes_long %>%
    mutate(Amount = round(Amount, 2))
  
  
  

# Ensure the chart has the appropriate maximum and minimum ranges   
    
  min_amount <- (min(incomes_data_private$income_tax, na.rm = TRUE) + 
                   min(incomes_data_private$HECS_payment, na.rm = TRUE) + 
                   min(incomes_data_private$medicare_levy, na.rm = TRUE)) / 1000  
  
  max_net_income <- ( max(incomes_data_private$work_income, na.rm = TRUE) + 
                        max(incomes_data_private$net_fam_a_income, na.rm = TRUE)
                      + max(incomes_data_private$net_fam_b_income, na.rm = TRUE) + 
                        max(incomes_data_private$PP_Pay, na.rm = TRUE) + 
                        max(incomes_data_private$JSP, na.rm = TRUE) + 
                        max(incomes_data_private$PA, na.rm = TRUE)) / 1000 - min_amount
  
  all_incomes_long$'Private Income (000s)' <- all_incomes_long$hours 
  incomes_data_private$'Private Income (000s)' <- incomes_data_private$hours 
  all_incomes_long$Amount <- all_incomes_long$Amount / 1000
  incomes_data_private$`Net Income` <- incomes_data_private$`Net Income` / 1000
  
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
  
  
  
  # Plotting the Hours Schedule. 
  Income_Schedule <- ggplot() +
    geom_col(data = all_incomes_long,
             aes(x = `Private Income (000s)`,
                 y = Amount , fill = `Income Type`), width = 0.5 ) +
    geom_line(data = incomes_data_private,
              aes(x = `Private Income (000s)`, y = `Net Income`), 
              size = 2, col = "black") +
    labs_e61(title = text_string,
             x = "Private Income (000s)",
             y = "$ (000s)",
             fill = "Income Type",
             colour = "") + add_baseline()  + scale_fill_e61() +
    scale_x_continuous(expand = c(0, Inf)) + 
    scale_fill_manual(values = c("Energy Supplement" = e61_tealdark, 
                                 "HECS Payment" = e61_bluedark,
                                 "Income Tax" = e61_greydark, 
                                 "Job Seeker Payment" = e61_bluelight,
                                 "Medicare Levy" = e61_coraldark , 
                                 "Family Tax Benefit A" = e61_corallight, 
                                 "Family Tax Benefit B" = e61_maroondark,
                                 "Parenting Payment" = e61_orangedark, 
                                 "Commonwealth Rent Assistance" = e61_orangelight,
                                 "Work Income" = e61_teallight, 
                                 "Pharmacutical Allowance" = "forestgreen")) 

  output$plot <- renderPlotly({ Income_Schedule })
  
  
  
  
  ######################################################################################
  ### EMTR Chart - private income 
  
  
  incomes_data_private <- calculator_function(Step_up, 1)
  
  
  incomes_changes <- incomes_data_private %>%
    mutate(across(-hours, ~. - lag(., default = first(.))), .names = "change_{.col}") %>%
    mutate(across(starts_with("change_"), ~if_else(row_number() == 1, 0, .)))
  
  incomes_ratios <- incomes_changes %>%
    mutate(across(-c(hours, work_income, starts_with("change_"), .names),
                  ~if_else(work_income == 0, 0, ./work_income)))
  
  
  all_ratios_long <- pivot_longer(incomes_ratios, 
                                  cols = c(income_tax, RA, JSP, ES, PP_Pay,
                                           net_fam_a_income, 
                                           net_fam_b_income, HECS_payment,
                                           medicare_levy, PA),
                                  names_to = "income_type", 
                                  values_to = "amount")
  
  all_ratios_long$income_type <- as.factor(all_ratios_long$income_type)
  levels(all_ratios_long$income_type) <- c("Energy Supplement", "HECS Payment",
                                           "Income Tax", 
                                           "Job Seeker Payment",
                                           "Medicare Levy", 
                                           "Family Tax Benefit A", 
                                           "Family Tax Benefit B", "Pharmacutical Allowance",
                                           "Parenting Payment", 
                                           "Commonwealth Rent Assistance", "Work Income")
  
  all_incomes_long$'Income Type' <- all_incomes_long$income_type
  
  all_incomes_long <- all_incomes_long %>%
    mutate(Amount = round(Amount, 2))
  
  incomes_data_private$'Net Income' <- incomes_data_private$net_income 
  
  # Creating the text string
  text_string <- paste("EMTRs for a", 
                       ifelse(partnered == 1, "partnered", "unpartnered"), 
                       ifelse(Home_owner == 1, "home owner", "renter"), 
                       "earning $", wage, "an hour, with", Numb_dep, "children,") 
  
 
  
  
  all_ratios_long$EMTR <- round(all_ratios_long$amount, 2)
  
  ### Rename Things 
  
  all_ratios_long$'Tax/Transfer' <- all_ratios_long$income_type
  incomes_data_private$'Private Income (000s)' <- incomes_data_private$hours
  
  all_ratios_long$'Private Income (000s)' <- as.numeric(all_ratios_long$hours)
  
  
  
  #### Calculate EMTRS and Average Tax Rates 
  all_ratios_long$EMTR <- abs(all_ratios_long$EMTR)
  all_ratios_long$'Total EMTR' <- round(abs(all_ratios_long$net_income - 1), 2)
  all_ratios_long$'Total EMTR' <- ifelse(all_ratios_long$`Private Income (000s)` == 0,
                                         0, all_ratios_long$'Total EMTR')
  
  incomes_data_private$'Average Tax Rate' <- 1 - round(incomes_data_private$net_income /
             (incomes_data_private$work_income + incomes_data_private$net_income[1]), 2)
  

  
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
  
  
  
  # Plotting
  EMTR_Schedule  <- ggplot() +
    geom_col(data = all_ratios_long, aes(x = `Private Income (000s)`,
                                         y = EMTR, fill = `Tax/Transfer`),
             width = 2) +
    geom_line(data = all_ratios_long, aes(x = `Private Income (000s)`, y = `Total EMTR` ), 
              col = "black" ) + 
    geom_line(data = incomes_data_private, aes( x = `Private Income (000s)`,
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
                                 "HECS Payment" = e61_bluedark, 
                                 "Income Tax" = e61_greydark, 
                                 "Job Seeker Payment" = e61_bluelight,
                                 "Medicare Levy" = e61_coraldark , 
                                 "Family Tax Benefit A" = e61_corallight, 
                                 "Family Tax Benefit B" = e61_maroondark,
                                 "Parenting Payment" = e61_orangedark, 
                                 "Commonwealth Rent Assistance" = e61_orangelight, 
                                 "Pharmacutical Allowance" = "forestgreen"))
  
  output$plot2 <- renderPlotly({ EMTR_Schedule })
  basis_text <- "income"
  basis_text2 <- "$1000 dollars of labour market income earned"
  }
  })
 
   
}
