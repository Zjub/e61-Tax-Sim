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
initial_setup <- reactiveValues(done = FALSE)

##########################################################################################
# INPUT FUNCTIONS 
# This section translates the inputs from the user into changes into the UI. 

##### Create a function which observes the number of dependents, and creates a box
##### for each dependent, and adds a descriptive with which child number they are. 

function(input, output, session) {
  # Observe the left number of dependents
  observeEvent(input$num_dependents, {
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
  
  # Observe the right number of dependents
  observeEvent(input$num_dependents2, {
    output$child_age_inputs2 <- renderUI({
      num <- input$num_dependents2
      if (num == 0) {
        return(NULL)
      } else {
        lapply(1:num, function(i) {
          numericInput(paste0("child_age2_", i), paste0("Dependent ", i, " Age (Right)"),
                       value = 0, min = 0, max = 19)
        })
      }
    })
  })

  output$notch_text <- renderText({""})
##### Create the text that displays below the charts to describe the results. 
##### These are intended to be dynamic - i.e. the the text updates depending on
####  the model inputs. Note that at this stage the only dynamic text is "income" or "hours'

  observe({
    if (!initial_setup$done) {
      # Set label and max_value based on the initial basis
      if (input$basis == "hours") {
        max_value <- input$max_hours
        label_text <- "Comparison at a number of hours worked:"
      } else if (input$basis == "income") {
        max_value <- input$max_income / 1000
        label_text <- "Comparison at work income (In $Thousands):"
      }
      
      # Update the numeric input with the initial label and range
      updateNumericInput(session, "compare_value", label = label_text, min = 0, max = max_value, step = 1000)
      
      # Set the flag to TRUE after the initial setup
      initial_setup$done <- TRUE
    }
  })
  
  observeEvent(input$update_input, {
    # Set label and max_value based on the basis
    if (input$basis == "hours") {
      max_value <- input$max_hours
      label_text <- "Comparison at a number of hours worked:"
    } else if (input$basis == "income") {
      max_value <- input$max_income
      label_text <- "Comparison at work income (In $Thousands):"
    }
    
    # Update the numeric input with the new label and range
    updateNumericInput(session, "compare_value", label = label_text, min = 0, max = max_value, step = 1000)
  })
  
  observeEvent(c(input$update_input), {
  # Import parameters for the given quarter ----
    output$Title_1 <- renderUI({
      HTML("<b>How do I interpret these charts?</b>")
    })
    
    output$comparison_text <- renderText({""})
    output$comparison_text2 <- renderText({""})
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
  
  
  # Define a helper function to process inputs, using suffix only for input references
  process_inputs <- function(person_suffix = "") {
    
    # Define input names with suffix if applicable
    partnered_input <- paste0("partnered", person_suffix)
    print(partnered_input)
    partner_income_input <- paste0("partner_income", person_suffix)
    num_dependents_input <- paste0("num_dependents", person_suffix)
    renter_input <- paste0("renter", person_suffix)
    weekly_rent_input <- paste0("weekly_rent", person_suffix)
    basis_input <- "basis" 
    wages_input <- paste0("wages", person_suffix)
    print(wages_input)
     max_hours_input <- paste0("max_hours")
    max_income_input <- paste0("max_income")
    hecs_debt_input <- paste0("hecs_debt", person_suffix)
    total_hecs_debt_input <- paste0("total_hecs_debt", person_suffix)
    
    # Assign globally without suffix for consistent variable names
    partnered <<- ifelse(input[[partnered_input]] == TRUE, 1, 0)
    print(input[[partnered_input]])
    partner_earnings <<- ifelse(partnered == 1, input[[partner_income_input]], 0)
    Have_dep <<- ifelse(input[[num_dependents_input]] > 0, 1, 0)
    num_dependents <- input[[num_dependents_input]]
    Home_owner <<- ifelse(input[[renter_input]] == TRUE, 0, 1)
    Rent <<- ifelse(input[[renter_input]] == TRUE, input[[weekly_rent_input]] * 2, 0)  # Fortnightly rent
    
    # Create vector for child ages
    if (num_dependents == 0) {
      child_ages <- integer(0)  # Return a zero-length integer vector if no dependents
    } else {
      child_ages <- sapply(1:num_dependents, function(i) input[[paste0("child_age", person_suffix, "_", i)]])
    }
    
    child_age <<- child_ages
    Numb_dep <<- length(child_ages[child_ages < 20])
    young_child <<- if (length(child_ages) > 0) min(child_ages) else 1000
    
    # Labour market characteristics
    wage <<- ifelse(input[[basis_input]] == "hours", input[[wages_input]], 25)
    max_hours <<- ifelse(input[[basis_input]] == "hours", input[[max_hours_input]], input[[max_income_input]])
    max_private_earnings <<- ifelse(input[[basis_input]] == "income", input$max_income / 1000, 100)
    max_range <<- round(ifelse(input[[basis_input]] == "hours", input[[max_hours_input]], input$max_income / 1000))
    
    # Hard-coded characteristics
    Carer <<- 0
    Disability <<- 0
    Main_carer_dep <<- 1
    over_60 <<- 0
    living_alone <<- 0
    ben_eligibility <<- 1
    partial_cap <<- 0
    
    # Parenting payment eligibility
    PPeligible <<- ifelse((young_child <= 6 | (young_child <= 8 & partnered == 0)) & Main_carer_dep == 1, 1, 0)
    
    # HECS Debt
    HECS_on <<- ifelse(input[[hecs_debt_input]] == TRUE, 1, 0)
    HECSDebt <<- ifelse(HECS_on == 1, input[[total_hecs_debt_input]], 0)
  }
  
##### medicare levy   
  Medicare_levy_on <<- 1
  work_for_the_dole <<- 0
  living_alone <<- 1 
  SAPTO_on <<- 1 
##### Rent assistance abatement   
  RA_Abate <<-1 

#### BTO
    
  BTO_on <<- 1
  
#### INCOME TAX BRACKETS 
  
  edited_tax_brackets <<- 0

  
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
  
  
  run_calc_over <- c("", "2")
  
  
############# HOURS WORKED ##############################################################3
  
  if (input$basis == "hours") {
    print(input[["wages2"]])
for(i in run_calc_over){
process_inputs(i)
  print(partnered)
print(wage)
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
  text_string2 <- paste("Income schedule for person", ifelse(i == "", "1", "2")) 
  
  text_string <- "Income Schedule"
  
  # Plotting the Hours Schedule. 
  Hours_Schedule_full <- ggplot() +
    geom_col(data = all_incomes_long,
             aes(x = hours, y = Amount / 1000  , fill = `Income Type`), width = 0.5 ) +
    geom_line(data = incomes_data_hourly, aes(x = hours, y = `Net Income` ), 
              size = 2, col = "black") +
    labs_e61(title = text_string2,
             x = "Hours worked",
             y = "$ (000's)",
             fill = "Income Type",
             colour = "") + add_baseline()  + scale_fill_e61() + 
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
  
  
  
  incomes_data_hourly$`Person Number` <- ifelse(i == "", "Person 1", "Person 2")
  assign(paste0("incomes_data_hourly_", i), incomes_data_hourly)
  assign(paste0("Hours_Schedule_full_", i), Hours_Schedule_full)

} 
    combined_incomes_data <- bind_rows(incomes_data_hourly_, incomes_data_hourly_2)
    
    min_amount <- 0  
    
    max_net_income <- ( max(combined_incomes_data$work_income, na.rm = TRUE) + 
                          max(combined_incomes_data$net_fam_a_income, na.rm = TRUE)
                        + max(combined_incomes_data$net_fam_b_income, na.rm = TRUE) + 
                          max(combined_incomes_data$PP_Pay, na.rm = TRUE) + 
                          max(combined_incomes_data$JSP, na.rm = TRUE)+ 
                          max(combined_incomes_data$PA, na.rm = TRUE)) / 1000 - min_amount
    

    
    
  Hours_Schedule_simple <- ggplot()  +
    geom_line(data = combined_incomes_data, aes(x = hours, y = `Net Income`, col = `Person Number` )) +
    labs_e61(title = text_string,
             x = "Hours worked",
             y = "$ (000's)",
             fill = "Income Type",
             colour = "") + add_baseline() +
    scale_x_continuous(expand = c(0, Inf)) + # Remove default expansion for x-axis
    scale_y_continuous(expand = c(0, 0), limits =c(min_amount, max_net_income)) 
  
  min_amount <- (min(combined_incomes_data$income_tax, na.rm = TRUE) + 
                   min(combined_incomes_data$HECS_payment, na.rm = TRUE) + 
                   min(combined_incomes_data$medicare_levy, na.rm = TRUE)) / 1000 

  # Use renderPlotly to switch between them
  output$plot <- renderPlotly({ 
    plot_to_display <-  Hours_Schedule_simple
    ggplotly(plot_to_display)
  })
  
  Hours_Schedule_full_ <- Hours_Schedule_full_ +   scale_x_continuous(expand = c(0, Inf)) + # Remove default expansion for x-axis
    scale_y_continuous(expand = c(0, 0), limits =c(min_amount, max_net_income)) 
  
  Hours_Schedule_full_2 <- Hours_Schedule_full_2 +   scale_x_continuous(expand = c(0, Inf)) + # Remove default expansion for x-axis
    scale_y_continuous(expand = c(0, 0), limits =c(min_amount, max_net_income)) 
  
  output$plot_detailed_hours_1 <- renderPlotly({ ggplotly(Hours_Schedule_full_) })
  output$plot_detailed_hours_2 <- renderPlotly({ ggplotly(Hours_Schedule_full_2) })
  
  output$comparison_text <- renderUI({
    req(input$compare_value)  # Ensure compare_value has a value
    
    # Determine the prefix text based on the selected basis
    prefix_text <- paste("At <b>", input$compare_value, "</b> hours worked:")
       # Retrieve and format Person 1 and Person 2 incomes, handling non-numeric cases
    person1_income <- combined_incomes_data %>%
      filter(`Person Number` == "Person 1" & hours == input$compare_value) %>%
      pull(`Net Income`) %>%
      first() %>%
      { if(is.numeric(.)) . * 1000 else NA } %>%  # Multiply by 1000 if numeric, else return NA
      scales::dollar(na.rm = TRUE)  # Format as dollar, handle NA gracefully
    
    person2_income <- combined_incomes_data %>%
      filter(`Person Number` == "Person 2" & hours == input$compare_value) %>%
      pull(`Net Income`) %>%
      first() %>%
      { if(is.numeric(.)) . * 1000 else NA } %>%  # Multiply by 1000 if numeric, else return NA
      scales::dollar(na.rm = TRUE)  # Format as dollar, handle NA gracefully
    
    Person_taxes <- combined_incomes_data %>%
      filter(`Person Number` == "Person 1" & hours == input$compare_value) %>%
      summarise(total = sum(medicare_levy, income_tax, na.rm = TRUE)) %>%
      pull(total)
    Person_taxes <- Person_taxes * -1 
    person_benefits <- combined_incomes_data %>%
      filter(`Person Number` == "Person 1" & hours == input$compare_value) %>%
      summarise(total = sum(net_fam_a_income, net_fam_b_income, PP_Pay, JSP, PA, RA, ES, na.rm = TRUE)) %>%
      pull(total)
    
    Person_taxes2 <- combined_incomes_data %>%
      filter(`Person Number` == "Person 2" & hours == input$compare_value) %>%
      summarise(total = sum(medicare_levy, income_tax, na.rm = TRUE)) %>%
      pull(total)
    Person_taxes2 <- Person_taxes2 * -1 
    person_benefits2 <- combined_incomes_data %>%
      filter(`Person Number` == "Person 2" & hours == input$compare_value) %>%
      summarise(total = sum(net_fam_a_income, net_fam_b_income, PP_Pay, JSP, PA, RA, ES, na.rm = TRUE)) %>%
      pull(total)
    
    person_benefits2 <- round(person_benefits2, digits = 2)
    person_benefits <- round(person_benefits, digits = 2)
    Person_taxes2 <- round(Person_taxes2, digits = 2)
    Person_taxes <- round(Person_taxes, digits = 2)
    
    # Construct the output text with bold formatting
    HTML(paste0(
      prefix_text,
      "<br>Person 1's total income is <b>", person1_income, "</b> a year. ", "They pay $", Person_taxes, " in taxes ",
      "and recieve $", person_benefits, " in total benefits.",  
      " While Person 2's total income is <b>", person2_income, "</b> a year. ", "They pay $", Person_taxes2, " in taxes ", 
      "and recieve $", person_benefits2, " in total benefits." 
    ))
  })
  
  ######################################################################################
  ### EMTR Chart - hours worked. 
  
  ### Increments of 1 rather than 0.1 
  for(i in run_calc_over){
    process_inputs(i)
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
  text_string2 <- paste("EMTR for Person", ifelse(i == "", "1", "2")) 
  
  # Plotting
  EMTR_Schedule_complex  <- ggplot() +
    geom_col(data = all_ratios_long, aes(x = hours,
                                         y = EMTR, fill = `Tax/Transfer`),
             width = 2)  + 
    geom_line(data = incomes_data_hourly, aes( x = hours, y = `Average Tax Rate`),
              col = "grey", linetype = "dashed") + 
    scale_colour_manual(values = c("Net Income" = "black")) +
    labs_e61(title = text_string2,
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
  
  all_ratios_long$`Person Number` <- ifelse(i == "", "Person 1", "Person 2")
  assign(paste0("all_ratios_long_", i), all_ratios_long)
  incomes_data_hourly$`Person Number` <- ifelse(i == "", "Person 1", "Person 2")
  assign(paste0("incomes_data_hourly_", i), incomes_data_hourly)
  
  assign(paste0("EMTR_Schedule_complex", ifelse(i == "", "1", "2")), EMTR_Schedule_complex)
  
  }
  
  
  combined_ratios <- bind_rows(all_ratios_long_, all_ratios_long_2)
  combined_incomes <- bind_rows(incomes_data_hourly_, incomes_data_hourly_2)
  
  
  EMTR_Schedule_simple <- ggplot() +
    geom_line(data = combined_ratios, aes(x = hours, y = `Total EMTR`, col = `Person Number` ) ) + 
    geom_line(data = combined_incomes, aes( x = hours, y = `Average Tax Rate`, col = `Person Number`),
              linetype = "dashed")  +
    labs_e61(title = "EMTR from working an additional hour a week",
             x = "Hours Worked",
             y = "EMTR",
             fill = "Income Type",
             colour = "") + add_baseline()  + scale_fill_e61() + 
    geom_hline(yintercept = 1, linetype = "dashed", col = "red")
  
  max_EMTR <- max(max(combined_ratios$`Total EMTR`), 1)
  
  EMTR_Schedule_complex1 <-   EMTR_Schedule_complex1+   scale_x_continuous(expand = c(0, Inf)) + 
    scale_y_continuous(expand = c(0, 0), limits =c(0, max_EMTR)) 
  
  EMTR_Schedule_complex2 <-   EMTR_Schedule_complex2 +   scale_x_continuous(expand = c(0, Inf)) + 
    scale_y_continuous(expand = c(0, 0), limits =c(0, max_EMTR)) 
  
  output$plot2 <- renderPlotly({ 
    plot_to_display <- EMTR_Schedule_simple 
    ggplotly(plot_to_display)
  })
  
  output$plot2_detailed <- renderPlotly({ 
    plot_to_display <- EMTR_Schedule_complex1
    ggplotly(plot_to_display)
  })
  
  output$plot2_detailed2 <- renderPlotly({ 
    plot_to_display2 <- EMTR_Schedule_complex2 
    ggplotly(plot_to_display2)
  })

  notch_explanations <- list()
  
  # Condition 1: When gross_fam_income first crosses fam_ftba_eoy_incthreshold
  ftba_eoy_rows <- combined_incomes %>%
    group_by(`Person Number`) %>%
    filter(work_income + partner_earnings > fam_ftba_eoy_incthreshold) %>%
    slice_min(hours) %>%  # Get the first instance by hours
    mutate(Explanation = paste0("the person crossing the income threshold to recieve the FTB A End of Year Supplement.
                                As such, they lose the entire payment of $", fam_ftba_eoy_supp, " per year for for each eligible child")) %>%
    select(hours, `Person Number`, Explanation)
  
  # Add the results to the list
  notch_explanations[[1]] <- ftba_eoy_rows
  
  # Condition 2: When JSP + PP_Pay first reaches zero
  income_test_ftba_rows <- combined_incomes %>%
    group_by(`Person Number`) %>%
    filter((JSP + PP_Pay) == 0) %>%
    slice_min(hours) %>%  # Get the first instance by hours
    mutate(Explanation = "the person abateing off their main income support payment. As a result, they now have the income test applied
           to their FTB A payment") %>%
    select(hours, `Person Number`, Explanation)
  
  # Add the results to the list
  notch_explanations[[2]] <- income_test_ftba_rows
  
  # Condition 3: When gross_fam_income first crosses fam_b_threshold
  ftbb_rows <- combined_incomes %>%
    group_by(`Person Number`) %>%
    filter(gross_fam_income > fam_b_threshold) %>%
    slice_min(hours) %>%  # Get the first instance by hours
    mutate(Explanation = "the person crossing the income threshold for FTB B") %>%
    select(hours, `Person Number`, Explanation)
  
  # Add the results to the list
  notch_explanations[[3]] <- ftbb_rows
  
  # Combine all conditions into the final Possible_Notch_Explanations dataframe
  Possible_Notch_Explanations <- bind_rows(notch_explanations)
  
  print(Possible_Notch_Explanations)
  output$notch_text <- renderUI({
    # Filter combined_ratios for rows where Total EMTR is greater than 1 and keep distinct hours and Person Number
    high_emtr_rows <- combined_ratios %>%
      filter(`Total EMTR` > 1) %>%
      distinct(hours, `Person Number`, .keep_all = TRUE) %>%
      left_join(Possible_Notch_Explanations, by = c("hours", "Person Number"))
    
    # Check if any rows meet the condition
    if (nrow(high_emtr_rows) == 0) {
      return("No notches detected.")  # Return message if no rows meet the condition
    }
    
    # Group by Person Number and create messages
    grouped_messages <- high_emtr_rows %>%
      split(.$`Person Number`) %>%  # Split the dataframe by Person Number
      lapply(function(person_data) {
        # Header for each person
        person_number <- unique(person_data$`Person Number`)
        person_message <- paste0(person_number, " has the following notches:<br>")
        
        # Create messages for each notch for this person
        notch_messages <- lapply(1:nrow(person_data), function(i) {
          hours <- person_data$hours[i]
          total_emtr <- person_data$`Total EMTR`[i]
          emtr_percent <- round(total_emtr * 100, 2)  # Convert to percentage and round to 2 decimal places
          explanation <- person_data$Explanation[i]
          
          # Construct the notch message
          message <- paste0(
            "At ", hours, " hours worked: the person has an EMTR of ", 
            round(total_emtr, 2), ", which means they lose ", 
            emtr_percent, "% of income earned from the additional hour of work due to taxes paid and benefits withdrawn."
          )
          
          # Add explanation if it exists
          if (!is.na(explanation)) {
            message <- paste0(message, " This is likely due to ", explanation, ".")
          }
          
          message
        })
        
        # Combine all notch messages for this person, separated by line breaks
        paste(person_message, paste(notch_messages, collapse = "<br>"), sep = "<br>")
      })
    
    # Combine all grouped messages into a single HTML output
    HTML(paste(grouped_messages, collapse = "<br><br>"))
  })
  output$comparison_text2 <- renderUI({
    req(input$compare_value)  # Ensure compare_value has a value
    
    # Determine the prefix text based on the selected basis
    # Retrieve and format Person 1 and Person 2 incomes, handling non-numeric cases
    person1_EMTR <- combined_ratios %>%
      filter(`Person Number` == "Person 1" & hours == input$compare_value) %>%
      pull(`Total EMTR`) %>%
      first() 
    person1_EMTR_percent <- person1_EMTR * 100 
    
    person2_EMTR <- combined_ratios %>%
      filter(`Person Number` == "Person 2" & hours == input$compare_value) %>%
      pull(`Total EMTR`) %>%
      first() 
    
    Additional_hour <- input$compare_value - 1  
    person1_EMTR <- round(person1_EMTR, digits = 2)
    person1_EMTR_percent <- person1_EMTR * 100
    person2_EMTR <- round(person2_EMTR, digits = 2)
    person2_EMTR_percent <- person2_EMTR * 100
    # Construct the output text with bold formatting
    HTML(ifelse(input$compare_value > 0, paste0(
      "<br>Person 1's Effective Marginal Tax Rate is <b>", person1_EMTR, "</b>. This means that 
      by moving from working ", Additional_hour, " to ", input$compare_value, " hours the person lost ", 
      person1_EMTR_percent, "% of their additional work income earned in taxes paid and benefit withdrawals.", 
      "<br>Person 2's Effective Marginal Tax Rate is <b>", person2_EMTR, "</b>. This means that 
      by moving from working ", Additional_hour, " to ", input$compare_value, " hours the person lost ", 
      person2_EMTR_percent, "% of their additional work income earned in taxes paid and benefit withdrawals."), "")
      
    )
  })
  
 
  
#### Ensure that the text is correct to describe the charts. 
  
  basis_text <- "hours"
  basis_text2 <- "hour worked"
  
 
###### Income Basis #####################################################################  
  } else if (input$basis == "income") {
    for(i in run_calc_over){
      process_inputs(i)
  
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
  text_string <- paste("Income schedule for Person", ifelse(i == "", "1", "2"))
  
  
  
  # Plotting the Hours Schedule. 
  Income_Schedule_Detailed <- ggplot() +
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
  incomes_data_private$`Person Number` <- ifelse(i == "", "Person 1", "Person 2")
  assign(paste0("incomes_data_hourly_", i), incomes_data_private)
  assign(paste0("Hours_Schedule_full_", i), Income_Schedule_Detailed)
  
} 
    combined_incomes_data <- bind_rows(incomes_data_hourly_, incomes_data_hourly_2)
    
    min_amount <- 0  
    
    max_net_income <- ( max(combined_incomes_data$work_income, na.rm = TRUE) + 
                          max(combined_incomes_data$net_fam_a_income, na.rm = TRUE)
                        + max(combined_incomes_data$net_fam_b_income, na.rm = TRUE) + 
                          max(combined_incomes_data$PP_Pay, na.rm = TRUE) + 
                          max(combined_incomes_data$JSP, na.rm = TRUE)+ 
                          max(combined_incomes_data$PA, na.rm = TRUE)) / 1000 - min_amount
    Hours_Schedule_simple <- ggplot()  +
      geom_line(data = combined_incomes_data, aes(x = hours, y = `Net Income`, col = `Person Number` )) +
      labs_e61(title = text_string,
               x = "Hours worked",
               y = "$ (000's)",
               fill = "Income Type",
               colour = "") + add_baseline() +
      scale_x_continuous(expand = c(0, Inf)) + # Remove default expansion for x-axis
      scale_y_continuous(expand = c(0, 0), limits =c(min_amount, max_net_income)) 
    
    min_amount <- (min(combined_incomes_data$income_tax, na.rm = TRUE) + 
                     min(combined_incomes_data$HECS_payment, na.rm = TRUE) + 
                     min(combined_incomes_data$medicare_levy, na.rm = TRUE)) / 1000 
    
    # Use renderPlotly to switch between them
    output$plot <- renderPlotly({ 
      plot_to_display <-  Hours_Schedule_simple
      ggplotly(plot_to_display)
    })
    
    Hours_Schedule_full_ <- Hours_Schedule_full_ +   scale_x_continuous(expand = c(0, Inf)) + # Remove default expansion for x-axis
      scale_y_continuous(expand = c(0, 0), limits =c(min_amount, max_net_income)) 
    
    Hours_Schedule_full_2 <- Hours_Schedule_full_2 +   scale_x_continuous(expand = c(0, Inf)) + # Remove default expansion for x-axis
      scale_y_continuous(expand = c(0, 0), limits =c(min_amount, max_net_income)) 
    
    output$plot_detailed_hours_1 <- renderPlotly({ ggplotly(Hours_Schedule_full_) })
    output$plot_detailed_hours_2 <- renderPlotly({ ggplotly(Hours_Schedule_full_2) })
  
    output$comparison_text <- renderUI({
      req(input$compare_value)  # Ensure compare_value has a value
      in_thousands <- input$compare_value * 1000 
      # Determine the prefix text based on the selected basis
      prefix_text <- paste("At <b> $", in_thousands, "</b> of private income earned:")
      # Retrieve and format Person 1 and Person 2 incomes, handling non-numeric cases
      person1_income <- combined_incomes_data %>%
        filter(`Person Number` == "Person 1" & hours == input$compare_value) %>%
        pull(`Net Income`) %>%
        first() %>%
        { if(is.numeric(.)) . * 1000 else NA } %>%  # Multiply by 1000 if numeric, else return NA
        scales::dollar(na.rm = TRUE)  # Format as dollar, handle NA gracefully
      
      person2_income <- combined_incomes_data %>%
        filter(`Person Number` == "Person 2" & hours == input$compare_value) %>%
        pull(`Net Income`) %>%
        first() %>%
        { if(is.numeric(.)) . * 1000 else NA } %>%  # Multiply by 1000 if numeric, else return NA
        scales::dollar(na.rm = TRUE)  # Format as dollar, handle NA gracefully
      
      Person_taxes <- combined_incomes_data %>%
        filter(`Person Number` == "Person 1" & hours == input$compare_value) %>%
        summarise(total = sum(medicare_levy, income_tax, na.rm = TRUE)) %>%
        pull(total)
      Person_taxes <- Person_taxes * -1 
      person_benefits <- combined_incomes_data %>%
        filter(`Person Number` == "Person 1" & hours == input$compare_value) %>%
        summarise(total = sum(net_fam_a_income, net_fam_b_income, PP_Pay, JSP, PA, RA, ES, na.rm = TRUE)) %>%
        pull(total)
      
      Person_taxes2 <- combined_incomes_data %>%
        filter(`Person Number` == "Person 2" & hours == input$compare_value) %>%
        summarise(total = sum(medicare_levy, income_tax, na.rm = TRUE)) %>%
        pull(total)
      Person_taxes2 <- Person_taxes2 * -1 
      person_benefits2 <- combined_incomes_data %>%
        filter(`Person Number` == "Person 2" & hours == input$compare_value) %>%
        summarise(total = sum(net_fam_a_income, net_fam_b_income, PP_Pay, JSP, PA, RA, ES, na.rm = TRUE)) %>%
        pull(total)
      
      person_benefits2 <- round(person_benefits2, digits = 2)
      person_benefits <- round(person_benefits, digits = 2)
      Person_taxes2 <- round(Person_taxes2, digits = 2)
      Person_taxes <- round(Person_taxes, digits = 2)
      
      # Construct the output text with bold formatting
      HTML(paste0(
        prefix_text,
        "<br>Person 1's total income is <b>", person1_income, "</b> a year. ", "They pay $", Person_taxes, " in taxes ",
        "and recieve $", person_benefits, " in total benefits.",  
        " While Person 2's total income is <b>", person2_income, "</b> a year. ", "They pay $", Person_taxes2, " in taxes ", 
        "and recieve $", person_benefits2, " in total benefits." 
      ))
    })
  
  ######################################################################################
  ### EMTR Chart - private income 
    for(i in run_calc_over){
      process_inputs(i)
  
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
  

  
  text_string2 <- paste("EMTR for Person", ifelse(i == "", "1", "2")) 
  
  
  
  # Plotting
  EMTR_Schedule_complex  <- ggplot() +
    geom_col(data = all_ratios_long, aes(x = `Private Income (000s)`,
                                         y = EMTR, fill = `Tax/Transfer`),
             width = 2) + 
    geom_line(data = incomes_data_private, aes( x = `Private Income (000s)`,
                                                                y = `Average Tax Rate`),
              col = "grey", linetype = "dashed")  +
    labs_e61(title = text_string2,
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
  
  all_ratios_long$`Person Number` <- ifelse(i == "", "Person 1", "Person 2")
  assign(paste0("all_ratios_long_", i), all_ratios_long)
  incomes_data_private$`Person Number` <- ifelse(i == "", "Person 1", "Person 2")
  assign(paste0("incomes_data_hourly_", i), incomes_data_private)
  
  assign(paste0("EMTR_Schedule_complex", ifelse(i == "", "1", "2")), EMTR_Schedule_complex)
  
  }
  
  
  combined_ratios <- bind_rows(all_ratios_long_, all_ratios_long_2)
  combined_incomes <- bind_rows(incomes_data_hourly_, incomes_data_hourly_2)
  
  EMTR_Schedule_simple <- ggplot() +
    geom_line(data = combined_ratios, aes(x = hours, y = `Total EMTR`, col = `Person Number` ) ) + 
    geom_line(data = combined_incomes, aes( x = hours, y = `Average Tax Rate`, col = `Person Number`),
              linetype = "dashed")  +
    labs_e61(title = "EMTR from working earning an additional $1000",
             x = "Hours Worked",
             y = "EMTR",
             fill = "Income Type",
             colour = "") + add_baseline()  + scale_fill_e61() + 
    geom_hline(yintercept = 1, linetype = "dashed", col = "red")
  
  max_EMTR <- max(max(combined_ratios$`Total EMTR`), 1)
  
  EMTR_Schedule_complex1 <-   EMTR_Schedule_complex1+   scale_x_continuous(expand = c(0, Inf)) + 
    scale_y_continuous(expand = c(0, 0), limits =c(0, max_EMTR)) 
  
  EMTR_Schedule_complex2 <-   EMTR_Schedule_complex2 +   scale_x_continuous(expand = c(0, Inf)) + 
    scale_y_continuous(expand = c(0, 0), limits =c(0, max_EMTR)) 
  
  output$plot2 <- renderPlotly({ 
    plot_to_display <- EMTR_Schedule_simple 
    ggplotly(plot_to_display)
  })
  
  output$plot2_detailed <- renderPlotly({ 
    plot_to_display <- EMTR_Schedule_complex1
    ggplotly(plot_to_display)
  })
  
  output$plot2_detailed2 <- renderPlotly({ 
    plot_to_display2 <- EMTR_Schedule_complex2 
    ggplotly(plot_to_display2)
  })
  
  notch_explanations <- list()
  
  # Condition 1: When gross_fam_income first crosses fam_ftba_eoy_incthreshold
  ftba_eoy_rows <- combined_incomes %>%
    group_by(`Person Number`) %>%
    filter(work_income + partner_earnings > fam_ftba_eoy_incthreshold) %>%
    slice_min(hours) %>%  # Get the first instance by hours
    mutate(Explanation = paste0("the person crossing the income threshold to recieve the FTB A End of Year Supplement.
                                As such, they lose the entire payment of $", fam_ftba_eoy_supp, " per year for for each eligible child")) %>%
    select(hours, `Person Number`, Explanation)
  ftba_eoy_rows$hours <- ftba_eoy_rows$hours - 1 
  # Add the results to the list
  notch_explanations[[1]] <- ftba_eoy_rows
  
  # Condition 2: When JSP + PP_Pay first reaches zero
  income_test_ftba_rows <- combined_incomes %>%
    group_by(`Person Number`) %>%
    filter((JSP + PP_Pay + PA) == 0) %>%
    slice_min(hours) %>%  # Get the first instance by hours
    mutate(Explanation = "the person abateing off their main income support payment. As a result, they now have the income test applied
           to their FTB A payment") %>%
    select(hours, `Person Number`, Explanation)
  
  # Add the results to the list
  notch_explanations[[2]] <- income_test_ftba_rows
 
  # Condition 3: When gross_fam_income first crosses fam_b_threshold
  ftbb_rows <- combined_incomes %>%
    group_by(`Person Number`) %>%
    filter(gross_fam_income > fam_b_threshold) %>%
    slice_min(hours) %>%  # Get the first instance by hours
    mutate(Explanation = "the person crossing the income threshold for FTB B") %>%
    select(hours, `Person Number`, Explanation)
  
  # Add the results to the list
  notch_explanations[[3]] <- ftbb_rows
  
  # Combine all conditions into the final Possible_Notch_Explanations dataframe
  Possible_Notch_Explanations <- bind_rows(notch_explanations)
  
  print(Possible_Notch_Explanations)
  
  
  output$notch_text <- renderUI({
    # Filter combined_ratios for rows where Total EMTR is greater than 1 and keep distinct hours and Person Number
    high_emtr_rows <- combined_ratios %>%
      filter(`Total EMTR` > 1) %>%
      distinct(hours, `Person Number`, .keep_all = TRUE) %>%
      left_join(Possible_Notch_Explanations, by = c("hours", "Person Number"))
    
    # Check if any rows meet the condition
    if (nrow(high_emtr_rows) == 0) {
      return("No notches detected.")  # Return message if no rows meet the condition
    }
    
    # Group by Person Number and create messages
    grouped_messages <- high_emtr_rows %>%
      split(.$`Person Number`) %>%  # Split the dataframe by Person Number
      lapply(function(person_data) {
        # Header for each person
        person_number <- unique(person_data$`Person Number`)
        person_message <- paste0(person_number, " has the following notches:<br>")
        
        # Create messages for each notch for this person
        notch_messages <- lapply(1:nrow(person_data), function(i) {
          hours <- person_data$hours[i] * 1000
          total_emtr <- person_data$`Total EMTR`[i]
          emtr_percent <- round(total_emtr * 100, 2)  # Convert to percentage and round to 2 decimal places
          explanation <- person_data$Explanation[i]
          
          # Construct the notch message
          message <- paste0(
            "At $", hours, " of private income: the person has an EMTR of ", 
            round(total_emtr, 2), ", which means they lose ", 
            emtr_percent, "% of the additional $1000 earned to taxes paid and benefits withdrawn."
          )
          
          # Add explanation if it exists
          if (!is.na(explanation)) {
            message <- paste0(message, " This is likely due to ", explanation, ".")
          }
          
          message
        })
        
        # Combine all notch messages for this person, separated by line breaks
        paste(person_message, paste(notch_messages, collapse = "<br>"), sep = "<br>")
      })
    
    # Combine all grouped messages into a single HTML output
    HTML(paste(grouped_messages, collapse = "<br><br>"))
  })
    
 
  
  output$comparison_text2 <- renderUI({
    req(input$compare_value)  # Ensure compare_value has a value
    
    # Determine the prefix text based on the selected basis
    # Retrieve and format Person 1 and Person 2 incomes, handling non-numeric cases
    person1_EMTR <- combined_ratios %>%
      filter(`Person Number` == "Person 1" & hours == input$compare_value) %>%
      pull(`Total EMTR`) %>%
      first() 
    person1_EMTR_percent <- person1_EMTR * 100 
    
    person2_EMTR <- combined_ratios %>%
      filter(`Person Number` == "Person 2" & hours == input$compare_value) %>%
      pull(`Total EMTR`) %>%
      first() 
    
    Additional_dollar <- input$compare_value * 1000 - 1000  
    Current_dollar <- input$compare_value * 1000 
    person1_EMTR <- round(person1_EMTR, digits = 2)
    person1_EMTR_percent <- person1_EMTR * 100
    person2_EMTR <- round(person2_EMTR, digits = 2)
    person2_EMTR_percent <- person2_EMTR * 100
    # Construct the output text with bold formatting
    HTML(ifelse(input$compare_value > 0, paste0(
      "<br>Person 1's Effective Marginal Tax Rate is <b>", person1_EMTR, "</b>. This means that 
      by moving from earning $", Additional_dollar, " to $", Current_dollar, " the person lost ", 
      person1_EMTR_percent, "% of the $1000 income earned in taxes paid and benefit withdrawals.", 
      "<br>Person 2's Effective Marginal Tax Rate is <b>", person2_EMTR, "</b>. This means that 
      by moving from earning $", Additional_dollar, " to ", Current_dollar, " the person lost $", 
      person2_EMTR_percent, "% of the $1000 income earned in taxes paid and benefit withdrawals."), "")
      
    )
  })
  
  
  }
  })
 
   
}
