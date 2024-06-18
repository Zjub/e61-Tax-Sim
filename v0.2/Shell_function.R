

calc_income_or_hours <- function(income_or_hours){
  
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
  
          if(income_or_hours == 0){
          
          #### Produce earnings chart and EMTR chart based upon hours worked. 
          
        if(max_hours < 1){
          print("Please allow the individual to consider working more than one hour")
        }  
        else{  
          #####################################################################################################
          #### Earnings Chart - By hours worked. 
          
          #### Looping through hours worked, based upon wage, from 0 to max_hours, in 
          #### incriments of 0.1, to allow for a reasonably smooth EMTR curve. 
          
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
          text_string <- paste("Taxes and transfers for the", 
                               ifelse(partnered == 1, "partnered", "unpartnered"), 
                               ifelse(Home_owner == 1, "home owner", "renter")) 
          
          # Append Parenting Payment Benefit eligibility for the title of the chart 
          if (PPeligible == 1 & Numb_dep > 0) {
            text_string <- paste(text_string, ",eligible for the Parenting Payment")
          } else if (PPeligible == 0 & Numb_dep > 0) {
            text_string <- paste(text_string, ", not eligible for the Parenting Payment")
          }
          
          max_net_income <- max(incomes_data_hourly_highly_detailed$net_income, na.rm = TRUE) * 1.9
          min_amount <- min(all_incomes_long$Amount, na.rm = TRUE) * 1.4
          
          # Plotting the Hours Schedule. 
          Hours_Schedule <- ggplot() +
            geom_col(data = all_incomes_long,
                     aes(x = hours, y = Amount  , fill = `Income Type`), width = 0.5 ) +
            geom_line(data = incomes_data_hourly_highly_detailed, aes(x = hours, y = `Net Income` ), 
                      size = 2, col = "black") +
            labs_e61(title = text_string,
                     x = "Hours worked",
                     y = "$",
                     fill = "Income Type",
                     colour = "") + add_baseline()  + scale_fill_e61() +
            scale_x_continuous(expand = c(0, Inf)) + # Remove default expansion for x-axis
            scale_y_continuous(expand = c(0, 0), limits =c(min_amount, max_net_income)) 
        
          ######################################################################################
          ### EMTR Chart - hours worked. 
          
          
          incomes_data_hourly_highly_detailed <- highly_detailed_variable_calculator(wage, 1)
          
          
          incomes_changes <- incomes_data_hourly_highly_detailed %>%
            mutate(across(-hours, ~abs(. - lag(., default = first(.)))), .names = "change_{.col}") %>%
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
          levels(all_incomes_long$income_type) <- c("Energy Supplement", "HECS Payment", "Income Tax", 
                                                    "Job Seeker Payment", "Medicare Levy", 
                                                    "Family Tax Benefit A", 
                                                    "Family Tax Benefit B", "Parenting Payment", 
                                                    "Commonwealth Rent Assistance", "Work Income")
          
          all_incomes_long$'Income Type' <- all_incomes_long$income_type
          
          all_incomes_long <- all_incomes_long %>%
            mutate(Amount = round(Amount, 2))
          
          incomes_data_hourly_highly_detailed$'Net Income' <-  incomes_data_hourly_highly_detailed$net_income 
          
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
          
          # Plotting
          EMTR_Schedule  <- ggplot() +
            geom_col(data = all_ratios_long, aes(x = hours, y = EMTR, fill = `Tax/Transfer`), width = 2) +
            scale_colour_manual(values = c("Net Income" = "black")) +
            labs_e61(title = "Earnings Schedule (left) and Effective Marginal Tax Rates",
                     x = "Hours Worked",
                     y = "EMTR",
                     fill = "Income Type",
                     colour = "") + add_baseline()  + scale_fill_e61() + 
            geom_hline(yintercept = 1, linetype = "dashed", col = "red") 
    
          Hours_Schedule <- plotly(Hours_Schedule)
          EMTR_Schedule <- plotly(EMTR_Schedule)
          
          interactive_combined_plot = subplot(Hours_Schedule, EMTR_Schedule,
                                              nrows = 1, shareX = TRUE, shareY = TRUE, titleX = FALSE, titleY = FALSE)
          
          # Display the interactive combined plot
          print(interactive_combined_plot)
        }
          }
        else{
          if(max_private_earnings < 10000 ){
            print("Please all the individual to consider earning more than $10,000 of 
                  private income")
          }
          else{
            
          }
          }
  
  
    
  
  
  
  
  
  
  
  
}