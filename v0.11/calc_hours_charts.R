

calc_hours_charts <- function(){
 
  

net_income_calculator_hourly <- function(wage_rate) {
  net_income_profile_hourly <- numeric((max_hours +1))
  for (hours in 0:max_hours) {
    net_income_profile_hourly[hours + 1] <- calc_net_income(wage_rate, hours, HECS_on)[["net_income"]]
  }
  return(net_income_profile_hourly)
}

net_incomes_data_hourly <- net_income_calculator_hourly(wage)

incomesdf_hourly <- data.frame(Hours = seq(0,max_hours,by=1), "Take home income" = net_incomes_data_hourly) %>% mutate("Work Income" = wage*Hours*52) %>% pivot_longer(!Hours,names_to = "variable",values_to = "value")

plot_income_hourly <- ggplot(incomesdf_hourly, aes(x=Hours,y=value/1000,colour=variable)) + geom_line() + theme_e61() + labs_e61(
  title = "Net Income",
  subtitle = paste0("Annual income, period = ",policy_date1),
  y = "(000s)",
  x = "Hours worked",
  sources = c("e61", cite)
) + scale_y_continuous_e61(labels = scales::dollar,limits=c(0,max(incomesdf_hourly$value/1000)*1.2)) +
  plot_label("Work Income",y= 0.9*max(incomesdf_hourly$value/1000),x= 10,colour = palette_e61(2)[2]) +
  plot_label("Disposable Income",y= 0.8*max(incomesdf_hourly$value/1000),x= 10,colour = palette_e61(2)[1]) +
  geom_hline(yintercept = 0)

#plot_income_hourly

#int_plot_income_hourly <- ggplotly(plot_income_hourly)

#int_plot_income_hourly



incomes_spread <- incomesdf_hourly %>%
  spread(key = variable, value = value) %>%
  mutate(Take.home.income_diff = Take.home.income - lag(Take.home.income, default = first(Take.home.income)),
         Work_Income_diff = `Work Income` - lag(`Work Income`, default = first(`Work Income`)),
         metric = if_else(Hours == 0, 0,  1- (Take.home.income_diff / Work_Income_diff))) %>%
  select(Hours, metric)

# Step 2: Plot the metric
EMTR <- ggplot(incomes_spread, aes(x = Hours, y = metric)) +
  geom_line() +
  labs_e61(title = "Effective Marginal Tax Rate For Working an Additional Hour",
           x = "Hours Worked",
           y = "EMTR")  + geom_hline(yintercept = 1, col = "red", linetype ="dashed")


#EMTR

#int_plot_EMTR_hourly <- ggplotly(EMTR)
#int_plot_EMTR_hourly 

#### Slightly Detailed Calculation/breakdown

detailed_variable_calculator <- function(wage_rate) {
  # Initialize an empty data frame to store results for each hour
  results_df <- data.frame(hours = integer(0), net_income = numeric(0), income_tax = numeric(0),
                           work_income = numeric(0), abated_benefit = numeric(0),
                           taxable_benefit = numeric(0), gross_income = numeric(0),
                           gross_fam_income = numeric(0), fam_a_income = numeric(0), 
                           supp_benefit = numeric(0), HECS_payment = numeric(0))
  
  for (hours in 0:max_hours) {
    # Calculate net income and other variables for the given number of hours
    calc_results <- calc_net_income(wage_rate, hours, HECS_on)
    
    # Append the results to the data frame
    results_df <- rbind(results_df, cbind(hours, as.data.frame(t(unlist(calc_results)))))
  }
  results_df$income_tax <- results_df$income_tax * -1 
  results_df$HECS_payment <- results_df$HECS_payment * -1 
  results_df$abated_benefit <- results_df$abated_benefit * 26
  results_df$taxable_benefit <- results_df$taxable_benefit * 26
  results_df$supp_benefit <- results_df$supp_benefit * 26
  return(results_df)
}

all_incomes_data_hourly <- detailed_variable_calculator(wage)  
all_incomes_long <- pivot_longer(all_incomes_data_hourly, 
                                 cols = c(income_tax, work_income, taxable_benefit, fam_a_income, supp_benefit, HECS_payment),
                                 names_to = "income_type", 
                                 values_to = "amount")

# Plotting
#ggplot() +
#  geom_area(data = all_incomes_long, aes(x = hours, y = amount / 1000, fill = income_type), position = "stack") +
#  geom_line(data = all_incomes_data_hourly, aes(x = hours, y = net_income / 1000, colour = "Net Income"), size = 2) +
#  scale_colour_manual(values = c("Net Income" = e61_skydark)) +
#  labs_e61(title = "Net Income ($1000s) Decomposition by hours worked",
#           x = "Hours",
#           y = "$",
#           fill = "Income Type",
#           colour = "") + add_baseline()  + theme(legend.position = "bottom") + scale_fill_e61()




#### Fully Detailed Calculation/breakdown

highly_detailed_variable_calculator <- function(wage_rate) {
  # Initialize an empty data frame to store results for each hour
  results_df <- data.frame(hours = integer(0), net_income = numeric(0), income_tax = numeric(0),
                           work_income = numeric(0), JSP = numeric(0), PP_Pay = numeric(0), 
                           RA = numeric(0), ES = numeric(0),
                           taxable_benefit = numeric(0), gross_income = numeric(0),
                           gross_fam_income = numeric(0), net_fam_a_income = numeric(0), 
                           net_fam_b_income = numeric(0), HECS_payment <- numeric(0))
  
  for (hours in seq(0, max_hours, by = 0.1)) {
    # Calculate net income and other variables for the given number of hours
    calc_results <- calc_net_income_detailed(wage_rate, hours, HECS_on)
    
    # Append the results to the data frame
    results_df <- rbind(results_df, cbind(hours, as.data.frame(t(unlist(calc_results)))))
  }
  results_df$income_tax <- results_df$income_tax * -1 
  results_df$HECS_payment <- results_df$HECS_payment * -1 
  results_df$taxable_benefit <- results_df$taxable_benefit * 26
  return(results_df)
}

incomes_data_hourly_highly_detailed <- highly_detailed_variable_calculator(wage)

all_incomes_long <- pivot_longer(incomes_data_hourly_highly_detailed, 
                                 cols = c(income_tax, work_income, RA, JSP, ES, PP_Pay,  net_fam_a_income, net_fam_b_income, HECS_payment),
                                 names_to = "income_type", 
                                 values_to = "Amount")

#all_incomes_long <- all_incomes_long %>%
#  group_by(income_type) %>%
#  filter(!(all(amount == 0))) %>%
#  ungroup()

all_incomes_long$income_type <- as.factor(all_incomes_long$income_type)
levels(all_incomes_long$income_type) <- c("Energy Supplement", "HECS Payment", "Income Tax", 
                                          "Job Seeker Payment", "Family Tax Benefit A", 
                                          "Family Tax Benefit B", "Parenting Payment", 
                                          "Commonwealth Rent Assistance", "Work Income")

all_incomes_long$'Income Type' <- all_incomes_long$income_type

all_incomes_long <- all_incomes_long %>%
  mutate(Amount = round(Amount, 2))

incomes_data_hourly_highly_detailed$'Net Income' <-  incomes_data_hourly_highly_detailed$net_income 

# Creating the text string
text_string <- paste("Taxes and transfers for a", 
                     ifelse(partnered == 1, "partnered", "unpartnered"), 
                     ifelse(Home_owner == 1, "home owner", "renter"), 
                     "earning $", wage, "an hour, with", Numb_dep, "children") 

# Append Parenting Payment Benefit eligibility
if (PPeligible == 1 & Numb_dep > 0) {
  text_string <- paste(text_string, ",eligible for the Parenting Payment")
} else if (PPeligible == 0 & Numb_dep > 0) {
  text_string <- paste(text_string, ", not eligible for the Parenting Payment")
}

# Append HECS Debt information
if (HECS_on == 1) {
  text_string <- paste(text_string, "and", HECSDebt, "in HECS Debt.")
}


# Plotting
plot_income_highly_detailed <- ggplot() +
  geom_col(data = all_incomes_long, aes(x = hours, y = Amount  , fill = `Income Type`), width = 3 ) +
  geom_line(data = incomes_data_hourly_highly_detailed, aes(x = hours, y = `Net Income` ), size = 2, col = "black") +
  labs_e61(title = text_string,
           x = "Hours worked",
           y = "$",
           fill = "Income Type",
           colour = "") + add_baseline()  + scale_fill_e61() +
  scale_x_continuous(expand = c(0, 0)) + # Remove default expansion for x-axis
  scale_y_continuous(expand = c(0, 0)) 

#plot_income_highly_detailed

int_plot_income_highly_detailed <- ggplotly(plot_income_highly_detailed)
int_plot_income_highly_detailed 


highly_detailed_variable_calculator <- function(wage_rate) {
  # Initialize an empty data frame to store results for each hour
  results_df <- data.frame(hours = integer(0), net_income = numeric(0), income_tax = numeric(0),
                           work_income = numeric(0), JSP = numeric(0), PP_Pay = numeric(0), 
                           RA = numeric(0), ES = numeric(0),
                           taxable_benefit = numeric(0), gross_income = numeric(0),
                           gross_fam_income = numeric(0), net_fam_a_income = numeric(0), 
                           net_fam_b_income = numeric(0), HECS_payment <- numeric(0))
for (hours in 0:max_hours) {
  # Calculate net income and other variables for the given number of hours
  calc_results <- calc_net_income_detailed(wage_rate, hours, HECS_on)
  
  # Append the results to the data frame
  results_df <- rbind(results_df, cbind(hours, as.data.frame(t(unlist(calc_results)))))
}
results_df$income_tax <- results_df$income_tax * -1 
results_df$HECS_payment <- results_df$HECS_payment * -1 
results_df$taxable_benefit <- results_df$taxable_benefit * 26
return(results_df)
}

incomes_data_hourly_highly_detailed <- highly_detailed_variable_calculator(wage)


incomes_changes <- incomes_data_hourly_highly_detailed %>%
  mutate(across(-hours, ~abs(. - lag(., default = first(.)))), .names = "change_{.col}") %>%
  mutate(across(starts_with("change_"), ~if_else(row_number() == 1, 0, .)))

incomes_ratios <- incomes_changes %>%
  mutate(across(-c(hours, work_income, starts_with("change_"), .names),
                ~if_else(work_income == 0, 0, ./work_income)))


all_ratios_long <- pivot_longer(incomes_ratios, 
                                cols = c(income_tax, RA, JSP, ES, PP_Pay,  net_fam_a_income, net_fam_b_income, HECS_payment),
                                names_to = "income_type", 
                                values_to = "amount")

#all_ratios_long <- all_ratios_long %>%
 # group_by(income_type) %>%
#  filter(!(all(amount == 0))) %>%
#  ungroup()


all_ratios_long$income_type <- as.factor(all_ratios_long$income_type)
levels(all_ratios_long$income_type) <- c("Energy Supplement", "HECS Payment", "Income Tax", 
                                          "Job Seeker Payment", "Family Tax Benefit A", 
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
plot_EMTR_Highly_Detailed <- ggplot() +
  geom_col(data = all_ratios_long, aes(x = hours, y = EMTR, fill = `Tax/Transfer`), width = 2) +
  scale_colour_manual(values = c("Net Income" = "black")) +
  labs_e61(title = text_string,
           x = "Hours Worked",
           y = "EMTR",
           fill = "Income Type",
           colour = "") + add_baseline()  + scale_fill_e61() + 
  geom_hline(yintercept = 1, linetype = "dashed", col = "red") 

#plot_EMTR_Highly_Detailed

int_plot_EMTR_Highly_Detailed <- ggplotly(plot_EMTR_Highly_Detailed)

 
#grid.arrange(plot_income_hourly, EMTR,  nrow = 1, ncol = 2)

#grid.arrange(plot_income_highly_detailed,  plot_EMTR_Highly_Detailed,  nrow = 1, ncol = 2)

#int_plot_income_highly_detailed 

return(list(
  int_plot_income_highly_detailed = int_plot_income_highly_detailed,
  int_plot_EMTR_Highly_Detailed = int_plot_EMTR_Highly_Detailed
))
}
