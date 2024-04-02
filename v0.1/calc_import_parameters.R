## File for setting up the parameters based on selection ----
# Last update: 25/08/2023
# Updated by: Matt Nolan
# Initial author: Matt Nolan

# Function for importing and setting up the policy parameter object ----
# policy_date format is based on the relevant quarter (i.e. Q3 for July-Sep) and year.  Q3_2023 will refer to the policy parameters during the September quarter of 2023
# The policy object produced is invariant of the details of the individual being looked at - so this only needs to be run in the initial set up stage.

policy_parameters <- function(policy_date1){
  import_name <- paste0("Policy files/",policy_date1,".csv")
  #import_name <- paste0("Policy files/","Q1_2023",".csv")
  policy_parameters_1 <- read.csv(import_name)
  return(policy_parameters_1)
  
  # Assign policy object
  for (i in 1:nrow(policy_parameters_1)){
    assign(policy_parameters_1$Parameter[i],policy_parameters_1$Value[i])
  }
  
  # Income tax
  
  tax_brackets <- c(0, tax_free, tax_threshold_1, tax_threshold_2, tax_threshold_3)
  tax_scale <- c(0, tax_rate_1, tax_rate_2, tax_rate_3, tax_rate_4)
  
  # Benefit elements
  
  JSP_S_ND_arate <- c(0,JSP_S_ND_arate_1,JSP_S_ND_arate_2) 
  JSP_S_ND_athresh <- c(0,JSP_S_ND_athresh_1,JSP_S_ND_athresh_2)
}
  
  