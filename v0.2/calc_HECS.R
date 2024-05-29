## Calculating of the gross benefit payment ----
# Last update: 25/08/2023
# Updated by: Matt Nolan
# Initial author: Matt Nolan

# Function for calculating income tax ----
# Includes Low Income Tax Credit, Low and Middle Income Tax Credit, Temporary Budget Repair Levy, and Beneficiary Tax offset.

calc_HECS <- function(gross_income) {
 
  
  HECS_brackets <- c(0) # Start with 0
  HECS_scale <- c(0) # Start with 0
  
  
  threshold_vars <- grep("^HECS_Threshold_", all_vars, value = TRUE)
  num_thresholds <- length(threshold_vars)
  
  for (i in 1:num_thresholds) {
    HECS_brackets <- c(HECS_brackets, get(paste("HECS_Threshold_", i, sep = "")))
    HECS_scale <- c(HECS_scale, get(paste("HECS_Rate", i, sep = "")))
  }
  
  
  
  
  tempinc <- gross_income

  index <- findInterval(tempinc, HECS_brackets)
  
  # Extract the corresponding value from HECS_scale
  if(index > 0) { # Ensure index is within the vector bounds
    HECS_rate <- HECS_scale[index]
  } else {
    HECS_rate <- NA # tempinc does not fall within the defined brackets
  }
  
 HECS_Repayment <- HECS_rate * tempinc
 
 HECS_Repayment <- ifelse(HECS_Repayment > HECSDebt, HECSDebt, HECS_Repayment)
  
  
  HECS <- max(0,(HECS_Repayment ))
  
  return(HECS)
}
