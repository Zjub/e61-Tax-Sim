## Calculating of the gross benefit payment ----
# Last update: 25/08/2023
# Updated by: Matt Nolan
# Initial author: Matt Nolan

# Function for calculating income tax ----
# Includes Low Income Tax Credit, Low and Middle Income Tax Credit, Temporary Budget Repair Levy, and Beneficiary Tax offset.

calc_income_tax <- function(gross_income,work_income) {
  
  # Set up the income tax scale
  tax_brackets <- c(0, tax_free, tax_threshold_1, tax_threshold_2, tax_threshold_3)
  tax_scale <- c(0, tax_rate_1, tax_rate_2, tax_rate_3, tax_rate_4)
  
  
  create_tax_vectors <- function(num_brackets) {
    tax_brackets <- c(0, sapply(1:num_brackets, function(i) get(paste0("tax_threshold_", i), envir = .GlobalEnv)))
    tax_scale <- c(0, sapply(1:num_brackets, function(i) get(paste0("tax_rate_", i), envir = .GlobalEnv)))
    
    return(list(tax_brackets = tax_brackets, tax_scale = tax_scale))
  }
  
  if(edited_tax_brackets == 1){
    
    tax_vectors <- create_tax_vectors(num_brackets)
    tax_brackets <- tax_vectors$tax_brackets
    tax_scale <- tax_vectors$tax_scale
    
  }
  
  tax_nc <- 0
  tempinc <- gross_income
  for (i in length(tax_brackets):2) {
    if (gross_income > tax_brackets[i]) {
      tax_nc <- tax_nc + (tempinc - tax_brackets[i]) * tax_scale[i]
      tempinc <- tax_brackets[i]
    }
  }

  LITO_credit <- 0
  if (LITO_on == 1){
    calculate_LITO <- function(gross_income) {
      if (gross_income <= LITO_thresh_1) {
        # If income is below or equal to the first threshold
        LITO_credit <- LITO_gross
      } else if (gross_income > LITO_thresh_1 & gross_income <= LITO_thresh_2) {
        # If income is between the first and second thresholds
        excess_income <- gross_income - LITO_thresh_1
        LITO_credit <- LITO_gross - (excess_income * LITO_rate_1)
      } else if (gross_income > LITO_thresh_2) {
        # If income is between the second threshold and 66,667
        excess_income <- gross_income - LITO_thresh_2
        LITO_credit <- LITO_gross - (LITO_thresh_2 - LITO_thresh_1) * LITO_rate_1 - (excess_income * LITO_rate_2)
      } else {
        # If income exceeds 66,667
        LITO_credit <- 0
      }
      LITO_credit <- max(0, LITO_credit)
      return(LITO_credit)
      
    }
    LITO_credit <- calculate_LITO(gross_income)
  }

  LMITO_credit <- 0
  if (LMITO_on == 1){
    LMITO_credit <- ifelse(gross_income <= LMITO_thresh_1,LMITO_gross,ifelse(gross_income <= LMITO_thresh_2,LMITO_gross + (gross_income-LMITO_thresh_1)*LMITO_inc_rate,ifelse(gross_income <- LMITO_thresh_3,LMITO_max,max(0,LMITO_max - (gross_income - LMITO_thresh_3)*LMITO_dec_rate))))
  }

  BRL <- 0
  if (BRL_on == 1){
    BRL <- max(0,(gross_income - BRL_thresh)*BRL_rate)
  }

  BTO_credit <- 0
  
  if(BTO_on == 1){
    benefit_paid <- calc_benefit_abated(work_income,partner_earnings)[["net_benefit"]]*26
    if (benefit_paid > BTO_thresh){
      BTO_credit <- BTO_rate*(benefit_paid - BTO_thresh)
    }
  }
  
  SAPTO <- 0
  
  if(SAPTO_on == 1){
    SAPTO <- ifelse(gross_income > SAPTO_Single_Shade_Out & partnered == 0 & Numb_dep > 0,
                    max(SAPTO_Single_Max - (gross_income - SAPTO_Single_Shade_Out) * SAPTO_Rate, 0),
                    SAPTO )
    
    SAPTO <- ifelse(gross_income < SAPTO_Single_Shade_Out & partnered == 0 & Numb_dep > 0,
                    SAPTO_Single_Max, SAPTO )
    BTO_credit <- ifelse(BTO_on == 1 & partnered == 0 & Numb_dep > 0 , 0, BTO_credit)
  }
  
  tax <- max(0,(tax_nc + BRL - LITO_credit - LMITO_credit - BTO_credit - SAPTO))
  
  medicarelevy <- 0 
 
if(partnered == 0 & Numb_dep == 0){
    if(gross_income >= medi_LI_threshold_S & gross_income < medi_PI_threshold_S){
      
      medicarelevy <-  (gross_income - medi_LI_threshold_S) * 0.1
          } 
  
  if( gross_income > medi_PI_threshold_S){
    
    medicarelevy <-  gross_income * 0.02
  } 
}
 
  if(partnered == 1 | Numb_dep > 0){
    
    
    
    if(gross_income + partner_earnings >= medi_LI_threshold_P + (Numb_dep * medi_dep_LI_threshold)  &
       gross_income + partner_earnings < medi_PI_threshold_P + (medi_dep_PI_threshold * Numb_dep)){
      
      medicarelevy <-  (gross_income + partner_earnings - medi_LI_threshold_P - Numb_dep * medi_dep_LI_threshold) * 0.1
    } 
    
    if(gross_income + partner_earnings > medi_PI_threshold_P + medi_dep_PI_threshold * Numb_dep & 
       gross_income >= medi_LI_threshold_S & gross_income < medi_PI_threshold_S ){
      
      medicarelevy <-  (gross_income - medi_LI_threshold_S) * 0.1
    }
    
    
      
    
    if( gross_income > medi_PI_threshold_S & gross_income + partner_earnings > medi_PI_threshold_P + medi_dep_PI_threshold * Numb_dep){
      
      medicarelevy <-  gross_income * 0.02
    } 
    if(gross_income < medi_LI_threshold_S){
      medicarelevy <- 0 
    }
    
  }  
   
  return(list(tax = tax, medicarelevy = medicarelevy))
}
