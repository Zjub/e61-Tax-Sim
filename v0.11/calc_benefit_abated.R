## Calculating the abated benefit received ----
# Last update: 25/08/2023
# Updated by: Matt Nolan
# Initial author: Matt Nolan


## Function ----
calc_benefit_abated <- function(work_income,partner_earnings) {
  # Abate down based on partner income first.
  taxable_benefit = calc_benefit_gross()[["taxable_benefit"]]
  gross_benefit = calc_benefit_gross()[["gross_benefit"]]
  supp_benefit = calc_benefit_gross()[["supp_benefit"]]
  abate = 0
  #abate <- ifelse(Have_dep == 1, max((partner_earnings/26 - partner_thresh_dep)*partner_abate,0),max((partner_earnings/26 - partner_thresh)*partner_abate,0))
  tempwi <- work_income/26 # Fortnightly pay
  
  JSPabate = abate 
  PPabate = abate 
  
  JSP_S_ND_arate <- c(0,JSP_S_ND_arate_1,JSP_S_ND_arate_2)
  JSP_S_ND_athresh <- c(0,JSP_S_ND_athresh_1,JSP_S_ND_athresh_2)
  
  
  
  PP_S_athresh <- NA 
  
  if (Numb_dep <= 0) {
    PP_S_athresh <- NA
  } else if (Numb_dep == 1) {
    PP_S_athresh <- PP_S_athresh_base
  } else if (Numb_dep == 2) {
    PP_S_athresh <- PP_S_athresh_2
  } else if (Numb_dep == 3) {
    PP_S_athresh <- PP_S_athresh_3
  } else if (Numb_dep > 3) {
    PP_S_athresh <- PP_S_athresh_3 + PP_S_athresh_mult * (Numb_dep - 3)
  }
  
  
  
  
  if (PPeligible == 1 & partnered == 0){
    if (work_income/26 > PP_S_athresh){
      abate <- abate + (tempwi - PP_S_athresh)*PP_S_arate
      PPabate <- PPabate + (tempwi - PP_S_athresh)*PP_S_arate
    }}
  
  
  if(PPeligible == 0 & partnered == 0){
    for (i in length(JSP_S_ND_athresh):2) {
      if (work_income/26 > JSP_S_ND_athresh[i]){
        abate <- abate + (tempwi - JSP_S_ND_athresh[i])*JSP_S_ND_arate[i]
        JSPabate <- JSPabate + (tempwi - JSP_S_ND_athresh[i])*JSP_S_ND_arate[i]
        tempwi <- JSP_S_ND_athresh[i]
      }
    }
  }
  
  
  
  
  if(partnered == 1){
  if ((work_income / 26) <= PP_C_I_Threshold_1) {
    if (partner_earnings/26 <= PP_C_P_Threshold) {
      PPabate <- 0
      abate <- 0
      JSPabate <- 0 # No reduction
    } else {
      PPabate <- (partner_earnings/26 - PP_C_P_Threshold) * PP_C_arate_2
      JSPabate <- (partner_earnings/26 - PP_C_P_Threshold) * PP_C_arate_2
      abate <- (partner_earnings/26 - PP_C_P_Threshold) * PP_C_arate_2 
    }
  } else if ((work_income / 26) > PP_C_I_Threshold_1 & (work_income / 26) <= PP_C_I_Threshold_2) {
    if (partner_earnings/26 <= PP_C_P_Threshold) {
      PPabate <- ((work_income / 26) - PP_C_I_Threshold_1) * PP_C_arate_1
      JSPabate <- ((work_income / 26) - PP_C_I_Threshold_1) * PP_C_arate_1
      abate <- ((work_income / 26) - PP_C_I_Threshold_1) * PP_C_arate_1 # Reduction based on your income over $PP_C_I_Threshold_1
    } else {
      PPabate <- ((work_income / 26) - PP_C_I_Threshold_1) * PP_C_arate_1 + (partner_earnings/26 - PP_C_P_Threshold) * PP_C_arate_2 
      JSPabate <- ((work_income / 26) - PP_C_I_Threshold_1) * PP_C_arate_1 + (partner_earnings/26 - PP_C_P_Threshold) * PP_C_arate_2 
       abate <- ((work_income / 26) - PP_C_I_Threshold_1) * PP_C_arate_1 + (partner_earnings/26 - PP_C_P_Threshold) * PP_C_arate_2 # Reduction based on both incomes
    }
  } else { # (work_income / 26) > PP_C_I_Threshold_2
    if (partner_earnings/26 <= PP_C_P_Threshold) {
      PPabate <- ((PP_C_I_Threshold_2 - PP_C_I_Threshold_1)* PP_C_arate_1) + ((work_income / 26) - PP_C_I_Threshold_2) * PP_C_arate_2 # Reduction based on your income over $PP_C_I_Threshold_2
      JSPabate <- ((PP_C_I_Threshold_2 - PP_C_I_Threshold_1)* PP_C_arate_1) + ((work_income / 26) - PP_C_I_Threshold_2) * PP_C_arate_2 # Reduction based on your income over $PP_C_I_Threshold_2
      abate <- ((PP_C_I_Threshold_2 - PP_C_I_Threshold_1)* PP_C_arate_1) + ((work_income / 26) - PP_C_I_Threshold_2) * PP_C_arate_2 # Reduction based on your income over $PP_C_I_Threshold_2
    } else {
      PPabate <- ((PP_C_I_Threshold_2 - PP_C_I_Threshold_1)* PP_C_arate_1) + ((work_income / 26) - PP_C_I_Threshold_2) * PP_C_arate_2 + (partner_earnings/26 - PP_C_P_Threshold) * PP_C_arate_2 # Reduction based on both incomes
      JSPabate <- ((PP_C_I_Threshold_2 - PP_C_I_Threshold_1)* PP_C_arate_1) + ((work_income / 26) - PP_C_I_Threshold_2) * PP_C_arate_2 + (partner_earnings/26 - PP_C_P_Threshold) * PP_C_arate_2 
      abate <- ((PP_C_I_Threshold_2 - PP_C_I_Threshold_1)* PP_C_arate_1) + ((work_income / 26) - PP_C_I_Threshold_2) * PP_C_arate_2 + (partner_earnings/26 - PP_C_P_Threshold) * PP_C_arate_2 # Reduction based on both incomes
    }
  }
}
  
  
  JSP <-   calc_benefit_gross()[["JSP_Pay"]] - JSPabate
  PP_Pay <-  calc_benefit_gross()[["PP_Pay"]] - PPabate
  
  
  RA <- calc_benefit_gross()[["RA"]]
  
  taxable_benefit <- taxable_benefit - abate
  net_benefit <- gross_benefit - abate
  
 
  ES <- calc_benefit_gross()[["ES"]]
  
  
  
  return(list(taxable_benefit = max(taxable_benefit,0), net_benefit = max(net_benefit,0), 
              JSP = max(JSP, 0), PP_Pay = max(PP_Pay, 0), RA = max(RA, 0), ES = max(ES, 0), supp_benefit = supp_benefit ))
}
