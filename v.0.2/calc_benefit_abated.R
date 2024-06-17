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
  work_income <- work_income
  abate = 0
  #abate <- ifelse(Have_dep == 1, max((partner_earnings/26 - partner_thresh_dep)*partner_abate,0),max((partner_earnings/26 - partner_thresh)*partner_abate,0))
  tempwi <- work_income / 26 # Fortnightly pay
  
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
    if (tempwi > PP_S_athresh){
      abate <- abate + (tempwi - PP_S_athresh)*PP_S_arate
      PPabate <- PPabate + (tempwi - PP_S_athresh)*PP_S_arate
    }}
  
  
  if(PPeligible == 0 & partnered == 0){
    for (i in length(JSP_S_ND_athresh):2) {
      if (tempwi > JSP_S_ND_athresh[i]){
        abate <- abate + (tempwi - JSP_S_ND_athresh[i])*JSP_S_ND_arate[i]
        JSPabate <- JSPabate + (tempwi - JSP_S_ND_athresh[i])*JSP_S_ND_arate[i]
        tempwi <- JSP_S_ND_athresh[i]
      }
    }
  }
  
  
  
  
  if(partnered == 1){
  if ((tempwi) <= PP_C_I_Threshold_1) {
    if (partner_earnings/26 <= PP_C_P_Threshold) {
      PPabate <- 0
      abate <- 0
      JSPabate <- 0 # No reduction
    } else {
      PPabate <- (partner_earnings/26 - PP_C_P_Threshold) * PP_C_arate_2
      JSPabate <- (partner_earnings/26 - PP_C_P_Threshold) * PP_C_arate_2
      abate <- (partner_earnings/26 - PP_C_P_Threshold) * PP_C_arate_2 
    }
  } else if ((tempwi) > PP_C_I_Threshold_1 & (tempwi) <= PP_C_I_Threshold_2) {
    if (partner_earnings/26 <= PP_C_P_Threshold) {
      PPabate <- ((tempwi) - PP_C_I_Threshold_1) * PP_C_arate_1
      JSPabate <- ((tempwi) - PP_C_I_Threshold_1) * PP_C_arate_1
      abate <- ((tempwi) - PP_C_I_Threshold_1) * PP_C_arate_1 
    } else {
      PPabate <- ((tempwi) - PP_C_I_Threshold_1) * PP_C_arate_1 + 
        (partner_earnings/26 - PP_C_P_Threshold) * PP_C_arate_2 
      JSPabate <- ((tempwi) - PP_C_I_Threshold_1) * PP_C_arate_1 + 
        (partner_earnings/26 - PP_C_P_Threshold) * PP_C_arate_2 
       abate <- ((tempwi) - PP_C_I_Threshold_1) * PP_C_arate_1 + 
         (partner_earnings/26 - PP_C_P_Threshold) * PP_C_arate_2 # Reduction based on both incomes
    }
  } else { # (tempwi) > PP_C_I_Threshold_2
    if (partner_earnings/26 <= PP_C_P_Threshold) {
      PPabate <- ((PP_C_I_Threshold_2 - PP_C_I_Threshold_1)* PP_C_arate_1) + 
        ((tempwi) - PP_C_I_Threshold_2) * PP_C_arate_2 
      JSPabate <- ((PP_C_I_Threshold_2 - PP_C_I_Threshold_1)* PP_C_arate_1) + 
        ((tempwi) - PP_C_I_Threshold_2) * PP_C_arate_2 
      abate <- ((PP_C_I_Threshold_2 - PP_C_I_Threshold_1)* PP_C_arate_1) + 
        ((tempwi) - PP_C_I_Threshold_2) * PP_C_arate_2 
    } else {
      PPabate <- ((PP_C_I_Threshold_2 - PP_C_I_Threshold_1)* PP_C_arate_1) + 
        ((tempwi) - PP_C_I_Threshold_2) * PP_C_arate_2 + 
        (partner_earnings/26 - PP_C_P_Threshold) * PP_C_arate_2 
      JSPabate <- ((PP_C_I_Threshold_2 - PP_C_I_Threshold_1)* PP_C_arate_1) +
        ((tempwi) - PP_C_I_Threshold_2) * PP_C_arate_2 + 
        (partner_earnings/26 - PP_C_P_Threshold) * PP_C_arate_2 
      abate <- ((PP_C_I_Threshold_2 - PP_C_I_Threshold_1)* PP_C_arate_1) + 
        ((tempwi) - PP_C_I_Threshold_2) * PP_C_arate_2 + 
        (partner_earnings/26 - PP_C_P_Threshold) * PP_C_arate_2 
    }
  }
}
  
  ### Test to see if the individual is recieving FTB A above the base rate. If so 
  ### they will recieve RA and the ES as part of that, rather than through their taxable
  ### benefit. 
  
  net_fam_a_flag = ifelse(young_child < 19, 1, 0)
  net_fam_a_flag <- ifelse(is.na(net_fam_a_flag), 0, net_fam_a_flag) 
  
  
  RA <- ifelse(net_fam_a_flag == 1, 0, calc_benefit_gross()[["RA"]])
  ES <- ifelse(net_fam_a_flag == 1, 0,  calc_benefit_gross()[["ES"]])
  
  
  JSP <-  calc_benefit_gross()[["JSP_Pay"]]
  JSP <-   ifelse( JSP > 0, calc_benefit_gross()[["JSP_Pay"]] + work_for_the_dole , JSP)
  PP_Pay <-  calc_benefit_gross()[["PP_Pay"]]
  
  ### See what proportion of the benefits we need to abate (evenly)
  
  if(RA_Abate == 1){
  
  Abatement_rate <- ifelse(JSP + PP_Pay + RA + ES > 0, (abate) / (JSP + PP_Pay + RA + ES), 0)
  
  ## Can't abate more than 100% of your benefit. 
  
  Abatement_rate <- min(Abatement_rate, 1)
  
  ### abate these benefits 
  
  JSP <-   JSP - (JSP * Abatement_rate)
  PP_Pay <-  PP_Pay - (PP_Pay * Abatement_rate)
  
  ### abate RA and ES (note that this will not do anything if they are 0, because they 
  ### are occuring through FTB A)
  
  RA <- RA - (RA * Abatement_rate)
  ES <- ES - (ES * Abatement_rate)
  }
  else{
    Abatement_rate <- ifelse(JSP + PP_Pay > 0, (abate) / (JSP + PP_Pay), 0)
    
    ## Can't abate more than 100% of your benefit. 
    
    Abatement_rate <- min(Abatement_rate, 1)
    
    ### abate these benefits 
    
    JSP <-   JSP - (JSP * Abatement_rate)
    PP_Pay <-  PP_Pay - (PP_Pay * Abatement_rate)
    
    
  }
  ### Final Benefit amounts 
  taxable_benefit <- JSP + PP_Pay 
  net_benefit <- JSP + PP_Pay + RA + ES 

  return(list(taxable_benefit = max(taxable_benefit,0), net_benefit = max(net_benefit,0), 
              JSP = max(JSP, 0), PP_Pay = max(PP_Pay, 0), RA = max(RA, 0), ES = max(ES, 0),
              supp_benefit = supp_benefit ))
}
