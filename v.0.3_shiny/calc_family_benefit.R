## Calculating of the family benefit received ----
# Last update: 25/08/2023
# Updated by: Matt Nolan
# Initial author: Matt Nolan

## Function ----
calc_family_benefit <- function(child_age,gross_fam_income,work_income,taxable_benefit){
  
  ##### Setup variables for eligibility and thresholds. 
  
  super_contributions = super_cont_rate*(work_income + partner_earnings)
  ATI = gross_fam_income #+ super_contributions
  net_fam_a = 0
  net_fam_b = 0
  numb_child_12 <- length(child_age[child_age < 13])
  numb_child_19 <- length(child_age[child_age <20 & child_age > 12])
  
  RA <- calc_benefit_gross()[["RA"]] 
  ES <- calc_benefit_gross()[["ES"]] 
  
  
  ##### Initial Rate of Fam_a if there is no abatement 
  
  fam_a <- fam_a_young_pay*numb_child_12 + fam_a_old_pay*numb_child_19

  
  First_abatement <- max(min(ATI - fam_max_threshold, fam_base_threshold - fam_max_threshold), 0) 
  Second_abatement <- max(ATI - fam_base_threshold, 0)
  
  if(RA_Abate == 1){
  
  if (Numb_dep > 0){
    fam_a_abatement_1 <- (First_abatement*fam_max_rate + Second_abatement*fam_a_highATI_rate)/26
    fam_a_abatement_2 <- (fam_a_highATI_rate*Second_abatement)/26
    
    if (fam_a + RA + ES - fam_a_abatement_1 >= 
              fam_basic_pay*(numb_child_19+numb_child_12) - fam_a_abatement_2){
    Fam_a_test <- "max_rate"
    abatement_rate <-  ifelse(fam_a + RA + ES > 0 , 
                                fam_a_abatement_1  /  (fam_a + RA + ES), 
                               0)
    abatement_rate <- min(abatement_rate, 1)
    net_fam_a <- fam_a - (fam_a * abatement_rate)
    RA <- RA - (RA * abatement_rate)
    ES <- ES - (ES * abatement_rate)
    }
   else { 
     Fam_a_test <- "base_rate"
     abatement_rate <- fam_a_abatement_2 / fam_basic_pay*(numb_child_19+numb_child_12)
     abatement_rate <- min(abatement_rate, 1)
     net_fam_a <- max(fam_basic_pay*(numb_child_19+numb_child_12) - fam_a_abatement_2, 0)
     RA <- 0 
     ES <- 0 
     }
  
   }
    else {
    net_fam_a = 0
    Fam_a_test <- "NA"
    abatement_rate <- 0
    RA <- 0 
    ES <- 0 
    }
  } else{
    
    if (Numb_dep > 0){
      fam_a_abatement_1 <- (First_abatement*fam_max_rate + Second_abatement*fam_a_highATI_rate)/26
      fam_a_abatement_2 <- fam_a_highATI_rate*Second_abatement/26
      
      if (fam_a  - fam_a_abatement_1 >= 
          fam_basic_pay*(numb_child_19+numb_child_12) - fam_a_abatement_2){
        Fam_a_test <- "max_rate"
        abatement_rate <-  ifelse(fam_a  > 0 , 
                                  fam_a_abatement_1  /  (fam_a), 
                                  0)
        abatement_rate <- min(abatement_rate, 1)
        net_fam_a <- max(fam_a - (fam_a * abatement_rate),0)
        }
      else { 
        Fam_a_test <- "base_rate"
        abatement_rate <- fam_a_abatement_2 / fam_basic_pay*(numb_child_19+numb_child_12)
        abatement_rate <- min(abatement_rate, 1)
        net_fam_a <- min(fam_basic_pay*(numb_child_19+numb_child_12) - fam_a_abatement_2, 0)
        RA <- 0 
        ES <- 0 
      }
      
    } else {
      net_fam_a = 0
      Fam_a_test <- "NA"
      abatement_rate <- 0
      RA <- 0 
      ES <- 0 
    }
    
    
    
  }
  
  
  # Add Family Benefit B
  FTB_B_eligibility <- 0
  if(length(child_age[child_age < 13]) > 1){
    FTB_B_eligibility <- 1 
  }
  if(length(child_age[child_age < 19]) > 1 & partnered == 0){
    FTB_B_eligibility <- 1 
  }
  
  
  numb_child_5 <- length(child_age[child_age < 5])
  numb_child_18 <- length(child_age[child_age < 19 & child_age > 4])
  
  fam_b <- fcase(numb_child_5 > 0, fam_b_young_pay,
                 numb_child_18 > 0, fam_b_old_pay,
                 default = 0)
  
  highest_hh_income <- max(partner_earnings  , work_income  + taxable_benefit)
  lowest_hh_income <- min(partner_earnings  , work_income  + taxable_benefit)
  
  fam_b_abatement <- 0.2*max(lowest_hh_income - fam_be_low_thresh, 0) / 26
  
  
  
  if (partnered == 0){
    net_fam_b = ifelse(ATI > fam_b_threshold,0,fam_b) # The documentation appears to suggest no abatement, just a cliff face!
  }   else{
    net_fam_b = ifelse(highest_hh_income  > fam_b_threshold, 0 , max(fam_b - fam_b_abatement, 0))
  }
  net_fam_b <- net_fam_b * FTB_B_eligibility
  
  fam_a_ann_supp <- ifelse(ATI < fam_ftba_eoy_incthreshold, fam_ftba_eoy_supp * Numb_dep, 0) / 26
  net_fam_a <- ifelse(net_fam_a > 0, net_fam_a + fam_a_ann_supp, 0)
  
  net_fam_b <- ifelse(net_fam_b > 0, (fam_ftbb_eoy_supp / 26) + net_fam_b, 0)
  
  
  net_fam = net_fam_a + net_fam_b
  return(list(net_fam = net_fam,gross_fam = fam_a + fam_b, net_fam_a = net_fam_a, net_fam_b = net_fam_b,
              Fam_a_test = Fam_a_test, abatement_rate = abatement_rate, RA = RA, 
              ES = ES))
}
