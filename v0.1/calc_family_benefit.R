## Calculating of the family benefit received ----
# Last update: 25/08/2023
# Updated by: Matt Nolan
# Initial author: Matt Nolan

## Function ----
calc_family_benefit <- function(child_age,gross_fam_income,work_income,taxable_benefit){
  # Note, the LITSO is not counted in the super information.
  super_contributions = super_cont_rate*(work_income + partner_earnings)
  ATI = gross_fam_income + super_contributions
  net_fam_a = 0
  net_fam_b = 0
  net_fam = 0
  numb_child_12 <- length(child_age[child_age < 13])
  numb_child_19 <- length(child_age[child_age <20 & child_age > 12])
  fam_a <- fam_a_young_pay*numb_child_12 + fam_a_old_pay*numb_child_19
  if (ATI < fam_base_threshold & Numb_dep > 0){
    net_fam_a <- max((fam_a*26 - max((ATI-fam_max_threshold),0)*fam_max_rate - max((ATI - fam_base_threshold),0)*fam_base_rate)/26,fam_basic_pay*(numb_child_19+numb_child_12))
  }
  else if (Numb_dep > 0) {
    net_fam_a <- max(max((fam_a*26 - max((ATI-fam_max_threshold),0)*fam_max_rate - max((ATI - fam_base_threshold),0)*fam_base_rate)/26,(fam_basic_pay*(numb_child_19+numb_child_12)*26 - fam_a_highATI_rate*(ATI - fam_base_threshold))/26),0)
  }
  else {
    net_fam_a = 0
  }
  # Add Family Benefit B
  numb_child_5 <- length(child_age[child_age < 5])
  numb_child_18 <- length(child_age[child_age < 19 & child_age > 4])
  fam_b <- fam_b_young_pay*numb_child_5 + fam_b_old_pay*numb_child_18
  if (partnered == 0){
    net_fam_b = ifelse(ATI > fam_b_threshold,0,fam_b) # The documentation appears to suggest no abatement, just a cliff face!
  }
  else{
    net_fam_b = ifelse(max(partner_earnings,work_income)*(1+super_cont_rate) > fam_b_threshold,0,max(fam_b-0.2*(min(partner_earnings*(1+super_cont_rate),work_income*(1+super_cont_rate)+taxable_benefit*26)),0))
  }
  net_fam = net_fam_a + net_fam_b
  return(list(net_fam = net_fam,gross_fam = fam_a + fam_b))
}
