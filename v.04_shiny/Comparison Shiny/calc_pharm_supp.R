## Calculating of the PA ----
# Last update: 30/09/2024
# Updated by: Matt Maltman
# Initial author: Matt Maltman


## Function ----
calc_pharm_supp <- function(partnered, over_60, Numb_dep, partial_cap, Main_carer_dep){
  # This needs to be updated to include the family benefit amounts
  
  PA_eligibility <- ifelse(over_60 == 1 | partnered == 0 & Numb_dep > 0 |
                             partial_cap == 1 | Main_carer_dep == 1 & Numb_dep > 0, 1, 0)
  PA = 0
  if (PA_eligibility == 1){
    if (partnered == 1){
      PA = PA_partnered
    }
   else {
      PA = PA_single
    }
  }
  return(PA)
}
