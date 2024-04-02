## Calculating the abated benefit received ----
# Last update: 25/08/2023
# Updated by: Matt Nolan
# Initial author: Matt Nolan


## Function ----
calc_benefit_abated <- function(work_income,partner_earnings) {
  # Abate down based on partner income first.
  taxable_benefit = calc_benefit_gross()[["taxable_benefit"]]
  gross_benefit = calc_benefit_gross()[["gross_benefit"]]
  abate = 0
  abate <- ifelse(Have_dep == 1, max((partner_earnings/26 - partner_thresh_dep)*partner_abate,0),max((partner_earnings/26 - partner_thresh)*partner_abate,0))
  tempwi <- work_income/26 # Fortnightly pay
  if (PPeligible == 1 & partnered == 0){
    if (work_income/26 > PP_S_athresh){
      abate <- abate + (tempwi - PP_S_athresh)*PP_S_arate
    }
  }
  else {
    for (i in length(JSP_S_ND_athresh):2) {
      if (work_income/26 > JSP_S_ND_athresh[i]){
        abate <- abate + (tempwi - JSP_S_ND_athresh[i])*JSP_S_ND_arate[i]
        tempwi <- JSP_S_ND_athresh[i]
      }
    }
  }
  taxable_benefit <- taxable_benefit - abate
  net_benefit <- gross_benefit - abate
  return(list(taxable_benefit = max(taxable_benefit,0), net_benefit = max(net_benefit,0)))
}
