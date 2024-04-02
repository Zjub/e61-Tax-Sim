## Calculating the abated benefit received ----
# Last update: 13/03/2024
# Updated by: Matt Nolan
# Initial authors: Matt Nolan
# The logic here is that we first abate down on the based on partner income.  We then apply two single abatement rates - the one when you have a child AND PP_S, and the one when you don't.  The one when you do not is the same as the abatement rate faced when you have a partner, which is why it is the else command.


## Function ----
calc_benefit_abated_inc <- function(work_income,partner_earnings) {
  # Define terms
  JSP_S_ND_arate <- c(0,JSP_S_ND_arate_1,JSP_S_ND_arate_2)
  JSP_S_ND_athresh <- c(0,JSP_S_ND_athresh_1,JSP_S_ND_arate_2)

  # Abate down based on partner income first.
  taxable_benefit = calc_benefit_gross_inc()[["taxable_benefit"]]
  gross_benefit = calc_benefit_gross_inc()[["gross_benefit"]]
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
