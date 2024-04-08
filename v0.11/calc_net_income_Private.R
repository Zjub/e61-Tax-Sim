## Calculating of the individuals net income ----
# Last update: 25/08/2023
# Updated by: Matt Nolan
# Initial author: Matt Nolan

calc_net_income_private <- function(private_earnings, HECS_on) {
  work_income <- private_earnings
  abated_benefit <- calc_benefit_abated(work_income,partner_earnings)[["net_benefit"]]
  taxable_benefit <- calc_benefit_abated(work_income,partner_earnings)[["taxable_benefit"]]
  gross_income = work_income + (taxable_benefit)*26
  income_tax <- calc_income_tax(gross_income,work_income)
  HECS_payment <- ifelse(HECS_on == 1, calc_HECS(gross_income), 0)
  gross_fam_income = gross_income + partner_earnings # The supplements are not part of adjusted taxable income
  fam_a_income = calc_family_benefit(child_age,gross_fam_income,work_income,taxable_benefit)[["net_fam"]]*26
  supp_benefit = calc_benefit_abated(work_income,partner_earnings)[["supp_benefit"]]

  RA_to_0_flag <- ifelse(taxable_benefit > 0  | 
                           (calc_family_benefit(child_age,gross_fam_income,work_income,taxable_benefit)[["net_fam_a"]] - (fam_basic_pay * Numb_dep)) > 0,
                         0, 1)
  
  supp_benefit <- ifelse(RA_to_0_flag == 1, 0, supp_benefit)
    
  net_income <- gross_income - income_tax - HECS_payment + fam_a_income + (supp_benefit * 26)
  
  
  
  return(list(
    net_income = net_income,
    income_tax = income_tax,
    work_income = work_income,
    abated_benefit = abated_benefit,
    taxable_benefit = taxable_benefit,
    gross_income = gross_income,
    gross_fam_income = gross_fam_income,
    fam_a_income = fam_a_income,
    supp_benefit = supp_benefit,
    HECS_payment = HECS_payment
  ))
}