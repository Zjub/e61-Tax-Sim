## Calculating of the individuals net income ----
# Last update: 13/03/2024
# Updated by: Matt Nolan
# Initial authors: Matt Nolan

calc_net_income_inc <- function(work_income,tax_brackets,tax_scale) {
  work_income <- work_income
  abated_benefit <- calc_benefit_abated(work_income,partner_earnings)[["net_benefit"]]
  taxable_benefit <- calc_benefit_abated(work_income,partner_earnings)[["taxable_benefit"]]
  gross_income = work_income + (taxable_benefit)*26

  income_tax <- calc_income_tax(gross_income,work_income,tax_brackets,tax_scale)

  gross_fam_income = gross_income + partner_earnings # The supplements are not part of adjusted taxable income
  fam_a_income = calc_family_benefit(child_age,gross_fam_income,work_income,taxable_benefit)[["net_fam"]]*26
  net_income <- gross_income - income_tax + fam_a_income + abated_benefit - taxable_benefit

  return(list(net_income =net_income,income_tax = income_tax))
}
