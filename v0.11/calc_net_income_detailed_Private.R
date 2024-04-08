## Calculating of the individuals net income ----
# Last update: 25/08/2023
# Updated by: Matt Nolan
# Initial author: Matt Nolan

calc_net_income_detailed_private <- function(private_earnings, HECSon) {
  work_income <- private_earnings
  abated_benefit <- calc_benefit_abated(work_income,partner_earnings)[["net_benefit"]]
  taxable_benefit <- calc_benefit_abated(work_income,partner_earnings)[["taxable_benefit"]]
  gross_income = work_income + (taxable_benefit)*26
  income_tax <- calc_income_tax(gross_income,work_income)
  HECS_payment <- ifelse(HECSon == 1, calc_HECS(gross_income), 0)
  gross_fam_income = gross_income + partner_earnings # The supplements are not part of adjusted taxable income
  net_fam_a = calc_family_benefit(child_age,gross_fam_income,work_income,taxable_benefit)[["net_fam_a"]]*26
  net_fam_b = calc_family_benefit(child_age,gross_fam_income,work_income,taxable_benefit)[["net_fam_b"]]*26
  supp_benefit = calc_benefit_abated(work_income,partner_earnings)[["supp_benefit"]]
  
  
  JSP <- calc_benefit_abated(work_income,partner_earnings)[["JSP"]]
  PP_Pay <- calc_benefit_abated(work_income,partner_earnings)[["PP_Pay"]]
  RA <- calc_benefit_abated(work_income,partner_earnings)[["RA"]]
  ES <- calc_benefit_abated(work_income,partner_earnings)[["ES"]]
  
  RA_to_0_flag <- ifelse(JSP + PP_Pay > 0  | 
                           (calc_family_benefit(child_age,gross_fam_income,work_income,taxable_benefit)[["net_fam_a"]] - (fam_basic_pay * Numb_dep)) > 0,
                         0, 1)
  
  supp_benefit <- ifelse(RA_to_0_flag == 1, 0, supp_benefit)
  
  RA <- ifelse(RA_to_0_flag == 1, 0, RA)
  ES <- ifelse(RA_to_0_flag == 1, 0, ES)

  net_income <- gross_income - income_tax - HECS_payment + net_fam_a + net_fam_b + ((supp_benefit) * 26)
  
  return(list(
    net_income = net_income,
    income_tax = income_tax,
    work_income = work_income,
    JSP = JSP * 26,
    PP_Pay = PP_Pay * 26 , 
    RA = RA * 26 , 
    ES = ES * 26 , 
    taxable_benefit = taxable_benefit,
    gross_income = gross_income,
    gross_fam_income = gross_fam_income, 
    net_fam_a_income = net_fam_a, 
    net_fam_b_income = net_fam_b, 
    HECS_payment = HECS_payment
  ))
}