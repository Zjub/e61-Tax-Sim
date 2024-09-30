
calc_hub_ <- function(wage_rate, hours, HECSon, Medicare_levy_on) {
  work_income <- wage_rate * hours * 52
  taxable_benefit <- calc_benefit_abated(work_income,partner_earnings)[["taxable_benefit"]]
  
  gross_income = work_income + (taxable_benefit)*26
  
  income_tax <- calc_income_tax(gross_income,work_income)[["tax"]]
  
  medicare_levy <- ifelse(Medicare_levy_on == 1,
                          calc_income_tax(gross_income,work_income)[["medicarelevy"]], 0) 
  HECS_payment <- ifelse(HECSon == 1, calc_HECS(gross_income), 0)
  
  gross_fam_income = gross_income + partner_earnings # The supplements are not part of adjusted taxable income
  
  net_fam_a = calc_family_benefit(child_age,gross_fam_income,work_income,taxable_benefit)[["net_fam_a"]]*26
  
  net_fam_b = calc_family_benefit(child_age,gross_fam_income,work_income,taxable_benefit)[["net_fam_b"]]*26
  
  supp_benefit = calc_benefit_abated(work_income,partner_earnings)[["supp_benefit"]]
  
  
  JSP <- calc_benefit_abated(work_income,partner_earnings)[["JSP"]]
  PP_Pay <- calc_benefit_abated(work_income,partner_earnings)[["PP_Pay"]]
  RA <- max(calc_benefit_abated(work_income,partner_earnings)[["RA"]], 
            calc_family_benefit(child_age,gross_fam_income,work_income,taxable_benefit)[["RA"]])
  ES <- max(calc_benefit_abated(work_income,partner_earnings)[["ES"]], 
            calc_family_benefit(child_age,gross_fam_income,work_income,taxable_benefit)[["ES"]])
  
  supp_benefit <- RA + ES 
  
  net_income <- gross_income - income_tax - medicare_levy - HECS_payment +
                 net_fam_a + net_fam_b + ((supp_benefit) * 26)
  
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
    HECS_payment = HECS_payment, 
    medicare_levy = -medicare_levy
  ))
}