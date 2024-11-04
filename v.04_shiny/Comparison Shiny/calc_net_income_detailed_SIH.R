## Calculating of the individuals net income ----
# Last update: 25/08/2023
# Updated by: Matt Nolan
# Initial author: Matt Nolan

calc_net_income_detailed_SIH <- function(wage_rate, hours, HECSon) {
  work_income <- wage_rate * hours * 52
  taxable_benefit <- calc_benefit_abated(work_income,partner_earnings)[["taxable_benefit"]]
  
  ### Imperfect proxy for eligibility
  taxable_benefit <- ifelse(INEWLSCP + IPARENCP > 0, taxable_benefit, 0)
  
  gross_income = work_income + (taxable_benefit)*26
  income_tax <- calc_income_tax(gross_income,work_income)[["tax"]]
  HECS_payment <- ifelse(HECSon == 1, calc_HECS(gross_income), 0)
  gross_fam_income = gross_income + partner_earnings # The supplements are not part of adjusted taxable income
  net_fam_a = calc_family_benefit(child_age,gross_fam_income,work_income,taxable_benefit)[["net_fam_a"]]*26
    net_fam_b = calc_family_benefit(child_age,gross_fam_income,work_income,taxable_benefit)[["net_fam_b"]]*26
 # supp_benefit = calc_benefit_abated(work_income,partner_earnings)[["supp_benefit"]]
  
  
  JSP <- calc_benefit_abated(work_income,partner_earnings)[["JSP"]]
  JSP <- ifelse(INEWLSCP > 0, JSP, 0)
  PP_Pay <- calc_benefit_abated(work_income,partner_earnings)[["PP_Pay"]]
  PP_Pay <- ifelse(IPARENCP > 0, PP_Pay, 0)
  RA <- max(calc_benefit_abated(work_income,partner_earnings)[["RA"]], 
            calc_family_benefit(child_age,gross_fam_income,work_income,taxable_benefit)[["RA"]])
  ES <- max(calc_benefit_abated(work_income,partner_earnings)[["ES"]], 
            calc_family_benefit(child_age,gross_fam_income,work_income,taxable_benefit)[["ES"]])
  
  net_fam_a <- ifelse(IFAMLSCM > 0,  net_fam_a, 0)
  net_fam_b <- ifelse(IFAMLSCM > 0,  net_fam_b, 0)
  
  supp_benefit <- RA + ES 
  net_income <- gross_income - income_tax - HECS_payment + net_fam_a + net_fam_b + ((supp_benefit) * 26)
  
  return(list(
    net_income = net_income / 52,
    income_tax = income_tax /52,
    work_income = work_income /52,
    JSP = JSP / 2,
    PP_Pay = PP_Pay / 2   , 
    RA = RA /2   , 
    ES = ES / 2  , 
    taxable_benefit = taxable_benefit / 2  ,
    gross_income = gross_income / 52 ,
    gross_fam_income = gross_fam_income / 52 , 
    net_fam_a_income = net_fam_a / 52, 
    net_fam_b_income = net_fam_b / 52 , 
    HECS_payment = HECS_payment / 52 
  ))
}