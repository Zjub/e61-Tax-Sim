## Calculating of the gross benefit payment ----
# Last update: 25/08/2023
# Updated by: Matt Nolan
# Initial author: Matt Nolan


## Function ----
calc_benefit_gross <- function(){

  JSP_Pay <- calc_main_benefit(Numb_dep,partnered,Main_carer_dep)[["JSP_pay"]]
  PP_Pay <- calc_main_benefit(Numb_dep,partnered,Main_carer_dep)[["PP_pay"]]
  RA <- calc_rent_assistance(Rent,partnered,Numb_dep,living_alone,ben_eligibility, Home_owner)
  ES <- calc_energy_supp(ben_eligibility, partnered,Have_dep,over_60)
  PA <- calc_pharm_supp(partnered, over_60, Numb_dep, partial_cap, Main_carer_dep) 
  gross_benefit = JSP_Pay + PP_Pay + RA + ES + PA 
  taxable_benefit = JSP_Pay + PP_Pay 
  supp_benefit = RA + ES + PA 
  
  return(list(gross_benefit = gross_benefit,
              taxable_benefit = taxable_benefit,
              supp_benefit = supp_benefit,
              JSP_Pay = JSP_Pay,
              PP_Pay = PP_Pay,
              RA = RA, ES = ES, PA = PA))
}
