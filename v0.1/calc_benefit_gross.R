## Calculating of the gross benefit payment ----
# Last update: 25/08/2023
# Updated by: Matt Nolan
# Initial author: Matt Nolan


## Function ----
calc_benefit_gross <- function(){
  gross_benefit = calc_main_benefit(Numb_dep,partnered,Main_carer_dep)[["JSP_pay"]] + calc_main_benefit(Numb_dep,partnered,Main_carer_dep)[["PP_pay"]] + calc_rent_assistance(Rent,partnered,Numb_dep,living_alone,ben_eligibility) + calc_energy_supp(partnered,Have_dep,over_60)
  taxable_benefit = calc_main_benefit(Numb_dep,partnered,Main_carer_dep)[["JSP_pay"]] + calc_main_benefit(Numb_dep,partnered,Main_carer_dep)[["PP_pay"]]
  supp_benefit = gross_benefit - taxable_benefit
  return(list(gross_benefit = gross_benefit,taxable_benefit = taxable_benefit,supp_benefit = supp_benefit))
}
