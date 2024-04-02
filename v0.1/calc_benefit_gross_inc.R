## Calculating of the gross benefit payment ----
# Last update: 13/03/2024
# Updated by: Matt Nolan
# Initial authors: Matt Nolan


## Function ----
calc_benefit_gross_inc <- function(){
  gross_benefit = calc_main_benefit_inc(Numb_dep,partnered,Main_carer_dep)[["JSP_pay"]] + calc_main_benefit_inc(Numb_dep,partnered,Main_carer_dep)[["PP_pay"]] + calc_rent_assistance_inc(Rent,partnered,Numb_dep,living_alone,ben_eligibility) + calc_energy_supp(partnered,Have_dep,over_60)
  taxable_benefit = calc_main_benefit_inc(Numb_dep,partnered,Main_carer_dep)[["JSP_pay"]] + calc_main_benefit_inc(Numb_dep,partnered,Main_carer_dep)[["PP_pay"]]
  supp_benefit = gross_benefit - taxable_benefit
  return(list(gross_benefit = gross_benefit,taxable_benefit = taxable_benefit,supp_benefit = supp_benefit))
}
