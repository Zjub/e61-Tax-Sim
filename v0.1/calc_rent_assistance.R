## Calculating of the commonwealth rent assistance ----
# Last update: 25/08/2023
# Updated by: Matt Nolan
# Initial author: Matt Nolan



# Function for rent assistance ----

calc_rent_assistance <- function(Rent,partnered,Numb_dep,living_alone,ben_eligibility){
  rent_assistance = 0
  if (ben_eligibility == 1){
    if (Numb_dep == 0){
      rent_assistance = ifelse(living_alone == 1,max((min(rent_max_single_la,Rent)-rent_min_single_la)*ra_rate,0),ifelse(partnered == 0,max((min(rent_max_single_share,Rent)-rent_min_single_share)*ra_rate,0),max((min(rent_max_couple,Rent)-rent_min_couple)*ra_rate,0)))
    }
    else if (Numb_dep < 3) {
      rent_assistance = ifelse(partnered == 0,max((min(rent_max_single_dep1,Rent)-rent_min_single_dep1)*ra_rate,0),max((min(rent_max_couple_dep1,Rent)-rent_min_couple_dep1)*ra_rate,0))
    }
    else {
      rent_assistance = ifelse(partnered == 0,max((min(rent_max_single_dep3,Rent)-rent_min_single_dep3)*ra_rate,0),max((min(rent_max_couple_dep3,Rent)-rent_min_couple_dep3)*ra_rate,0))
    }
  }
  return(rent_assistance)
}
