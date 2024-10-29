## Calculating of the energy supplement ----
# Last update: 25/08/2023
# Updated by: Matt Nolan
# Initial author: Matt Nolan


## Function ----
calc_energy_supp <- function(ben_eligibility,partnered,Have_dep,over_60){
  # This needs to be updated to include the family benefit amounts
  energy_supp = 0
  if (ben_eligibility == 1){
    if (partnered == 1){
      energy_supp = energy_partnered
    }
    else if (Have_dep == 0 & over_60 == 0){
      energy_supp = energy_single
    }
    else if (over_60 == 1){
      energy_supp = energy_over60
    }
    else {
      energy_supp = energy_single_dep
    }
  }
  return(energy_supp)
}
