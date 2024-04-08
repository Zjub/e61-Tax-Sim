## Calculating of the energy supplement ----
# Last update: 13/03/2024
# Updated by: Matt Nolan
# Initial authors: Matt Nolan


## Function ----
calc_energy_supp_inc <- function(ben_eligibility,partnered,Have_dep,over_60){
  # This needs to be updated to include the family benefit amounts
  over_60 <- 0 # Hard coding this

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
