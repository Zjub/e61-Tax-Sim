
library(dplyr)

pacman::p_load(
  tidyverse,
  data.table,
  plotly,
  theme61)

person_file_path <- paste0("C:/Users/MatthewMaltman/Documents/SIH/SIH19BP.dta")

household_file_path <- paste0("C:/Users/MatthewMaltman/Documents/SIH/SIH19BH.dta")

income_unit_file_path <- paste0("C:/Users/MatthewMaltman/Documents/SIH/SIH19BI.dta")


# Load the SIH 2019 file 
SIH2019_Person <- haven::read_dta(person_file_path)
SIH2019_Household <- haven::read_dta(household_file_path)
SIH2019_IU <- haven::read_dta(income_unit_file_path)


#### Merge Person with Income Unit, by their household ID, the Income Unit Number within that household, 
#### And whether they are classed as part of the "Extended family" 

SIH2019_Person <- left_join(SIH2019_Person, SIH2019_IU, by = c('ABSHID', 'ABSIID', 'ABSFID'))

#### Merge Person and household file, based upon Household ID 

SIH2019_Person <- left_join(SIH2019_Person, SIH2019_Household, by = c('ABSHID'))
### This divides employee earnings (in total), by the total hours worked. 
### Note this is effectively the "average hourly wage", and includes earnings from overtime. 
SIH2019_Person$wage <- as.numeric(ifelse(SIH2019_Person$USHOURBC > 0, 
                                         SIH2019_Person$IWSSUCP8 / SIH2019_Person$USHOURBC,
                                         0))




SIH2019_Person$hours <- SIH2019_Person$USHOURBC

### A person is over 60 if they are aged above 59
SIH2019_Person$over_60 <- ifelse(SIH2019_Person$AGEEC > 59, 1, 0)

### IUType is 1 == couple with dependents, 2 == couple only, 3 == single parent, 
### 4 == Lone Person. 

#### If they are 1 or 2 they are a couple

SIH2019_Person$partnered <- ifelse(SIH2019_Person$IUTYPEP < 3, 1, 0)

### If they are 4 they live alone (for the purposes of being a single person income unit)
SIH2019_Person$living_alone <- ifelse(SIH2019_Person$IUTYPEP == 4, 1, 0)

### If they are 1 or 3 below they have children. 

SIH2019_Person$Have_dep <- ifelse(SIH2019_Person$IUTYPEP == 1 | SIH2019_Person$IUTYPEP == 3 , 1, 0) 

##### TENURPCF 1 == Own outright, 2 with a mortgage. 

SIH2019_Person$Home_owner <- ifelse(SIH2019_Person$TENURPCF < 3, 1, 0)

### This is a person's reported HECSdebt

SIH2019_Person$HECSDebt <- SIH2019_Person$LIAHECCP

### 99 if they don't have a dependent child, otherwise AGYDCIU is youngest dependent (up to 24 years old)

SIH2019_Person$young_child <- as.numeric(ifelse(SIH2019_Person$AGODCIU != 99, SIH2019_Person$AGYDCIU, NA_character_))

#### This takes all household earnings (from all sources and subtracts all governmetn allowances)

SIH2019_Person$HHPrivateEarnings <- SIH2019_Person$INCTSCU8 - SIH2019_Person$ITGCBCU8

#### This does the same at the person level. 

SIH2019_Person$PerPrivateEarnings <- SIH2019_Person$INCTSCP8 - SIH2019_Person$ITGCBCP8

### Assume that all private household earnings not from the individual are from their partner. 

SIH2019_Person$partner_earnings <- SIH2019_Person$HHPrivateEarnings - SIH2019_Person$PerPrivateEarnings 

### Number of depedent Children, up to 5. 

SIH2019_Person$Numb_dep <- SIH2019_Person$DEPKIDBC

### Calculate rent for the income unit. 
SIH2019_Person$Rent <- as.numeric(ifelse(SIH2019_Person$Home_owner != 1,
                          SIH2019_Person$IURENT,
                          NA_character_)) * 2  # Multiiply by 2 to get the fortnightly rent amount

#### No way of directly estimating whether the person is the main childcarer in the HH
### However, we can infer via - if they are a single parent they are the main carer, 
### If they are partnered with kids then 

SIH2019_Person <- SIH2019_Person %>%
  mutate(Main_carer_dep = ifelse(IUTYPEP == 3 | (IUTYPEP == 1 & PerPrivateEarnings < HHPrivateEarnings),
                                 1, 0))


SIH2019_Person$PPeligible <- ifelse((SIH2019_Person$young_child <= 6 |
                                       (SIH2019_Person$young_child <= 14 & SIH2019_Person$partnered == 0)) & 
                                      SIH2019_Person$Main_carer_dep == 1,1,0)



SIH2019_Person_Subset <- subset(SIH2019_Person, SIH2019_Person$QTRITRWH == 1)



child_ages <- vector("list", nrow(SIH2019_Person))

# Loop through each row of the dataframe
for (i in 1:nrow(SIH2019_Person)) {
  # Initialize an empty vector for the current row
  child_age <- integer(0)
  
  # Loop through the range 0 to 14 for both variable patterns
  for (j in 0:14) {
    # Construct the variable names based on the current index j
    iufa_var <- paste0("IUFA", j, "YB")
    iuma_var <- paste0("IUMA", j, "YB")
    
    # Check if the variables exist in the dataframe to avoid errors
    if (iufa_var %in% names(SIH2019_Person)) {
      # Append the age j, repeated according to the variable value
      child_age <- c(child_age, rep(j, SIH2019_Person[i, iufa_var]))
    }
    if (iuma_var %in% names(SIH2019_Person)) {
      child_age <- c(child_age, rep(j, SIH2019_Person[i, iuma_var]))
    }
  }
  
  # Store the constructed vector in the list
  child_ages[[i]] <- child_age
}

SIH2019_Person$child_ages <- child_ages

# Individual characteristics
Carer <- 0
Disability <- 0

# Meets conditions to access benefit on a personal level
ben_eligibility <- 1



list_of_results <- list()

for (i in 1:4) {
library(theme61) # Having some specific issues pulling this package in.
# Include functions ----
scripts <- list.files(pattern = "calc_")
for (script in scripts){
  source(script)
}

# Import parameters for the given quarter ----

policy_date <- ifelse(i == 1, "Q3_2019", "Q2_2020")
policy_date <- ifelse(i == 2, "Q4_2019", policy_date)
policy_date <- ifelse(i == 3, "Q1_2020", policy_date)

policy_date1 <- policy_date # 

policy_parameters_1 <- policy_parameters(policy_date1)

# Assign policy parameters from the policy file.
for (k in 1:nrow(policy_parameters_1)){
  assign(policy_parameters_1$Parameter[k],policy_parameters_1$Value[k])
}



SIH2019_Person_Subset <- subset(SIH2019_Person, SIH2019_Person$QTRITRWH == i)

SIH2019_Person_Subset <- subset(SIH2019_Person_Subset, SIH2019_Person_Subset$AGEEC < 66)


column_names1 <- c("AGEEC", "wage", "over_60", "partnered", "living_alone", "Have_dep", "Home_owner",
                   "HECSDebt", "young_child", "partner_earnings", "Numb_dep", "Rent",
                   "Main_carer_dep", "PPeligible", "child_ages", "hours")

column_names2 <- c("ITGCBCP6", "DISPSCP8", "IWSSUCP8", "IAGECP", "IFAMLSCP", "IPARENCP", "CWKCRA", "INEWLSCP", 
                   "INEWLSCM", "IFAMLSCM", "IPARENCM", "IAGECM", "CWKCRAM")


results <- data.frame(matrix(ncol = 42, nrow = 0))

colnames(results) <- c(column_names1, column_names2, "net_income", "income_tax", "work_income", "JSP", 
                       "PP_Pay", "RA", "ES", "taxable_benefit", "gross_income", "gross_fam_income", 
                       "net_fam_a_income", "net_fam_b_income", "HECS_payment")


for (j in 1:nrow(SIH2019_Person_Subset)) {
  values <- SIH2019_Person_Subset[j, column_names1]
  actual_values <- SIH2019_Person_Subset[j, column_names2]
  
  # Assign values to local variables for calculations
  list2env(values, envir = .GlobalEnv)
  list2env(actual_values, envir = .GlobalEnv)

  
  # Modify child_age based on Main_carer_dep
  child_age <- if (Main_carer_dep == 0) 99 else child_ages
  child_age <- unlist(child_age)
  
  # Perform the calculation
  HECS_on <- 1  # This seems to be a constant from your description
  calc_results <- calc_net_income_detailed_SIH(wage, hours, HECS_on)  # Adjust arguments if needed
  
  # Combine all values into a single list and append as a row to the results dataframe
  new_row <- c(values, actual_values, calc_results)
  results[nrow(results) + 1, ] <- new_row
}


results <- results %>%
  rename(
    TotalGovtPay_Stated = ITGCBCP6,
    DispInc_Stated = DISPSCP8,
    EmployeeInc_Stated = IWSSUCP8,
    AgedPen_Stated = IAGECP,
    FTB_Stated = IFAMLSCP,
    PP_Stated = IPARENCP,
    CRA_Stated = CWKCRA,
    JSP_Stated = INEWLSCP,
    JSP_SIH_Modelled = INEWLSCM,
    FTB_SIH_Modelled = IFAMLSCM,
    PP_SIH_Modelled = IPARENCM,
    AgedPen_SIH_Modelled = IAGECM,
    CRA_SIH_Modelled = CWKCRAM
  )

list_of_results[[i]] <- results

}

final_results <- bind_rows(list_of_results)

# Convert list to a comma-separated string
final_results$child_ages <- sapply(final_results$child_ages, function(x) paste(x, collapse = ","))

write.csv( final_results, "SIHComparison.csv")
