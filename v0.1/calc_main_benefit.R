## Calculating main benefit ----
# Last update: 25/08/2023
# Updated by: Matt Nolan
# Initial author: Matt Nolan

# Function calculating main benefit ----

calc_main_benefit <- function(Numb_dep,partnered,Main_carer_dep) {
  PPeligible <- ifelse((young_child <= 6 | (young_child <= 8 & partnered == 0)) & Main_carer_dep == 1,1,0)
  
  PP_S_athresh <- PP_S_athresh_base + PP_S_athresh_mult*(Numb_dep - 1)
  
  JSP_pay <- 0
  PP_pay <- 0
  if (PPeligible == 1){
    PP_pay <- ifelse(partnered == 1,PP_C_pay,PP_S_pay)
  }
  else {
    JSP_pay <- ifelse(Numb_dep == 0 & partnered == 0,JSP_S_ND_pay,ifelse(Numb_dep > 0 & partnered == 0, JSP_S_D_pay,ifelse(Numb_dep == 0,JSP_C_ND_pay,JSP_C_D_pay)))
  }
  return(list(PP_pay = PP_pay,JSP_pay = JSP_pay))
}
