

SIH_Diagnositics_Path <- paste0("C:/Users/MatthewMaltman/Documents/Tax calculator/e61-Tax-Sim/v0.1/SIHcomparison.csv")

SIH_Diagnostics <- read.csv(SIH_Diagnositics_Path)

### Create a dummy variable for each of the transfer payments 
### set to 1 if the person reports recieving at least 1 dollar of the 
### payment

SIH_Diagnostics <- SIH_Diagnostics %>%
  mutate(
    AgedPen_rec = as.integer(AgedPen_Stated > 0),
    FTB_rec = as.integer(FTB_Stated > 0),
    PP_rec = as.integer(PP_Stated > 0),
    CRA_rec = as.integer(CRA_Stated > 0),
    JSP_rec = as.integer(JSP_Stated > 0)
  )

### Create a string, which reports the combination of payments the individual 
### is supposidely on, for diagnostic purposes. 

SIH_Diagnostics <- SIH_Diagnostics %>%
  rowwise() %>%
  mutate(PaymentTypes = toString(c("AgedPen_rec", 
                                   "FTB_rec",
                                   "PP_rec",
                                   "CRA_rec",
                                   "JSP_rec")[c(AgedPen_rec, FTB_rec, PP_rec, CRA_rec, JSP_rec) == 1]))


#### It appears that the calculator atm assumes you should get RA if you have an income level
# sufficient to be on the JSP, rather than that whether you are recieving the JSP 
## This is a bug that I should fix later on. For now, you can manually set CRA to 
### 0 via uncommenting the below code. 

#SIH_Diagnostics$net_income <-  ifelse(SIH_Diagnostics$PaymentTypes == "",
#                                       SIH_Diagnostics$net_income - SIH_Diagnostics$RA, 
#                                       SIH_Diagnostics$net_income)

#SIH_Diagnostics$RA<-  ifelse(SIH_Diagnostics$PaymentTypes == "", 0, SIH_Diagnostics$RA)


# Ensure PaymentTypes is a factor for better plotting
SIH_Diagnostics$PaymentTypes <- factor(SIH_Diagnostics$PaymentTypes)

### SIH records FTB togethe (both A and B). Our calc does them seperately. 
#### Make a variable that combines our calculator's estimates. 

SIH_Diagnostics$FTB <- SIH_Diagnostics$net_fam_a_income + SIH_Diagnostics$net_fam_b_income

library(ggplot2)
library(theme61)


# Define a function to plot the distribution if at least one variable is not zero
plot_distribution <- function(df, var1, var2, label) {
  # Filter rows where at least one of the variables is not zero
  filtered_df <- df[!(df[[var1]] == 0 & df[[var2]] == 0), ]
  # Calculate the difference
  filtered_df$difference <- filtered_df[[var1]] - filtered_df[[var2]]
  filtered_df$recipient_status <- ifelse(filtered_df[[var1]] == 0, "SIH Non Recipient",
                                         ifelse(filtered_df[[var2]] == 0, "e61_Tax_Calc Non Recipient", 
                                                "Recipient"))
  
  
  max_diff <- max(abs(filtered_df$difference), na.rm = TRUE)
  
  # Create the plot with facets
  p <- ggplot(filtered_df, aes(x=difference, fill = recipient_status)) +
    geom_histogram(bins=30,  color="black", position = "stack") +
    facet_wrap(~ PaymentTypes) + # Add facet_wrap
    labs(title=paste("Distribution of", label),
         subtitle=paste("Number of rows used:", nrow(filtered_df)),
         x="Difference", y="Density") +
    theme_minimal() +
    xlim(-max_diff, max_diff) + geom_vline(xintercept = 0) + 
    theme(legend.position = "bottom")
  
  print(p)
}




# Apply the function to each set of variables

### NOTE: a positive value indicates we have *underestimated* the level of the payment. 
### a negative value indicates that we have *overestimated* the level of the payment. 
#### Variables ending in "Stated' refer to what the person actually reports recieving. 
#### Variables ending in "SIH_Modelled" refer to what the SIH calculator (in the data itself)
#### report the person should be recieving. 
plot_distribution(SIH_Diagnostics, "DispInc_Stated", "net_income", "DispInc_stated - net_income")
plot_distribution(SIH_Diagnostics, "JSP_Stated", "JSP", "JSP_stated - JSP")
plot_distribution(SIH_Diagnostics, "JSP_SIH_Modelled", "JSP", "JSP_SIH_Modelled - JSP")
plot_distribution(SIH_Diagnostics, "PP_Stated", "PP_Pay", "PP_Pay_Stated - PP_Pay")
plot_distribution(SIH_Diagnostics, "PP_SIH_Modelled", "PP_Pay", "PP_SIH_Modelled - PP_Pay")
plot_distribution(SIH_Diagnostics, "CRA_Stated", "RA", "CRA_Stated - RA")
plot_distribution(SIH_Diagnostics, "CRA_SIH_Modelled", "RA", "CRA_SIH_Modelled - RA")
plot_distribution(SIH_Diagnostics, "FTB_SIH_Modelled", "FTB", "FTB_SIH_Modelled - FTB")
plot_distribution(SIH_Diagnostics, "FTB_Stated", "FTB", "FTB_Stated - FTB")



