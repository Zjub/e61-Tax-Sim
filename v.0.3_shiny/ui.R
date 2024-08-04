#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(shinycssloaders)

fluidPage(
  # Application title
  titlePanel("e61 Tax and Transfer Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        id = "sidebar_tabs",
        tabPanel("Person Characteristics",
      # Radio buttons for selecting work basis
      radioButtons("basis", "Select basis:",
                   choices = list("Hours" = "hours", "Income" = "income"),
                   selected = "hours"),
      
      # Conditional panel for hours basis
      conditionalPanel(
        condition = "input.basis == 'hours'",
        numericInput("max_hours", "The  maximum weekly hours the individual would consider working:", value = 40, min = 1, max = 168),
        numericInput("wages", "Wages (per hour):", value = 25, min = 1)
      ),
      
      # Conditional panel for income basis
      conditionalPanel(
        condition = "input.basis == 'income'",
        numericInput("max_income", "The maximum income the individual could earn in a year is:", value = 50000, min = 1)
      ),
      
      checkboxInput("partnered", "Partnered"),
      
      # Conditional panel for Partner's income
      conditionalPanel(
        condition = "input.partnered == true",
        numericInput("partner_income", "Partner's Income (Annual):", value = 0, min = 0)
      ),
      
      # Check box for Renter
      checkboxInput("renter", "Renter"),
      
      # Conditional panel for Weekly Rent
      conditionalPanel(
        condition = "input.renter == true",
        numericInput("weekly_rent", "Weekly Rent:", value = 0, min = 0)
      ),
      
      # Numeric input for Number of Dependents
      numericInput("num_dependents", "Number of Dependents:", value = 0, min = 0, max = 10),
      
      # UI output for dynamic child age inputs
      uiOutput("child_age_inputs"),
      
      checkboxInput("hecs_debt", "Include HECS debt?"),
      
      # Conditional panel for Total HECS Debt
      conditionalPanel(
        condition = "input.hecs_debt == true",
        numericInput("total_hecs_debt", "Total HECS Debt:", value = 0, min = 0)
      ),
      
      checkboxInput("medicare_levy", "Exclude the Medicare Levy?"),
        ),
      tabPanel("Tax and Transfer Characteristics",
               checkboxInput("edit_tax_brackets", "Edit Tax Brackets"),
               conditionalPanel(
                 condition = "input.edit_tax_brackets == true",
                 numericInput("num_tax_brackets", "Number of Tax Brackets:", value = 4, min = 0, max = 10),
                 checkboxInput("turn_off_LITO", "Turn off LITO (Low Income Tax Offset)"),
                 checkboxInput("turn_off_BTO", "Turn off the Beneficiary tax offset"),
                 uiOutput("tax_brackets_inputs")
               ), 
               checkboxInput("edit_job_seeker", "Edit Job Seeker and Parenting Payment (All values are weekly)"),
               conditionalPanel(
                 condition = "input.edit_job_seeker == true",
                 numericInput("job_seeker_single", "Job Seeker - Single Amount:", value =  693.1 , min = 0),
                 numericInput("job_seeker_couple", "Job Seeker - Couple Amount:", value = 631.2, min = 0),
                 numericInput("PP_single", "Parenting Payment - Single Amount:", value = 922.1, min = 0),
                 numericInput("PP_couple", "Parenting Payment - Couple Amount:", value = 693.1, min = 0),
              #   numericInput("PP_add", "Parenting Payment - Abatement Threshold Increase Per Child for Singles:", value = 24.6, min = 0),
               #  numericInput("num_abatement_thresholds", "Number of Abatement Thresholds:", value = 2, min = 0, max = 2),
               #  uiOutput("abatement_inputs")
               ),   
              checkboxInput("UBI_mode", "Introduce a UBI (Everyone recieves Jobseeker or the Parenting Payment
                            at their maximum rate)"),
      )
      ),
      
      actionButton("update_input", "Update")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
     
      fluidRow(
        column(width = 6, withSpinner(plotlyOutput("plot", width = "100%", height = "600px"))),
        column(width = 6, withSpinner(plotlyOutput("plot2", width = "100%", height = "600px")))
      ), 
      textOutput("debug_text"), 
      uiOutput("Title_1"),
      textOutput("interpretation_text"),
      uiOutput("Title_2"),
      textOutput("interpretation_text_2"),
      uiOutput("Title_3"),
      textOutput("interpretation_text_3"),
      uiOutput("Title_4"),
      textOutput("interpretation_text_4"),
      div(style = "margin-top: 20px;",  # Adds some margin at the top
          p("By Matthew Maltman and Matt Nolan, last updated ", Sys.Date(), ". Currently using Q3 2024 Policy Parameters")# Add a text output for debugging
      )
    )
  ),
  tags$script(HTML("
        $(document).ready(function() {
            var enforceLimits = function(id) {
                var input = $('#' + id);
                input.on('input', function() {
                    var min = input.attr('min');
                    var max = input.attr('max');
                    var value = input.val();
                    if (min && parseInt(value) < parseInt(min)) {
                        input.val(min);
                    }
                    if (max && parseInt(value) > parseInt(max)) {
                        input.val(max);
                    }
                });
            };
            
            enforceLimits('max_hours');
            enforceLimits('wages');
            enforceLimits('num_dependents');
            enforceLimits('weekly_rent');
            enforceLimits('partner_income');
            enforceLimits('total_hecs_debt');
            enforceLimits('max_income');
            enforceLimits('num_tax_brackets');
            enforceLimits('job_seeker_single');
            enforceLimits('job_seeker_couple');
            enforceLimits('PP_single');
            enforceLimits('PP_couple');
        });
    "))
)

