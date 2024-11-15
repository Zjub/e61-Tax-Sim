#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

################################# Load in Relevant Packages 

if(!require("pacman")) install.packages("pacman")

pacman::p_load(shiny, plotly, shinycssloaders, shinyBS)

############################## Create the UI ################################3###########

fluidPage(
  ###################  Title 
  titlePanel("e61 Tax and Transfer Calculator"),
  
  tags$h2("Workforce Disincentives for the Working Aged Population (Ages 22-54)", 
          style = "font-weight: normal; font-size: 1.5em; margin-top: -10px;"),
  
  
  ############### Side Bar
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        id = "sidebar_tabs",
        
        ############ Person Characteristics Panel, where the user enters their information #######
        
        tabPanel("Person Characteristics",
                 # Button for selecting work basis
                 radioButtons("basis", "Select basis:",
                              choices = list("Hours" = "hours", "Income" = "income"),
                              selected = "hours"),
                 
                 # Depending on what is selected, either load in a box for wage + hours, or just income
                 
                 #### Hours 
                 conditionalPanel(
                   condition = "input.basis == 'hours'",
                   
                   numericInput("max_hours",
                                "The  maximum weekly hours the individual would consider working:",
                                value = 40, min = 1, max = 168),
                   numericInput("wages", "Wages (per hour):", value = 25, min = 1)
                 ),
                 
                 conditionalPanel(
                   condition = "input.basis == 'hours'",
                   numericInput("target_hours",
                                "The weekly hours of work you'd like information about is:",
                                value = 20, min = 1)#, max = max_x_var)
                 ),
                 
                 ### Income
                 conditionalPanel(
                   condition = "input.basis == 'income'",
                   numericInput("max_income",
                                "The maximum income the individual could earn in a year is:",
                                value = 50000, min = 1)
                 ),
                 
                 conditionalPanel(
                   condition = "input.basis == 'income'",
                   numericInput("target_income",
                                "The income you'd like information about is:",
                                value = 25000, min = 1)#,max = max_x_var)
                 ),
                 #### Check Box for Partnered
                 checkboxInput("partnered", "Partnered"),
                 
                 #### If Partnered selected, load a box for Partner's income
                 conditionalPanel(
                   condition = "input.partnered == true",
                   numericInput("partner_income", "Partner's Income (Annual):", value = 0, min = 0)
                 ),
                 
                 # Check box for Renter
                 checkboxInput("renter", "Renter"),
                 
                 # If Renter Selected, load a box for Weekly Rent
                 conditionalPanel(
                   condition = "input.renter == true",
                   numericInput("weekly_rent", "Weekly Rent:", value = 0, min = 0)
                 ),
                 
                 # Numeric input for Number of Dependents
                 numericInput("num_dependents", "Number of Children (dependents):",
                              value = 0, min = 0, max = 10),
                 
                 # Based upon the number of children selected, load in that many boxes for ages. 
                 uiOutput("child_age_inputs"),
                 # Box to include HECS debt
                 checkboxInput("hecs_debt", "Include HECS debt?"),
                 
                 # If HECS debt selected, load in a box to enter their HECS debt
                 conditionalPanel(
                   condition = "input.hecs_debt == TRUE",
                   numericInput("total_hecs_debt", "Total HECS Debt:", value = 0, min = 0)
                 ),
                 # Box to include the medicare levy 
                 checkboxInput("medicare_levy", "Exclude the Medicare Levy?"),
        ),
        ############ TTC Panel, where the user can change our TT system #######
        
        tabPanel("Tax and Transfer Changes",
                 # Box to alter the tax system
                 checkboxInput("edit_tax_brackets", "Edit Tax Brackets"),
                 # If selected, load a box to select the number of tax brackets desired, and 
                 # 2* that many numeric boxes. One for the tax rate, and one for the threshold 
                 conditionalPanel(
                   condition = "input.edit_tax_brackets == TRUE",
                   numericInput("num_tax_brackets", "Number of Tax Brackets:",
                                value = 4, min = 0, max = 10),
                   checkboxInput("turn_off_LITO",
                                 "Turn off LITO (Low Income Tax Offset)"),
                   checkboxInput("turn_off_BTO",
                                 "Turn off the Beneficiary tax offset"),
                   uiOutput("tax_brackets_inputs")
                 ), 
                 # Box to edit Jobseeker
                 checkboxInput("edit_job_seeker", 
                               "Edit Job Seeker and Parenting Payment (All values are weekly)"),
                 # load 4 boxes, (Job Seeker, PP) * (Single, Couple)
                 conditionalPanel(
                   condition = "input.edit_job_seeker == TRUE",
                   numericInput("job_seeker_single", "Job Seeker - Single Amount:",
                                value =  693.1 , min = 0),
                   numericInput("job_seeker_couple", "Job Seeker - Couple Amount:",
                                value = 631.2, min = 0),
                   numericInput("PP_single", "Parenting Payment - Single Amount:",
                                value = 922.1, min = 0),
                   numericInput("PP_couple", "Parenting Payment - Couple Amount:",
                                value = 693.1, min = 0)
                 ),  
                 # Box for "UBI" mode. Turning off all abatements for PP and JSP
                 checkboxInput("UBI_mode",
                               "[Experimental] Introduce a UBI (Everyone recieves Jobseeker or the Parenting Payment
                            at their maximum rate)"),
                 # Box to not abate RA alongside the main payment
                 checkboxInput("RA_Abate",
                               "Do not abate Rent Assistance alongside the main payment"),
        )
      ),
      ### Button to run the calculator    
      actionButton("update_input", "Update")
    ),
    
    ############### Main Panel ************* Results #########################################
    mainPanel(
      # Create a matching tabsetPanel that will be controlled by the sidebar
      tabsetPanel(
        id = "main_tabs",
        type = "hidden",  # Hide the tab navigation since we're controlling it from sidebar
        
        # First tab content (Person Characteristics)
        tabPanel("Person Characteristics",
                 checkboxInput("toggle_plot", "Switch to Detailed Plot", value = FALSE),
                 
                 withSpinner(plotlyOutput("plot", width = "100%", height = "600px")),
                 
                 tags$div(
                   style = "margin-top: 30px; margin-bottom: 50px;",
                   bsCollapse(
                     bsCollapsePanel(
                       title = "How do I interpret these incomes?",
                       style = "primary",
                       uiOutput("Income_Title_1"),
                       textOutput("Income_interpretation_text_1"),
                       uiOutput("Income_Title_2"),
                       textOutput("Income_interpretation_text_2")
                     )
                   )
                 ),
                 
                 withSpinner(plotlyOutput("plot2", width = "100%", height = "600px")),
                 
                 tags$div(
                   style = "margin-top: 30px; margin-bottom: 50px;",
                   bsCollapse(
                     bsCollapsePanel(
                       title = "What are these tax and income concepts?",
                       style = "primary",
                       br(),
                       tags$div(style = "margin-top: 5px;"),
                       textOutput("debug_text"),
                       br(),
                       uiOutput("Title_1"),
                       textOutput("interpretation_text"),
                       br(),
                       uiOutput("Title_2"),
                       textOutput("interpretation_text_2"),
                       br(),
                       uiOutput("Title_3"),
                       textOutput("interpretation_text_3"),
                       br(),
                       uiOutput("Title_4"),
                       textOutput("interpretation_text_4"),
                       br()
                     )
                   )
                 )
        ),
        
        # Second tab content (Tax and Transfer Characteristics)
        tabPanel("Tax and Transfer Changes",
                 tags$div(
                   style = "margin-top: 30px; margin-bottom: 50px;",
                   bsCollapse(
                     bsCollapsePanel(
                       title = "What is this?",
                       style = "primary",
                       textOutput("Intro_tax_text_1")
                     )
                   )
                 ),
                 
                 withSpinner(plotlyOutput("plot_tax", width = "100%", height = "600px")),
                 withSpinner(plotlyOutput("plot", width = "100%", height = "600px")),
                 #withSpinner(plotlyOutput("plot2_tax", width = "100%", height = "600px")),
                 
                 tags$div(
                   style = "margin-top: 30px; margin-bottom: 50px;",
                   bsCollapse(
                     bsCollapsePanel(
                       title = "What Assumptions have you made",
                       style = "primary",
                       textOutput("Ass_interpretation_text_1"),
                       uiOutput("Ass_interpretation_text_2")
                     )
                   )
                 )
        )
      )
    )
  ),
    
  ################################
  # Below code enforces the maximum and minimums for all the boxes, preventing the user from
  ### being able to put in unreasonable values . 
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


