################################# Load in Relevant Packages 

if(!require("pacman")) install.packages("pacman")

pacman::p_load(shiny, plotly, shinycssloaders)

############################## Create the UI ###########################################

fluidPage(
  ###################  Title 
  titlePanel("e61 Tax and Transfer Calculator"),
  
  tags$h2("Workforce Disincentives for the Working Aged Population (Ages 22-54)", 
          style = "font-weight: normal; font-size: 1.5em; margin-top: -10px;"),
  
  ############### Layout with Left Sidebar Panel and Center Main Panel
  
  sidebarLayout(
    
    # Left Side Panel
    sidebarPanel(
      tabsetPanel(
        id = "sidebar_tabs",
        
        tabPanel("Person Characteristics",
                 radioButtons("basis", "Select basis:",
                              choices = list("Hours" = "hours", "Income" = "income"),
                              selected = "hours"),
                 
                 # Hours and Wages Inputs Side-by-Side
                 conditionalPanel(
                   condition = "input.basis == 'hours'",
                   numericInput("max_hours", "The hours range you want to compare the individuals over:", value = 40, min = 1, max = 168),
                   fluidRow(
                     column(width = 6, tags$div(style = "text-align: center; font-weight: bold;", "Person 1")),
                     column(width = 6, tags$div(style = "text-align: center; font-weight: bold;", "Person 2"))
                   ),
                   tags$div(style = "margin-top: 10px; font-weight: bold;", "Hourly Wages"),
                   fluidRow(
                     column(width = 6, numericInput("wages", NULL, value = 25, min = 1)),
                     column(width = 6, numericInput("wages2", NULL,  value = 30, min = 1))
                   )
                 ),
                 
                 # Income Input
                 conditionalPanel(
                   condition = "input.basis == 'income'",
                   fluidRow(
                     column(width = 6, numericInput("max_income", "The Maximum Range of Income you want 
                                                    to compare the two people over", value = 50000, min = 1))
                   )
                 ),
                 
                 # Partnered Checkbox with Income Input
                 tags$div(style = "margin-top: 10px; font-weight: bold;", "Partnered:"),
                 fluidRow(
                   column(width = 6, checkboxInput("partnered", "Yes")),
                   column(width = 6, checkboxInput("partnered2", "Yes"))
                 ),
                 conditionalPanel(
                   condition = "input.partnered == true || input.partnered2 == true",
                   fluidRow(
                     column(width = 6, 
                            conditionalPanel(
                              condition = "input.partnered == true",
                              numericInput("partner_income", "Partner Income:", value = 0, min = 0)
                            )),
                     column(width = 6,
                            conditionalPanel(
                              condition = "input.partnered2 == true",
                              numericInput("partner_income2", "Partner Income:", value = 0, min = 0)
                            ))
                   )
                 ),
                 
                 # Renter Checkbox with Weekly Rent Input
                 tags$div(style = "margin-top: 10px; font-weight: bold;", "Renter:"),
                 fluidRow(
                   column(width = 6, checkboxInput("renter", "Yes")),
                   column(width = 6, checkboxInput("renter2", "Yes"))
                 ),
                 conditionalPanel(
                   condition = "input.renter == true || input.renter2 == true",
                   fluidRow(
                     column(width = 6, 
                            conditionalPanel(
                              condition = "input.renter == true",
                              numericInput("weekly_rent", "Weekly Rent:", value = 0, min = 0)
                            )),
                     column(width = 6, 
                            conditionalPanel(
                              condition = "input.renter2 == true",
                              numericInput("weekly_rent2", "Weekly Rent:", value = 0, min = 0)
                            ))
                   )
                 ),
                 
                 # Number of Dependents Side-by-Side
                 tags$div(style = "margin-top: 10px; font-weight: bold;", "Number of Dependents:"),
                 fluidRow(
                   column(width = 6, numericInput("num_dependents", "Dependents:", value = 0, min = 0, max = 10)),
                   column(width = 6, numericInput("num_dependents2", "Dependents:", value = 0, min = 0, max = 10))
                 ),
                 fluidRow(
                   column(width = 6, uiOutput("child_age_inputs")),
                   column(width = 6, uiOutput("child_age_inputs2"))
                 ),
                 
                 # HECS Debt Checkbox with Debt Amount Input
                 tags$div(style = "margin-top: 10px; font-weight: bold;",NULL),
                 fluidRow(
                   column(width = 6, checkboxInput("hecs_debt", "The Person has HECS Debt")),
                   column(width = 6, checkboxInput("hecs_debt2", "The Person has HECS Debt"))
                 ),
                 conditionalPanel(
                   condition = "input.hecs_debt == true || input.hecs_debt2 == true",
                   fluidRow(
                     column(width = 6, 
                            conditionalPanel(
                              condition = "input.hecs_debt == true",
                              numericInput("total_hecs_debt", "HECS Debt:", value = 0, min = 0)
                            )),
                     column(width = 6, 
                            conditionalPanel(
                              condition = "input.hecs_debt2 == true",
                              numericInput("total_hecs_debt2", "HECS Debt:", value = 0, min = 0)
                            ))
                   )
                 ),
                 
                         )
      ),
      
      actionButton("update_input", "Update")
    ),
    
    # Main Panel in the Center
    mainPanel(
      # Add introductory text above the first set of charts
      tags$h3("Compare how the tax and transfer system affects two working aged Australians"),
      tags$p("This calculator allows you to compare the incomes and Effective Marginal Tax Rates (EMTRs) of two working-aged Australians depending on their level of labour market engagement. The person's earnings will vary based upon their labour market engagement (e.g., hours worked and wages), their home ownership status, their partner's earnings, and the number of dependents they have. This all means that two working-aged Australians can have very different levels of take-home income even if they work the same number of hours and have the same hourly wage."),
      
      # First set of charts
      fluidRow(
        column(width = 6, withSpinner(plotlyOutput("plot", width = "100%", height = "600px"))),
        column(width = 6, withSpinner(plotlyOutput("plot2", width = "100%", height = "600px")))
      ),
      tags$h4("Points of interest"), 
      uiOutput("notch_text"), 
      tags$h3("How does the tax and transfer system affect take-home pay?"),
      tags$p("In the box below you can pick a particular point of labour market engagement in the earning schedule and see 
             how the two people compare. Their total earnings, tax paid, and benefits recieved may vary. Note that you
             will need to enter a valid value between 0 and your maximum amount of labour market engagement."),
      
      tags$div(
        style = "width: 100%;",  # Set the width to full or adjust as needed
        numericInput("compare_value", "", value = 10, min = 0, max = 0, step = 1)
      ), 
      
      uiOutput("comparison_text"), 
      uiOutput("comparison_text2"), 
      # Add the title above the second set of charts
      tags$h3("Maybe you're a stickler for detail?", style = "margin-top: 20px; font-weight: bold;"),
      tags$p("Perhaps you want to see what part of the Australian tax and transfer system is driving the differences in outcomes you see above. The below charts present person 1 (on the left) and person 2 (on the right). You can hover over the different colours to see the part of the tax and transfer system that is contributing to a person's take-home income, or their EMTR."),
      
      # Second set of charts
      fluidRow(
        column(width = 6, withSpinner(plotlyOutput("plot_detailed_hours_1", width = "100%", height = "600px"))),
        column(width = 6, withSpinner(plotlyOutput("plot_detailed_hours_2", width = "100%", height = "600px")))
      ),
      
      # Third set of charts
      fluidRow(
        column(width = 6, withSpinner(plotlyOutput("plot2_detailed", width = "100%", height = "600px"))),
        column(width = 6, withSpinner(plotlyOutput("plot2_detailed2", width = "100%", height = "600px")))
      ),
      
      # Footer text
      div(style = "margin-top: 20px;", 
          p("By Matthew Maltman and Matt Nolan, last updated ", Sys.Date(),
            ". Currently using Q3 2024 Policy Parameters")
      )
    )
  ),
  tags$style(HTML("
  label[for='compare_value'] {
    display: block;
    width: 100%;
    font-weight: bold;
  }
  
  #compare_value {
    width: 100%;  /* Set the input box to full width */
  }
")), 
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
            enforceLimits('compare_value');
            
            enforceLimits('max_hours2');
            enforceLimits('wages2');
            enforceLimits('num_dependents2');
            enforceLimits('weekly_rent2');
            enforceLimits('partner_income2');
            enforceLimits('total_hecs_debt2');
            enforceLimits('max_income2');
            enforceLimits('num_tax_brackets2');
            enforceLimits('job_seeker_single2');
            enforceLimits('job_seeker_couple2');
            enforceLimits('PP_single2');
            enforceLimits('PP_couple2');
        });
    "))
)

