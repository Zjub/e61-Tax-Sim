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

pacman::p_load(shiny, plotly, shinycssloaders)

############################## Create the UI ################################3###########

fluidPage(
###################  Title 
  titlePanel("e61 Tax and Transfer Calculator"),
  
  tags$h2("Workforce Disincentives for the Working Aged Population (Ages 22-54)", 
          style = "font-weight: normal; font-size: 1.5em; margin-top: -10px;"),

  radioButtons("basis", "Select basis:",
               choices = list("Hours" = "hours", "Income" = "income"),
               selected = "hours"),

############### Side Bar

sidebarLayout(
  sidebarPanel(
    tabsetPanel(
      id = "sidebar_tabs_left",
      # Person Characteristics Panel for the left sidebar
      tabPanel("Person Characteristics",
               # Hours conditional inputs
               conditionalPanel(
                 condition = "input.basis == 'hours'",
                 numericInput("max_hours", "Maximum weekly hours:", value = 40, min = 1, max = 168),
                 numericInput("wages", "Wages (per hour):", value = 25, min = 1)
               ),
               # Income conditional inputs
               conditionalPanel(
                 condition = "input.basis == 'income'",
                 numericInput("max_income", "Maximum annual income:", value = 50000, min = 1)
               ),
               checkboxInput("partnered", "Partnered"),
               conditionalPanel(
                 condition = "input.partnered == true",
                 numericInput("partner_income", "Partner's Income (Annual):", value = 0, min = 0)
               ),
               checkboxInput("renter", "Renter"),
               conditionalPanel(
                 condition = "input.renter == true",
                 numericInput("weekly_rent", "Weekly Rent:", value = 0, min = 0)
               ),
               numericInput("num_dependents", "Number of Dependents:", value = 0, min = 0, max = 10),
               uiOutput("child_age_inputs"),
               checkboxInput("hecs_debt", "Include HECS debt?"),
               conditionalPanel(
                 condition = "input.hecs_debt == true",
                 numericInput("total_hecs_debt", "Total HECS Debt:", value = 0, min = 0)
               ),
               checkboxInput("medicare_levy", "Exclude the Medicare Levy?")
      )
    )
  ),
  
############### Main Panel ************* Results #########################################
    mainPanel(

      ### Load in the two charts as plotly objects. "With Spinner" provides a signal to 
      ## the user that the charts are loading. 
      fluidRow(
               column(width = 6,checkboxInput("Detailed_1", "Show Detailed Results", value = FALSE),
               withSpinner(plotlyOutput("plot", width = "100%", height = "600px"))),
        column(width = 6, checkboxInput("Detailed_2", "Show Detailed Results", value = FALSE),
               withSpinner(plotlyOutput("plot2", width = "100%", height = "600px")))
      ), 
      #### Load in text for interpretation, which is created in the server. 
      textOutput("debug_text"), 
      uiOutput("Title_1"),
      textOutput("interpretation_text"),
      uiOutput("Title_2"),
      textOutput("interpretation_text_2"),
      uiOutput("Title_3"),
      textOutput("interpretation_text_3"),
      uiOutput("Title_4"),
      textOutput("interpretation_text_4"),
      
      ### Accreditation, with the current date. 
      div(style = "margin-top: 20px;",  # Adds some margin at the top
          p("By Matthew Maltman and Matt Nolan, last updated ", Sys.Date(),
            ". Currently using Q3 2024 Policy Parameters")# Add a text output for debugging
      )
    )
  ),
# Duplicate sidebar on the right
sidebarPanel(
  tabsetPanel(
    id = "sidebar_tabs_right",
    # Person Characteristics Panel for the right sidebar (duplicate)
    tabPanel("Person Characteristics",
             # Hours conditional inputs
             conditionalPanel(
               condition = "input.basis == 'hours'",
               numericInput("max_hours_right", "Maximum weekly hours:", value = 40, min = 1, max = 168),
               numericInput("wages_right", "Wages (per hour):", value = 25, min = 1)
             ),
             # Income conditional inputs
             conditionalPanel(
               condition = "input.basis == 'income'",
               numericInput("max_income_right", "Maximum annual income:", value = 50000, min = 1)
             ),
             checkboxInput("partnered_right", "Partnered"),
             conditionalPanel(
               condition = "input.partnered_right == true",
               numericInput("partner_income_right", "Partner's Income (Annual):", value = 0, min = 0)
             ),
             checkboxInput("renter_right", "Renter"),
             conditionalPanel(
               condition = "input.renter_right == true",
               numericInput("weekly_rent_right", "Weekly Rent:", value = 0, min = 0)
             ),
             numericInput("num_dependents_right", "Number of Dependents:", value = 0, min = 0, max = 10),
             uiOutput("child_age_inputs_right"),
             checkboxInput("hecs_debt_right", "Include HECS debt?"),
             conditionalPanel(
               condition = "input.hecs_debt_right == true",
               numericInput("total_hecs_debt_right", "Total HECS Debt:", value = 0, min = 0)
             ),
             checkboxInput("medicare_levy_right", "Exclude the Medicare Levy?")
    )
  )
)
)

# Action button to run calculations
actionButton("update_input", "Update")
)
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

