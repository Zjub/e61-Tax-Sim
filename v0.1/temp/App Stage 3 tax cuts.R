## Pulls in functions and policies to generate income and tax measures ----
# Last update: 24/01/2024
# Updated by: Matt Nolan
# Initial author: Matt Nolan

if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  data.table,
  theme61,
  shiny)

library(theme61) # Having some specific issues pulling this package in.

ui <- fluidPage(
  # Custom edits
  #tags$head()

  # Side panel title
  titlePanel("Stage 3 Tax cuts (for a 25-54 year old)"),

  sidebarLayout(
    sidebarPanel(
      # Style details
      style = "background-color: #ADD8E6;",

      # Button to update the graph and paragraph
      actionButton("update", "Update"),

      # Input widgets
      checkboxInput("input2", "Does this person have a partner? (Tick box for yes)", value = FALSE),
      conditionalPanel(
        condition = "input.input2",
        numericInput("input3", "How much does their partner earn annually ($, excluding compulsory superannuation contributions)", value = 0)
      ),
      checkboxInput("input6", "Do they have dependent children (Tick box for yes)", value = FALSE),
      conditionalPanel(
        condition = "input.input6",
        checkboxInput("input6b", "Are they the main carer of these children", value = TRUE)
      ),
      conditionalPanel(
        condition = "input.input6",
        textInput("input7", "Enter the ages of the children (separated by a comma)", value="11,4,9")
      ),
      checkboxInput("input5", "Is this person living alone (Tick box for yes)", value = FALSE),
      numericInput("input8", "How much does this person pay in rent per fortnight", value = 500),
      textInput("input11", "Change the current tax rates (separated by a comma)", value="0.19,0.325,0.37,0.45"),
      textInput("input12", "Change the current tax thresholds (separated by a comma)", value="18200,45000,120000,180000"),
      numericInput("input13", "What is the highest annual income you want in the plot (000s)?", value = 300),


      # Button to update the graph and paragraph
      actionButton("update2", "Update")
    ),

    mainPanel(
      # Style settings

      # Output elements
      tags$div(style = "text-align: center; font-style: italic;", "Alpha Version 0.1. Stage 3 tax cuts for a renter aged 25-54, with no significant assets and full benefit eligibility (inc FTBs) - medicare excluded"),
      tags$br(),
      tags$div(style = "text-align: left;","In the graphs below we will look at your suggested tax scale. Click update on the left of this screen to construct the graphs."),
      tags$br(),
      #plotOutput("plot_income"),
      plotOutput("plot2"),
      plotOutput("plot3"),
      plotOutput("plot4"),

    )
  )
)


#Define the server
server <- function(input, output, session) {

  observeEvent(c(input$update, input$update2), {


    if(!require("pacman")) install.packages("pacman")

    pacman::p_load(
      tidyverse,
      data.table,
      theme61)

    library(theme61) # Having some specific issues pulling this package in.
    # Include functions ----

    partnered <<- as.numeric(input$input2)
    living_alone <<- as.numeric(input$input5)
    Have_dep <<- as.numeric(input$input6)



    partner_earnings <<- ifelse(input$input2 == FALSE,0,input$input3)

    Main_carer_dep <<- ifelse(input$input6 == FALSE,FALSE,as.numeric(input$input6b))

    Rent <<- ifelse(input$input8 == "",0,input$input8)

    Max_inc <<- ifelse(input$input13 == "",300,input$input13)

    over_60 <<- 0

    ### Test
    # partnered <- 0
    # living_alone <- 0
    # Have_dep <- 0
    #
    # numeric_values <- unlist(strsplit("11,14,17", split = ",")[[1]])
    # child_age <- as.numeric(numeric_values)
    #
    # partner_earnings <- 0
    #
    # Main_carer_dep <- 1
    #
    # Rent <- 500
    # Max_inc <- 100
    ###

    if (Main_carer_dep == 0 | Have_dep == 0){
      child_age <<- 99
    }
    else{
      numeric_values <- unlist(strsplit(input$input7, split = ",")[[1]])
      child_age <<- as.numeric(numeric_values)
    }

    Numb_dep <<- length(child_age[child_age < 20])
    young_child <<- min(child_age)

    tax_free_S3 <- 18200
    tax_threshold_1_S3 <- 45000
    tax_threshold_2_S3 <- 120000
    tax_threshold_3_S3 <- 200000

    tax_rate_1_S3 <- 0.19
    tax_rate_2_S3 <- 0.30
    tax_rate_3_S3 <- 0.30
    tax_rate_4_S3 <- 0.45

    tax_brackets_S3 <- c(0, tax_free_S3, tax_threshold_1_S3, tax_threshold_2_S3, tax_threshold_3_S3)
    tax_scale_S3 <- c(0, tax_rate_1_S3, tax_rate_2_S3, tax_rate_3_S3, tax_rate_4_S3)

    numeric_values <- unlist(strsplit(input$input12, split = ",")[[1]])
    tax_brackets_new <- c(0,as.numeric(numeric_values))

    numeric_values <- unlist(strsplit(input$input11, split = ",")[[1]])
    tax_scale_new <- c(0,as.numeric(numeric_values))


    ### Test
    # numeric_values <- unlist(strsplit("18200,45000,120000,180000", split = ",")[[1]])
    # tax_brackets_new <- c(0,as.numeric(numeric_values)) + 0.01
    # numeric_values <- unlist(strsplit("0.19,0.325,0.37,0.45", split = ",")[[1]])
    # tax_scale_new <- c(0,as.numeric(numeric_values)) + 0.01
    ###

    scripts <- list.files(pattern = "calc_")
    for (script in scripts){
      source(script)
    }

    # Import parameters for the given quarter ----

    policy_date1 <- "Q4_2023"

    # policy_parameters(policy_date1) # This is not importing and assigning. We can assigned the object it returns manually and create the parameters in this file.

    policy_parameters_1 <- policy_parameters(policy_date1)

    # Assign policy object
    # for (i in 1:nrow(policy_parameters_1)){
    #   assign(policy_parameters_1$Parameter[i],policy_parameters_1$Value[i])
    # }
    # Assign policy object
    # Assign policy object
    for (i in 1:nrow(policy_parameters_1)) {
      eval(parse(text = paste0(policy_parameters_1$Parameter[i], " <<- ", policy_parameters_1$Value[i])))
    }


    # Income tax

    tax_brackets <- c(0, tax_free, tax_threshold_1, tax_threshold_2, tax_threshold_3)
    tax_scale <- c(0, tax_rate_1, tax_rate_2, tax_rate_3, tax_rate_4)

    # Benefit elements

    JSP_S_ND_arate <- c(0,JSP_S_ND_arate_1,JSP_S_ND_arate_2)
    JSP_S_ND_athresh <- c(0,JSP_S_ND_athresh_1,JSP_S_ND_athresh_2)


#TAXBEN_calc_workinc <- function(partnered = 0, partner_earnings = 0, over_60 = 0, living_alone = 0, Have_dep = 0, Main_carer_dep = 0, child_age = 99,Rent=200,cite="tvhe.co.nz"){
#remotes::install_github("e61-institute/theme61", dependencies = TRUE, upgrade = "always")

## Setup the individuals characteristics based on user inputs ----
# Define family characteristics

  PPeligible <<- ifelse((young_child <= 6 | (young_child <= 8 & partnered == 0)) & Main_carer_dep == 1,1,0)
  PP_S_athresh <<- PP_S_athresh_base + PP_S_athresh_mult*(Numb_dep - 1)

  # Individual characteristics turned off
  Carer <<- 0
  Disability <<- 0

  # Housing status
  Home_owner <<- 0
  Rent <<- Rent # Fortnightly rent amount

  # Meets conditions to access benefit on a personal level
  ben_eligibility <<- 1

  # Citation
  cite <- "tvhe.substack.com"


  ## Set up income plot and text ----


  net_income_calculator <- function(tax_brackets,tax_scale) {
    net_income_profile <- numeric((Max_inc + 1))
    for (i in 0:Max_inc) {
      net_income_profile[i + 1] <- calc_net_income(work_income = i*1000,tax_brackets,tax_scale)[["net_income"]]
    }
    return(net_income_profile)
  }

  net_incomes_data <- net_income_calculator(tax_brackets,tax_scale)

  incomesdf <- data.frame(Earnings = seq(0,Max_inc*1000,by=1000)) %>% mutate("Take home income" = net_incomes_data) %>% pivot_longer(!Earnings,names_to = "variable",values_to = "value")

  plot_income <- ggplot(incomesdf,aes(x=Earnings,y=value/1000,colour=variable)) + geom_line() + theme_e61(legend="bottom") + labs_e61(
    title = paste0("Net Income"),
    subtitle = "Annual income",
    y = "(000s)",
    x = "Gross Earnings",
    sources = c(cite)
  ) + scale_y_continuous(labels = scales::dollar,limits=c(0,max(incomesdf$value/1000)*1.2))

## Add new tax rates and compare incomes ----
  # Note: This writes over the prior brackets - for a future version would prefer to have the functions read in a file for brackets and scales, which allows us to pass different ones through the function.


  net_incomes_new_data <- net_income_calculator(tax_brackets = tax_brackets_S3,tax_scale = tax_scale_S3)

  incomesdf_new <- data.frame(Earnings = seq(0,Max_inc*1000,by=1000)) %>% mutate("Take home income (Stage 3)" = net_incomes_new_data) %>% pivot_longer(!Earnings,names_to = "variable",values_to = "value")

  tax_brackets_new <- tax_brackets_new
  tax_scale_new <- tax_scale_new

  net_incomes_new2_data <- net_income_calculator(tax_brackets = tax_brackets_new,tax_scale = tax_scale_new)

  incomesdf_new2 <- data.frame(Earnings = seq(0,Max_inc*1000,by=1000)) %>% mutate("Take home income (your version)" = net_incomes_new2_data) %>% pivot_longer(!Earnings,names_to = "variable",values_to = "value")



  comparison_df <- data.frame(Earnings = seq(0,Max_inc*1000,by=1000))  %>% mutate("Take home income (your version)" = net_incomes_new2_data) %>% mutate("Take home income (Stage 3)" = net_incomes_new_data) %>% mutate("Take home income (current)" = net_incomes_data) %>% pivot_longer(!Earnings,names_to = "variable",values_to = "value")

  comparison_graph <- ggplot(comparison_df,aes(x=Earnings,y=value,colour=variable)) + geom_line() + theme_e61(legend="bottom") + labs_e61(
    title = paste0("Net Income comparison"),
    subtitle = "Annual income",
    y = "",
    x = "Gross Earnings",
    sources = c(cite)
  ) + scale_y_continuous(labels = scales::dollar,limits=c(0,max(comparison_df$value)*1.2)) + scale_x_continuous_e61(labels = scales::dollar)

  plot2 <- comparison_graph

  comparison_diff_df <- comparison_df %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    mutate(Diff_Yours_vs_Current = `Take home income (your version)` - `Take home income (current)`,
           Diff_Stage3_vs_Current = `Take home income (Stage 3)` - `Take home income (current)`) %>%
    select(Earnings, Diff_Yours_vs_Current, Diff_Stage3_vs_Current) %>%
    pivot_longer(-Earnings, names_to = "Difference", values_to = "Value")


  difference_graph <- ggplot(comparison_diff_df, aes(x = Earnings, y = Value, colour = Difference)) +
    geom_line() + geom_hline(yintercept = 0) +
    theme_e61(legend = "bottom") +
    labs_e61(
      title = "Difference in Net Income",
      subtitle = "Difference from the current tax system",
      #y = "Difference in Net Income",
      y = "",
      x = "Gross Earnings",
      sources = c(cite)
    ) +
    scale_y_continuous(labels = scales::dollar) + scale_x_continuous_e61(labels = scales::dollar)

  plot3 <- difference_graph

  comparison_pct_df <- comparison_df %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    mutate(
      Pct_Diff_Yours_vs_Current = (`Take home income (your version)` - `Take home income (current)`) / Earnings,
      Pct_Diff_Stage3_vs_Current = (`Take home income (Stage 3)` - `Take home income (current)`) / Earnings
    ) %>%
    select(Earnings, Pct_Diff_Yours_vs_Current, Pct_Diff_Stage3_vs_Current) %>%
    pivot_longer(-Earnings, names_to = "Percentage Difference", values_to = "Value")

  percentage_difference_graph <- ggplot(comparison_pct_df, aes(x = Earnings, y = Value, colour = `Percentage Difference`)) +
    geom_line() +
    geom_hline(yintercept = 0) +
    theme_e61(legend = "bottom") +
    labs_e61(
      title = "Percentage Difference in Net Income",
      subtitle = "Percentage change in net income relative to current tax system",
      #y = "Percentage Difference",
      y = "",
      x = "Gross Earnings",
      sources = c(cite)
    ) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous_e61(labels = scales::dollar)

  plot4 <- percentage_difference_graph

  plot_income
  plot2
  plot3
  plot4


  #return(list(plot = plot_income, plot2 = comparison_graph))
#}




  #result <- TAXBEN_calc_workinc(partnered = as.numeric(input$input2), partner_earnings = partner_earnings, living_alone = as.numeric(input$input5), Have_dep = as.numeric(input$input6), Main_carer_dep = Main_carer_dep, child_age = child_age, Rent=Rent)

  #output$plot <- renderPlot({ result$plot })
  #output$plot2 <- renderPlot({ result$plot2 })
  #output$plot_income <- renderPlot({ plot_income })
  output$plot2 <- renderPlot({ plot2 })
  output$plot3 <- renderPlot({ plot3 })
  output$plot4 <- renderPlot({ plot4 })

  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
