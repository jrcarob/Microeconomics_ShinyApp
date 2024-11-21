#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#

library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)

ui <- dashboardPage(
  dashboardHeader(
    title = span(
      img(src = "https://cdn-icons-png.flaticon.com/512/2286/2286020.png", 
          height = "30px", style = "margin-right: 10px;"),
      "Microeconomics Simulator"
    ),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    disable = TRUE  # Disable the sidebar
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .box { 
          box-shadow: 0 4px 6px rgba(0,0,0,0.1); 
          border-radius: 10px; 
        }
        .content-wrapper {
          background-color: #f4f6f9;
        }
        .nav-tabs-custom .nav-tabs {
          margin-bottom: 15px;
        }
      "))
    ),
    
    # Tabbed interface for economic concepts
    tabBox(
      width = 12,
      title = "Economic Concepts",
      id = "concept",
      
      # Supply and Demand Tab
      tabPanel(
        title = "Supply and Demand",
        value = "supply_demand",
        fluidRow(
          column(
            width = 6,
            sliderInput(
              inputId = "demand_intercept",
              label = "Demand Intercept", 
              min = 10, 
              max = 100, 
              value = 50,
              step = 5
            )
          ),
          column(
            width = 6,
            sliderInput(
              inputId = "supply_intercept", 
              label = "Supply Intercept",
              min = 10, 
              max = 100, 
              value = 20,
              step = 5
            )
          )
        )
      ),
      
      # Elasticity Tab
      tabPanel(
        title = "Elasticity",
        value = "elasticity",
        fluidRow(
          column(
            width = 6,
            pickerInput(
              inputId = "elasticity_type",
              label = "Elasticity Type", 
              choices = c(
                "Price Elasticity", 
                "Income Elasticity", 
                "Cross-Price Elasticity"
              ),
              selected = "Price Elasticity"
            )
          ),
          column(
            width = 6,
            numericInputIcon(
              inputId = "price",
              label = "Price", 
              value = 10,
              icon = icon("dollar-sign")
            )
          )
        )
      ),
      
      # Production Costs Tab
      tabPanel(
        title = "Production Costs",
        value = "prod_costs",
        fluidRow(
          column(
            width = 6,
            sliderInput(
              inputId = "fixed_cost",
              label = "Fixed Cost", 
              min = 0, 
              max = 1000, 
              value = 500,
              step = 50
            )
          ),
          column(
            width = 6,
            sliderInput(
              inputId = "variable_cost", 
              label = "Variable Cost per Unit",
              min = 1, 
              max = 50, 
              value = 10,
              step = 1
            )
          )
        )
      ),
      
      # Consumer Surplus Tab
      tabPanel(
        title = "Consumer Surplus",
        value = "consumer_surplus",
        fluidRow(
          column(
            width = 6,
            sliderInput(
              inputId = "market_price", 
              label = "Market Price",
              min = 1, 
              max = 100, 
              value = 50,
              step = 5
            )
          )
        )
      )
    ),
    
    # Visualization and Metrics Rows
    fluidRow(
      box(
        width = 12,
        title = "Economic Concept Visualization",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("conceptPlot", height = "500px")
      )
    ),
    
    fluidRow(
      box(
        width = 6,
        title = "Concept Description",
        status = "info",
        uiOutput("conceptDescription")
      ),
      
      box(
        width = 6,
        title = "Key Metrics",
        status = "success",
        uiOutput("conceptMetrics")
      )
    )
  )
)