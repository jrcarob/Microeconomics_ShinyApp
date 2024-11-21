library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(reshape2)
library(scales)  # For better color and formatting

server <- function(input, output, session) {
  # Enhanced color palette with more vibrant and distinct colors
  color_palette <- c(
    "Demand" = "#1E90FF",     # Dodger Blue
    "Supply" = "#2ECC71",     # Emerald Green
    "Total_Cost" = "#E74C3C", # Vivid Red
    "Average_Cost" = "#F39C12", # Sunflower Orange
    "Marginal_Cost" = "#9B59B6" # Amethyst Purple
  )
  
  # Improved theme for consistent, professional styling
  enhanced_theme <- function() {
    theme_minimal() + 
      theme(
        # Background and plot area
        plot.background = element_rect(fill = "#F4F6F9", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "#E0E0E0", linetype = "dotted"),
        panel.grid.minor = element_line(color = "#F0F0F0", linetype = "dotted"),
        
        # Title and labels
        plot.title = element_text(
          face = "bold", 
          size = 18, 
          color = "#2C3E50", 
          margin = margin(0, 0, 15, 0)
        ),
        plot.subtitle = element_text(
          size = 12, 
          color = "gray50", 
          margin = margin(0, 0, 10, 0)
        ),
        
        # Axis styling
        axis.title = element_text(
          face = "bold", 
          size = 14, 
          color = "gray40"
        ),
        axis.text = element_text(
          size = 12, 
          color = "gray60"
        ),
        axis.line = element_line(
          color = "gray80", 
          linetype = "solid"
        ),
        
        # Legend improvements
        legend.position = "bottom",
        legend.title = element_text(
          face = "bold", 
          size = 12
        ),
        legend.text = element_text(
          size = 10
        ),
        legend.background = element_rect(
          fill = "#F4F6F9", 
          color = "gray90"
        ),
        legend.key = element_rect(
          fill = "white", 
          color = NA
        )
      )
  }
  
  # Supply and Demand Simulation
  supply_demand_data <- reactive({
    req(input$demand_intercept, input$supply_intercept)
    
    quantity <- seq(0, 100, length.out = 100)
    demand_curve <- input$demand_intercept - quantity
    supply_curve <- input$supply_intercept + quantity
    
    data.frame(
      Quantity = quantity,
      Demand = demand_curve,
      Supply = supply_curve
    )
  })
  
  # Elasticity Calculation
  elasticity_calculation <- reactive({
    req(input$elasticity_type, input$price)
    
    switch(input$elasticity_type,
           "Price Elasticity" = {
             quantity_original <- 100
             price_original <- input$price
             quantity_new <- 80
             price_new <- input$price * 1.1
             
             elasticity <- ((quantity_new - quantity_original) / quantity_original) / 
               ((price_new - price_original) / price_original)
             
             paste("Price Elasticity of Demand:", round(abs(elasticity), 2))
           },
           "Income Elasticity" = {
             income_original <- 50000
             quantity_original <- 100
             income_new <- 55000
             quantity_new <- 110
             
             elasticity <- ((quantity_new - quantity_original) / quantity_original) / 
               ((income_new - income_original) / income_original)
             
             paste("Income Elasticity:", round(elasticity, 2))
           },
           "Cross-Price Elasticity" = {
             price_good_a_original <- input$price
             quantity_good_b_original <- 100
             price_good_a_new <- input$price * 1.2
             quantity_good_b_new <- 90
             
             elasticity <- ((quantity_good_b_new - quantity_good_b_original) / quantity_good_b_original) / 
               ((price_good_a_new - price_good_a_original) / price_good_a_original)
             
             paste("Cross-Price Elasticity:", round(elasticity, 2))
           }
    )
  })
  
  # Production Costs Simulation
  production_costs_data <- reactive({
    req(input$fixed_cost, input$variable_cost)
    
    quantity <- seq(0, 100, by = 1)
    total_cost <- input$fixed_cost + input$variable_cost * quantity
    average_cost <- total_cost / quantity
    marginal_cost <- rep(input$variable_cost, length(quantity))
    
    data.frame(
      Quantity = quantity,
      Total_Cost = total_cost,
      Average_Cost = average_cost,
      Marginal_Cost = marginal_cost
    )
  })
  
  # Consumer Surplus Calculation
  consumer_surplus_data <- reactive({
    req(input$market_price)
    
    max_price <- 100
    quantity <- seq(0, 100, length.out = 100)
    demand_curve <- max_price - (quantity / 2)
    
    surplus <- ifelse(input$market_price < max_price, 
                      0.5 * (max_price - input$market_price) * 
                        (max_price - input$market_price) / max_price * 100, 
                      0)
    
    list(
      surplus = surplus,
      data = data.frame(
        Quantity = quantity,
        Price = demand_curve
      )
    )
  })
  
  # Enhanced Plot Rendering
  output$conceptPlot <- renderPlot({
    req(input$concept)
    
    switch(input$concept,
           "supply_demand" = {
             # Supply and Demand plot
             data <- supply_demand_data()
             ggplot(melt(data, id.vars = "Quantity"), 
                    aes(x = Quantity, y = value, color = variable)) +
               geom_line(size = 1.5) +
               scale_color_manual(values = color_palette) +
               labs(
                 title = "Supply and Demand Curve Dynamics",
                 subtitle = "Exploring Market Equilibrium",
                 x = "Quantity",
                 y = "Price",
                 color = "Curve Type"
               ) +
               enhanced_theme() +
               geom_point(
                 data = data.frame(
                   x = c(50),  # Equilibrium point (approximate)
                   y = c(50),
                   type = c("Equilibrium")
                 ),
                 aes(x = x, y = y), 
                 color = "darkred", 
                 size = 4, 
                 shape = 13
               )
           },
           "elasticity" = {
             # Elasticity plot
             ggplot(data.frame(
               Quantity = c(80, 100, 110),
               Price = c(110, 100, 90)
             ), aes(x = Quantity, y = Price)) +
               geom_line(color = "#E74C3C", size = 1.5) +
               geom_point(color = "#3498DB", size = 4) +
               labs(
                 title = "Elasticity Concept Visualization",
                 subtitle = "Price Responsiveness of Demand",
                 x = "Quantity",
                 y = "Price"
               ) +
               enhanced_theme()
           },
           "prod_costs" = {
             # Production Costs plot
             data <- production_costs_data()
             ggplot(melt(data, id.vars = "Quantity"), 
                    aes(x = Quantity, y = value, color = variable)) +
               geom_line(size = 1.5) +
               scale_color_manual(values = color_palette) +
               labs(
                 title = "Cost Structure Analysis",
                 subtitle = "Total, Average, and Marginal Costs",
                 x = "Production Quantity",
                 y = "Cost",
                 color = "Cost Type"
               ) +
               enhanced_theme() +
               scale_y_continuous(
                 labels = scales::dollar_format(),
                 breaks = pretty_breaks(n = 6)
               )
           },
           "consumer_surplus" = {
             # Consumer Surplus plot
             data <- consumer_surplus_data()
             ggplot(data$data, aes(x = Quantity, y = Price)) +
               geom_line(color = "#3498DB", size = 1.5) +
               geom_hline(
                 yintercept = input$market_price, 
                 color = "#E74C3C", 
                 linetype = "dashed",
                 size = 1.2
               ) +
               annotate(
                 "rect", 
                 xmin = 0, 
                 xmax = 50, 
                 ymin = input$market_price, 
                 ymax = max(data$data$Price),
                 alpha = 0.2, 
                 fill = "#2ECC71"
               ) +
               labs(
                 title = "Consumer Surplus Visualization",
                 subtitle = "Value Above Market Price",
                 x = "Quantity",
                 y = "Price"
               ) +
               enhanced_theme()
           }
    )
  }, height = 600, width = 1000)
  
  # Concept Description
  output$conceptDescription <- renderUI({
    descriptions <- switch(input$concept,
                           "supply_demand" = div(
                             h4("Supply and Demand Dynamics"),
                             p("Explore how market equilibrium is established through the interaction of supply and demand curves.")
                           ),
                           "elasticity" = div(
                             h4("Elasticity Explained"),
                             p("Measure the responsiveness of economic quantities to changes in price, income, or other factors.")
                           ),
                           "prod_costs" = div(
                             h4("Cost Structure Analysis"),
                             tags$ul(
                               tags$li("Total Cost: Sum of fixed and variable costs"),
                               tags$li("Average Cost: Total cost divided by quantity"),
                               tags$li("Marginal Cost: Cost of producing one additional unit")
                             )
                           ),
                           "consumer_surplus" = div(
                             h4("Consumer Surplus"),
                             p("Represents the difference between maximum willingness to pay and actual market price.")
                           )
    )
  })
  
  # Metrics Output
  output$conceptMetrics <- renderUI({
    metrics <- switch(input$concept,
                      "supply_demand" = div(
                        h4("Market Equilibrium Indicators"),
                        p("Intersection point of supply and demand curves represents equilibrium price and quantity.")
                      ),
                      "elasticity" = div(
                        h4("Elasticity Metrics"),
                        p(elasticity_calculation())
                      ),
                      "prod_costs" = div(
                        h4("Cost Metrics"),
                        p("Analyze how costs change with production volume.")
                      ),
                      "consumer_surplus" = div(
                        h4("Surplus Calculation"),
                        p(paste("Calculated Surplus:", 
                                round(consumer_surplus_data()$surplus, 2)))
                      )
    )
  })
}