library(shiny)
library(ggplot2)
library(dplyr)
library(patchwork)
library(grid)
library(cowplot)

source("TraitModel.R")

ui <- fluidPage(
  # Add custom CSS for slider colors
  tags$head(
    tags$style(HTML("
      /* Behavior sliders - orange */
      .behavior-slider .irs-bar,
      .behavior-slider .irs-bar-edge,
      .behavior-slider .irs-single,
      .behavior-slider .irs-from,
      .behavior-slider .irs-to {
        background: #df7b39fc !important;
        border-color: #df7b39fc !important;
      }
      
      /* Physiology sliders - gray */
      .physiology-slider .irs-bar,
      .physiology-slider .irs-bar-edge,
      .physiology-slider .irs-single,
      .physiology-slider .irs-from,
      .physiology-slider .irs-to {
        background: #707070cb !important;
        border-color: #707070cb !important;
      }
      
      /* Morphology sliders - blue */
      .morphology-slider .irs-bar,
      .morphology-slider .irs-bar-edge,
      .morphology-slider .irs-single,
      .morphology-slider .irs-from,
      .morphology-slider .irs-to {
        background: #3a578acf !important;
        border-color: #3a578acf !important;
      }
      
      /* Predictability sliders - black */
      .pred-slider .irs-bar,
      .pred-slider .irs-bar-edge,
      .pred-slider .irs-single,
      .pred-slider .irs-from,
      .pred-slider .irs-to {
        background: black !important;
        border-color: black !important;
      }
      
      /* Add styles for title and subtitle */
      .main-title {
        font-size: 24px;
        font-weight: bold;
        text-align: center;
        margin-bottom: 5px;
      }
      .subtitle {
        font-size: 16px;
        text-align: center;
        font-style: italic;
        margin-bottom: 20px;
      }
      
      /* Add spacing between plot and controls */
      .controls-row {
        margin-top: 50px;
      }
    "))
  ),
  
  # Replace empty titlePanel with formatted title and subtitle
  div(
    h2(class = "main-title", "Trait plasticity under increasing magnitudes of global change"),
    p(class = "subtitle", "From Sommer et al. ", HTML("<em>Adaptive phenotypic plasticity under global change</em>"))
  ),
  
  # Remove the sidebarLayout and replace with direct fluidRow structure
  fluidRow(
    column(12,
      plotOutput("traitPlot", height = "500px")
    )
  ),
  
  fluidRow(
    class = "controls-row",  # Add this class for spacing
    # Behavior parameters
    column(3,
      h4("Behavior", style = "color: #df7b39fc;"),
      div(class = "behavior-slider",
        sliderInput("behavior_flexibility", "Flexibility:", 
                  min = 0, max = 1, value = 0.9, step = 0.1),
        sliderInput("behavior_speed", "Speed:", 
                  min = 0, max = 1, value = 0.9, step = 0.1),
        sliderInput("behavior_maintenance", "Maintenance Cost:", 
                  min = 0, max = 1, value = 0.7, step = 0.1),
        sliderInput("behavior_production", "Production Cost:", 
                  min = 0, max = 1, value = 0.1, step = 0.1),
        sliderInput("behavior_threshold", "Trait Critical Threshold:", 
                  min = 0, max = 1, value = 0.8, step = 0.1)
      )
    ),
    
    # Physiology parameters
    column(3,
      h4("Physiology", style = "color: #707070cb;"),
      div(class = "physiology-slider",
        sliderInput("physiology_flexibility", "Flexibility:", 
                  min = 0, max = 1, value = 0.5, step = 0.1),
        sliderInput("physiology_speed", "Speed:", 
                  min = 0, max = 1, value = 0.5, step = 0.1),
        sliderInput("physiology_maintenance", "Maintenance Cost:", 
                  min = 0, max = 1, value = 0.5, step = 0.1),
        sliderInput("physiology_production", "Production Cost:", 
                  min = 0, max = 1, value = 0.5, step = 0.1),
        sliderInput("physiology_threshold", "Trait Critical Threshold:", 
                  min = 0, max = 1, value = 0.8, step = 0.1)
      )
    ),
    
    # Morphology parameters
    column(3,
      h4("Morphology", style = "color: #3a578acf;"),
      div(class = "morphology-slider",
        sliderInput("morphology_flexibility", "Flexibility:", 
                  min = 0, max = 1, value = 0.2, step = 0.1),
        sliderInput("morphology_speed", "Speed:", 
                  min = 0, max = 1, value = 0.1, step = 0.1),
        sliderInput("morphology_maintenance", "Maintenance Cost:", 
                  min = 0, max = 1, value = 0.1, step = 0.1),
        sliderInput("morphology_production", "Production Cost:", 
                  min = 0, max = 1, value = 0.8, step = 0.1),
        sliderInput("morphology_threshold", "Trait Critical Threshold:", 
                  min = 0, max = 1, value = 0.8, step = 0.1)
      )
    ),
    
    # Predictability parameters
    column(3,
      h4("Predictability", style = "color: black;"),
      div(class = "pred-slider",
        wellPanel(
          h5("Mean Change", align = "center"),
          sliderInput("mean_change_pred", "Predictability:",
                    min = 0, max = 1, value = 0.7, step = 0.1)
        ),
        wellPanel(
          h5("Variability", align = "center"),
          sliderInput("variability_pred", "Predictability:",
                    min = 0, max = 1, value = 0.5, step = 0.1)
        ),
        wellPanel(
          h5("Stochasticity", align = "center"),
          sliderInput("stochasticity_pred", "Predictability:",
                    min = 0, max = 1, value = 0, step = 0.1)
        )
      )
    )
  )
)

server <- function(input, output) {
  
  # Modified trait deployment function that uses input values
  trait_deployment_reactive <- function(magnitude, predictability, trait, scenario, input) {
    # Baseline likelihood of deployment, starting at 0 for magnitude 0
    base_likelihood <- exp(-0.5 * magnitude)
    if (magnitude == 0) return(0)
    
    # Get trait-specific parameters from input
    if (as.character(trait) == "Behavior") {
      flexibility <- input$behavior_flexibility
      speed <- input$behavior_speed
      maintenance_cost <- input$behavior_maintenance
      production_cost <- input$behavior_production
    } else if (as.character(trait) == "Physiology") {
      flexibility <- input$physiology_flexibility
      speed <- input$physiology_speed
      maintenance_cost <- input$physiology_maintenance
      production_cost <- input$physiology_production
    } else if (as.character(trait) == "Morphology") {
      flexibility <- input$morphology_flexibility
      speed <- input$morphology_speed
      maintenance_cost <- input$morphology_maintenance
      production_cost <- input$morphology_production
    }
    
    # Amplitude function based on global change dimension
    amplitude <- if(scenario == "Mean Change") {
      1  # Constant amplitude for mean change
    } else if(scenario == "Variability") {
      abs(sin(magnitude * pi))  # Periodic variation
    } else {  # Stochasticity
      set.seed(123)
      pmin(abs(rnorm(1, mean = 0, sd = magnitude)), 1)  # Random variation capped at 1
    }
    
    # Get trait-specific threshold
    max_threshold <- switch(as.character(trait),
      "Behavior" = input$behavior_threshold,
      "Physiology" = input$physiology_threshold,
      "Morphology" = input$morphology_threshold
    )
    
    # Smoother Threshold function considering amplitude
    T_t <- exp(-5 * (amplitude - max_threshold)^2)
    
    # Calculate total costs considering maintenance and production
    total_cost <- maintenance_cost * (1 - predictability) + production_cost * magnitude
    
    # Likelihood function incorporating flexibility, costs, and thresholds
    likelihood <- base_likelihood * flexibility * speed * T_t * (1 - total_cost) * amplitude
    
    # Ensure the likelihood is between 0 and 1
    return(pmax(pmin(likelihood, 1), 0))
  }
  
  # Generate data reactively
  generate_data_reactive <- function(scenario, input) {
    magnitudes <- seq(0, 1, length.out = 100)
    predictability <- switch(scenario,
                           "Mean Change" = input$mean_change_pred,
                           "Variability" = input$variability_pred,
                           "Stochasticity" = input$stochasticity_pred)
    traits <- c("Behavior", "Physiology", "Morphology")
    
    data <- expand.grid(Magnitude = magnitudes, Trait = traits)
    data$Predictability <- predictability
    data$Likelihood <- mapply(trait_deployment_reactive, 
                            data$Magnitude, 
                            data$Predictability, 
                            data$Trait,
                            scenario,
                            MoreArgs = list(input = input))
    data$Scenario <- scenario
    return(data)
  }
  
  # Render the plot
  output$traitPlot <- renderPlot({
    # Combine data for all scenarios
    scenarios <- c("Mean Change", "Variability", "Stochasticity")
    data <- do.call(rbind, lapply(scenarios, function(s) generate_data_reactive(s, input)))
    
    # Create and return the plot using the existing plot_trait_deployment function
    plot_trait_deployment(data)
  })
}

shinyApp(ui = ui, server = server) 