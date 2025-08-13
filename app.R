library(shiny)
library(ggplot2)
library(dplyr)
library(patchwork)
library(grid)
library(cowplot)
library(paletteer)

source("TraitModel.R")

# Define color palette using fishualize::Antennarius_commerson
trait_colors <- c(
  "Trait 1" = "#26677F",   # Blue
  "Trait 2" = "#635C72",   # Purple-gray
  "Trait 3" = "#89374F",   # Burgundy
  "Trait 4" = "#C46F3E",   # Orange
  "Trait 5" = "#B79E4B"    # Gold
)

ui <- fluidPage(
  # Add custom CSS for slider colors
  tags$head(
    tags$style(HTML("
      /* Trait 1 sliders */
      .trait-1-slider .irs-bar,
      .trait-1-slider .irs-bar-edge,
      .trait-1-slider .irs-single,
      .trait-1-slider .irs-from,
      .trait-1-slider .irs-to {
        background: #26677F !important;
        border-color: #26677F !important;
      }
      
      /* Trait 2 sliders */
      .trait-2-slider .irs-bar,
      .trait-2-slider .irs-bar-edge,
      .trait-2-slider .irs-single,
      .trait-2-slider .irs-from,
      .trait-2-slider .irs-to {
        background: #635C72 !important;
        border-color: #635C72 !important;
      }
      
      /* Trait 3 sliders */
      .trait-3-slider .irs-bar,
      .trait-3-slider .irs-bar-edge,
      .trait-3-slider .irs-single,
      .trait-3-slider .irs-from,
      .trait-3-slider .irs-to {
        background: #89374F !important;
        border-color: #89374F !important;
      }
      
      /* Trait 4 sliders */
      .trait-4-slider .irs-bar,
      .trait-4-slider .irs-bar-edge,
      .trait-4-slider .irs-single,
      .trait-4-slider .irs-from,
      .trait-4-slider .irs-to {
        background: #C46F3E !important;
        border-color: #C46F3E !important;
      }
      
      /* Trait 5 sliders */
      .trait-5-slider .irs-bar,
      .trait-5-slider .irs-bar-edge,
      .trait-5-slider .irs-single,
      .trait-5-slider .irs-from,
      .trait-5-slider .irs-to {
        background: #B79E4B !important;
        border-color: #B79E4B !important;
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
      
      /* Style for the Add/Remove trait buttons */
      .trait-buttons {
        text-align: center;
        margin: 20px 0;
      }
      
      #add_trait, #remove_trait {
        margin: 0 10px;
        padding: 8px 16px;
        border-radius: 4px;
        border: none;
        background-color: #f0f0f0;
        cursor: pointer;
      }
      
      #add_trait:hover, #remove_trait:hover {
        background-color: #e0e0e0;
      }
      
      /* Horizontal layout for trait controls */
      .trait-controls {
        display: flex;
        flex-direction: row;
        flex-wrap: wrap;
        gap: 20px;
        margin-top: 20px;
      }
      
      .trait-panel {
        flex: 0 0 250px;
        min-width: 250px;
      }
      
      /* All traits container outer wrapper */
      .trait-panels-outer {
        overflow-x: auto;
        padding: 0 40px 20px 40px;
        width: 100%;
        box-sizing: border-box;
        background: white;
      }
      .all-traits {
        display: flex;
        flex-direction: row;
        flex-wrap: nowrap;
        gap: 20px;
        min-width: 1240px; /* 5 panels x 240px + gap */
      }
      .trait-panel {
        flex: 0 0 240px;
        min-width: 240px;
        margin: 0 5px;
      }
      .trait-matrix th {
        text-align: center !important;
      }
      /* Hide minor ticks and grid labels on sliders */
      .irs-grid-pol, .irs-grid-text { display: none !important; }
    "))
  ),
  
  # Title and subtitle
  div(
    h2(class = "main-title", "Trait plasticity under increasing magnitudes of global change"),
    p(class = "subtitle", "From Sommer et al. (2025)", HTML("<em>A trait-based framework for adaptive phenotypic plasticity under global change</em>"))
  ),
  
  # Main plot
  fluidRow(
    column(12,
      plotOutput("traitPlot", height = "500px")
    )
  ),
  
  # Predictability controls (horizontal)
  fluidRow(
    class = "controls-row",
    column(12,
      h4("Predictability", style = "color: black;"),
      div(class = "trait-controls",
        div(class = "trait-panel",
          h5("Mean Change", align = "center"),
          div(class = "pred-slider",
          sliderInput("mean_change_pred", "Predictability:",
                    min = 0, max = 1, value = 0.7, step = 0.1)
          )
        ),
        div(class = "trait-panel",
          h5("Variability", align = "center"),
          div(class = "pred-slider",
          sliderInput("variability_pred", "Predictability:",
                    min = 0, max = 1, value = 0.5, step = 0.1)
          )
        ),
        div(class = "trait-panel",
          h5("Stochasticity", align = "center"),
          div(class = "pred-slider",
          sliderInput("stochasticity_pred", "Predictability:",
                    min = 0, max = 1, value = 0, step = 0.1)
        )
      )
    )
  )
  ),
  
  # Add/Remove trait buttons (centered)
  fluidRow(
    column(12,
      div(class = "trait-buttons",
        actionButton("add_trait", "Add Trait"),
        actionButton("remove_trait", "Remove Last Trait")
      )
    )
  ),
  
  # All trait properties in a matrix layout
  fluidRow(
    div(
      style = "overflow-x: auto; margin-top: 20px;",
      uiOutput("trait_matrix")
    )
  )
)

server <- function(input, output, session) {
  # Track number of additional traits (max 4)
  trait_count <- reactiveVal(0)
  
  # Handle adding new trait
  observeEvent(input$add_trait, {
    if (trait_count() < 4) {  # Limit to 4 additional traits
      trait_count(trait_count() + 1)
    }
  })
  
  # Handle removing trait
  observeEvent(input$remove_trait, {
    if (trait_count() > 0) {
      trait_count(trait_count() - 1)
    }
  })
  
  # Generate UI for additional traits
  output$trait_matrix <- renderUI({
    # List of property names and pretty labels
    properties <- list(
      flexibility = "Flexibility",
      speed = "Speed",
      maintenance = "Maintenance Cost",
      production = "Production Cost",
      threshold = "Trait Critical Threshold"
    )
    
    n_traits <- 1 + trait_count()
    
    # Table header
    header_row <- tags$tr(
      tags$th(""),
      lapply(properties, function(label) tags$th(label))
    )
    
    # Table rows for each trait
    trait_rows <- lapply(1:n_traits, function(i) {
      trait_label <- paste("Trait", i)
      color <- trait_colors[[trait_label]]
      tags$tr(
        tags$td(tags$span(trait_label, style = paste0("font-weight:bold;color:", color, ";"))),
        tags$td(div(class = paste0("trait-", i, "-slider"), sliderInput(paste0("flexibility_", i), NULL, min = 0, max = 1, value = ifelse(i == 1, 0.5, 0.3), step = 0.1, width = "150px", ticks = FALSE))),
        tags$td(div(class = paste0("trait-", i, "-slider"), sliderInput(paste0("speed_", i), NULL, min = 0, max = 1, value = ifelse(i == 1, 0.5, 0.3), step = 0.1, width = "150px", ticks = FALSE))),
        tags$td(div(class = paste0("trait-", i, "-slider"), sliderInput(paste0("maintenance_", i), NULL, min = 0, max = 1, value = ifelse(i == 1, 0.5, 0.3), step = 0.1, width = "150px", ticks = FALSE))),
        tags$td(div(class = paste0("trait-", i, "-slider"), sliderInput(paste0("production_", i), NULL, min = 0, max = 1, value = ifelse(i == 1, 0.5, 0.3), step = 0.1, width = "150px", ticks = FALSE))),
        tags$td(div(class = paste0("trait-", i, "-slider"), sliderInput(paste0("threshold_", i), NULL, min = 0, max = 1, value = ifelse(i == 1, 0.8, 0.6), step = 0.1, width = "150px", ticks = FALSE)))
      )
    })
    
    tags$table(
      class = "trait-matrix",
      style = "width:auto; border-collapse:separate; border-spacing: 10px;",
      header_row,
      trait_rows
    )
  })
  
  # Modified trait deployment function
  trait_deployment_reactive <- function(magnitude, predictability, trait_params, scenario) {
    # Baseline likelihood of deployment
    base_likelihood <- exp(-0.5 * magnitude)
    if (magnitude == 0) return(0)
    
    # Get trait parameters
    flexibility <- trait_params$flexibility
    speed <- trait_params$speed
    maintenance_cost <- trait_params$maintenance
    production_cost <- trait_params$production
    max_threshold <- trait_params$threshold
    
    # Amplitude function based on global change dimension
    amplitude <- if(scenario == "Mean Change") {
      1
    } else if(scenario == "Variability") {
      abs(sin(magnitude * pi))
    } else {
      set.seed(123)
      pmin(abs(rnorm(1, mean = 0, sd = magnitude)), 1)
    }
    
    # Calculate likelihood
    T_t <- exp(-5 * (amplitude - max_threshold)^2)
    total_cost <- maintenance_cost * (1 - predictability) + production_cost * magnitude
    likelihood <- base_likelihood * flexibility * speed * T_t * (1 - total_cost) * amplitude
    
    return(pmax(pmin(likelihood, 1), 0))
  }
  
  # Generate data for plotting
  generate_data_reactive <- function(scenario, input) {
    magnitudes <- seq(0, 1, length.out = 100)
    predictability <- switch(scenario,
                           "Mean Change" = input$mean_change_pred,
                           "Variability" = input$variability_pred,
                           "Stochasticity" = input$stochasticity_pred)
    
    n_traits <- 1 + trait_count()
    trait_params <- vector("list", n_traits)
    # Check that all required inputs exist
    for (i in 1:n_traits) {
      for (prop in c("flexibility_", "speed_", "maintenance_", "production_", "threshold_")) {
        if (is.null(input[[paste0(prop, i)]])) return(NULL)
      }
    }
    for (i in 1:n_traits) {
      trait_params[[i]] <- list(
        flexibility = input[[paste0("flexibility_", i)]],
        speed = input[[paste0("speed_", i)]],
        maintenance = input[[paste0("maintenance_", i)]],
        production = input[[paste0("production_", i)]],
        threshold = input[[paste0("threshold_", i)]]
      )
    }
    
    # Generate data for each trait
    data <- do.call(rbind, lapply(seq_along(trait_params), function(i) {
      trait_data <- expand.grid(
        Magnitude = magnitudes,
        Trait = paste("Trait", i)
      )
      trait_data$Predictability <- predictability
      trait_data$Likelihood <- sapply(magnitudes, function(m) {
        trait_deployment_reactive(m, predictability, trait_params[[i]], scenario)
      })
      trait_data$Scenario <- scenario
      return(trait_data)
    }))
    
    return(data)
  }
  
  # Render the plot
  output$traitPlot <- renderPlot({
    scenarios <- c("Mean Change", "Variability", "Stochasticity")
    data <- do.call(rbind, lapply(scenarios, function(s) generate_data_reactive(s, input)))
    
    if (is.null(data)) return(NULL)
    # Create plot with dynamic number of traits
    data$Scenario <- factor(data$Scenario, 
                          levels = c("Mean Change", "Variability", "Stochasticity"))
    data$Trait <- factor(data$Trait, levels = names(trait_colors))
    
    main_plot <- ggplot(data, aes(x = Magnitude, y = Likelihood, color = Trait)) +
      geom_line(linewidth = 1.2) +
      facet_wrap(~Scenario, nrow = 1, scales = "fixed") +
      theme_minimal() +
      scale_color_manual(values = trait_colors) +
      scale_x_continuous(breaks = seq(0, 1, 0.2), labels = sprintf("%.1f", seq(0, 1, 0.2))) +
      labs(x = "Magnitude of Global Change Dimension",
           y = "Likelihood of Trait Deployment") +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 15, b = 0)),
        strip.text = element_text(size = 14)
      )
    
    # Add insets
    insets <- list(
      "Mean Change" = create_inset("Mean Change"),
      "Variability" = create_inset("Variability"),
      "Stochasticity" = create_inset("Stochasticity")
    )
    
    p <- ggdraw(main_plot)
    for(scenario in names(insets)) {
      p <- p + draw_plot(
        insets[[scenario]], 
        x = ifelse(scenario == "Mean Change", 0.25,
                   ifelse(scenario == "Variability", 0.55, 0.85)),
        y = 0.75,
        width = 0.125,
        height = 0.125
      )
    }
    return(p)
  })
}

shinyApp(ui = ui, server = server) 