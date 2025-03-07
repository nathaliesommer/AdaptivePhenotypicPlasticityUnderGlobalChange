# Trait Model for Sommer et al - Oikos

# Libraries
library(ggplot2)
library(dplyr)
library(patchwork)
library(grid)
library(cowplot)

# Define the trait deployment model
trait_deployment <- function(magnitude, predictability, trait, max_thresholds) {
  # Baseline likelihood of deployment, starting at 0 for magnitude 0
  base_likelihood <- exp(-0.5 * magnitude)
  if (magnitude == 0) return(0)
  
  # Get the threshold for this specific trait
  max_threshold <- max_thresholds[[trait]]
  
  # Trait-specific parameters for flexibility, speed, and costs
  if (trait == "Behavior") {
    flexibility <- 0.9  # High flexibility
    speed <- 0.9         # Fastest response
    maintenance_cost <- 0.7
    production_cost <- 0.1
  } else if (trait == "Physiology") {
    flexibility <- 0.5  # Moderate flexibility
    speed <- 0.5        # Intermediate response
    maintenance_cost <- 0.5
    production_cost <- 0.5
  } else if (trait == "Morphology") {
    flexibility <- 0.2  # Low flexibility
    speed <- 0.1        # Slowest response
    maintenance_cost <- 0.1
    production_cost <- 0.8
  }
  
  # Amplitude function based on global change dimension
  amplitude <- 1  # Default for mean change
  if (predictability == 0.5) {  # Variability
    amplitude <- abs(sin(magnitude * pi))
  } else if (predictability == 0) {  # Stochasticity
    set.seed(123)
    amplitude <- abs(rnorm(1, mean = 0, sd = magnitude))
    amplitude <- pmin(amplitude, 1)  # Cap amplitude at 1
  }
  
  # Smooth thresholding function, considering amplitude
  T_t <- exp(-5 * (amplitude - max_threshold)^2)
  
  # Total cost calculation
  total_cost <- maintenance_cost * (1 - predictability) + production_cost * magnitude
  
  # Likelihood function
  likelihood <- base_likelihood * flexibility * speed * T_t * (1 - total_cost) * amplitude
  
  # Likelihood is between 0 and 1
  return(pmax(pmin(likelihood, 1), 0))
}

# Data for plotting
generate_data <- function(scenario, max_thresholds = list(
    "Behavior" = 0.8,
    "Physiology" = 0.8,
    "Morphology" = 0.8
  )) {
  magnitudes <- seq(0, 1, length.out = 100)
  predictability <- ifelse(scenario == "Mean Change", 1,
                           ifelse(scenario == "Variability", 0.5, 0))
  traits <- c("Behavior", "Physiology", "Morphology")
  
  data <- expand.grid(Magnitude = magnitudes, Trait = traits)
  data$Predictability <- predictability
  data$Likelihood <- mapply(trait_deployment, 
                           data$Magnitude, 
                           data$Predictability, 
                           data$Trait,
                           MoreArgs = list(max_thresholds = max_thresholds))
  data$Scenario <- scenario
  return(data)
}

# Combine scenarios
scenarios <- c("Mean Change", "Variability", "Stochasticity")
default_thresholds <- list(
  "Behavior" = 0.8,
  "Physiology" = 0.8,
  "Morphology" = 0.8
)
data <- do.call(rbind, lapply(scenarios, function(s) generate_data(s, max_thresholds = default_thresholds)))

# Data for inset
generate_inset_data <- function(scenario) {
  x <- seq(0, 1, length.out = 100)
  if(scenario == "Mean Change") {
    y <- exp(2 * x) - 1 
    y <- (y - min(y)) / (max(y) - min(y))
    data.frame(x = x, y = y)
  } else if(scenario == "Variability") {
    frequency <- 20
    amplitude <- x * 0.3
    y <- amplitude * sin(frequency * x * pi)
    data.frame(x = x, y = y)
  } else {
    set.seed(123)
    amplitude <- x * 0.3
    y <- amplitude * rnorm(length(x))
    data.frame(x = x, y = y)
  }
}

# Create individual inset plot
create_inset <- function(scenario) {
  inset_data <- generate_inset_data(scenario)
  ggplot(inset_data, aes(x = x, y = y)) +
    geom_line(color = "black", linewidth = 0.5) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = unit(c(1,1,1,1), "mm")
    ) +
    coord_cartesian(xlim = c(0, 1), 
                    ylim = c(-0.5, 0.5))
}

# Main plot
plot_trait_deployment <- function(data, max_thresholds = list(
    "Behavior" = 0.8,
    "Physiology" = 0.8,
    "Morphology" = 0.8
  )) {
  data$Scenario <- factor(data$Scenario, 
                          levels = c("Mean Change", "Variability", "Stochasticity"))
  main_plot <- ggplot(data, aes(x = Magnitude, y = Likelihood, color = Trait)) +
    geom_line(linewidth = 1.2) +
    facet_wrap(~Scenario, nrow = 1, scales = "fixed") +
    theme_minimal() +
    scale_color_manual(values = c("Behavior" = "#df7b39fc",
                                  "Physiology" = "#707070cb",
                                  "Morphology" = "#3a578acf")) +
    scale_x_continuous(breaks = seq(0, 1, 0.2), labels = sprintf("%.1f", seq(0, 1, 0.2))) +
    labs(x = "Magnitude of Global Change Dimension",
         y = "Likelihood of Trait Deployment") +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(margin = margin(t = 15, b = 0)),
      strip.text = element_text(size = 14)
    )
  
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
}

# Plotting the updated model
plot_trait_deployment(data)


