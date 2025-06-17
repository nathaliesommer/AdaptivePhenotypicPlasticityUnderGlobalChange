# Trait Model for Sommer et al - Oikos

# Libraries
library(ggplot2)
library(dplyr)
library(patchwork)
library(grid)
library(cowplot)

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


