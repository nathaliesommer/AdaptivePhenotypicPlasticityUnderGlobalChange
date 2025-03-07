# Adaptive Phenotypic Plasticity Under Global Change

Shiny app: [https://nathaliesommer.shinyapps.io/TraitModel/](https://nathaliesommer.shinyapps.io/TraitModel/)

## Authors
- Nathalie R. Sommer (Yale School of the Environment)
- Oswald J. Schmitz (Yale School of the Environment)
- Geoffrey C. Trussell (Northeastern University Marine Science Center)

## Overview
This repository contains the computational model and interactive visualization tools supporting the paper "Adaptive phenotypic plasticity under global change". The model explores how organisms deploy different types of phenotypic plasticity (behavioral, physiological, and morphological) in response to varying dimensions of global change (mean changes, variability, and stochasticity).

## Model Description
The trait deployment model quantifies the likelihood of different plastic responses based on:
- Magnitude of environmental change
- Environmental predictability
- Trait-specific characteristics:
  - Flexibility
  - Response speed
  - Maintenance costs
  - Production costs
  - Critical thresholds

The model incorporates three key dimensions of global change:
1. **Mean Change**: Directional shifts in environmental conditions
2. **Variability**: Periodic fluctuations in environmental conditions
3. **Stochasticity**: Random variations in environmental conditions

## Repository Structure
- `TraitModel.R`: Core implementation of the trait deployment model
- `app.R`: Interactive Shiny application for model exploration
- `TraitModel.Rproj`: R Project file for RStudio integration

## Interactive Application
The Shiny application (`app.R`) provides an interactive interface to explore model dynamics by adjusting:
- Trait-specific parameters (flexibility, speed, costs, thresholds)
- Environmental predictability for each global change dimension
- Visualization of trait deployment likelihood across different scenarios

## Requirements
- R (>= 4.0.0)
- Required R packages:
  - shiny
  - ggplot2
  - dplyr
  - patchwork
  - grid
  - cowplot

## Usage
1. Clone the repository
2. Open the project in RStudio using `TraitModel.Rproj`
3. Install required packages:
```R
install.packages(c("shiny", "ggplot2", "dplyr", "patchwork", "grid", "cowplot"))
```
4. Run the Shiny application:
```R
shiny::runApp()
```
OR, visit the Shinyapp at [https://nathaliesommer.shinyapps.io/TraitModel/](https://nathaliesommer.shinyapps.io/TraitModel/)

## Citation
If you use this model or code in your research, please cite:
```
Nathalie Sommer, Oswald J Schmitz, Geoffrey C. Trussell. Adaptive phenotypic plasticity under global change. Authorea. DOI: 10.22541/au.173391892.21552386/v1
```

## Contact
For questions about the model or implementation, please contact:
- Nathalie R. Sommer (nathalie.sommer@yale.edu)

## Acknowledgments
This work was supported by a collaborative grant (NSF DEB 2011884 and 
NSF DEB 2011857) to OJS and GCT, and a PEO Scholar Award to NRS. 
