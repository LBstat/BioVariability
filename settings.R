
# General setting
library(tidyverse)
library(readxl)

# Data manipulation and handling
library(data.table)
library(checkmate)
library(broom.mixed)
library(naniar)
library(purrr)

# Data visualization
library(viridis)
library(ggridges)
library(patchwork)
library(DHARMa)

# Palettes
variables <- list("Season", "RiskIBR", "RiskPT", "RiskAGAL")

palettes <- list(
  "Year" = c("2016" = "#7E57C2", "2017" = "#9575CD", "2018" = "#673AB7", "2019" = "#B39DDB", "2020" = "#512DA8"),
  "Season" = c("Winter" = "#003366", "Spring" = "#9DC183", "Summer" = "#9B1B30", "Autumn" = "#DAA520"),
  "RiskIBR" = c("Low risk" = "#9DC183", "Medium risk" = "#DAA520", "High risk" = "#9B1B30"),
  "RiskPT" = c("Low risk" = "#9DC183", "Medium risk" = "#DAA520", "High risk" = "#9B1B30"),
  "RiskAGAL" = c("Low risk" = "#9DC183", "High risk" = "#9B1B30")
)

# Theme style for plots
my_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 12),
    panel.grid.major = element_line(color = "lightgray", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

# Data analysis
library(mgcv)
library(gamm4)
library(gamlss)

# Parallelization
library(future.apply)

