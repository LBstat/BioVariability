
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
library(ggridges)
library(ggrepel)
library(qqplotr)
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

# Data analysis
library(mgcv)
library(gamm4)
library(gamlss)

# Parallelization
library(future.apply)

