# global.R

# Load all necessary libraries
library(shiny)
library(bslib)
library(thematic)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rhandsontable)
library(car)
library(stats)
library(psych)
library(scales)
library(readxl)
library(shinyWidgets)
library(bsicons)
library(rsconnect)
library(shinyjs)
library(shinycssloaders)
library(shinya11y)
library(plotly)
# --- bslib Theme Definitions ---
light_theme <- bs_theme(version = 5)
dark_theme <- bs_theme(version = 5, bootswatch = "darkly")


# --- Create a custom sample dataset ---
set.seed(123) # for reproducible scores
exam_scores <- data.frame(
  Student_ID = 1:30,
  Math_Score = round(rnorm(30, 85, 8)),
  Science_Score = round(rnorm(30, 88, 7)),
  Study_Hours = round(runif(30, 2, 15)),
  Gender = sample(c("Male", "Female"), 30, replace = TRUE)
)

# Source the helper file for the inferential UI. This makes the `inferential_tab_ui`
# object available to the main ui.R file.
source("R/utils.R")
source("R/ui_inferential.R")
