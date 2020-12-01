library(shiny)
library(shinydashboard)
library(fresh)
library(DT)
library(randomForest)
library(ggplot2)
library(ggfortify)
library(MASS)
library(dplyr)


source("ui.R")
source("server.R")

# Shiny App ---------------------------------------------------------------

shinyApp(ui, server)