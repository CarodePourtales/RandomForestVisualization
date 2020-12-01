# UI ----------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(fresh)

source("ui_model_result.R")
source("ui_model_overview.R")
source("ui_data_exploration.R")


  
ui <- (fluidPage(
  
  use_theme("bootstrap.css"),
  
  titlePanel("Dashboard"),
  
  navbarPage("Menu",
             tabPanel("Data Exploration", ui_data_exploration),
             tabPanel("Model Overview", ui_model_overview),
             tabPanel("Model Result", ui_model_result))
))
