# UI ----------------------------------------------------------------------

source("ui_model_result.R")
source("ui_model_overview.R")
source("ui_data_exploration.R")


  
ui <- (dashboardPage(
  
  dashboardHeader(
    title = 'Dashboard'
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Exploration", tabName = "data_exploration", icon = icon("dashboard")),
      menuItem("Model Overview", tabName = "model_overview", icon = icon("stream")),
      menuItem("Model Result", tabName = "model_result", icon = icon("trophy")))),
      
  
  dashboardBody(
    tabItems(
      ui_data_exploration,
      ui_model_overview,
      ui_model_result)
  )
))
