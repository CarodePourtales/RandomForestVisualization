#UI model Overview - ...

ui_model_correlation <- sidebarLayout(
  sidebarPanel(),
  mainPanel()
)

ui_model_predictors <- sidebarLayout(
  sidebarPanel(),
  mainPanel()
)

ui_model_class_predictors <- sidebarLayout(
  sidebarPanel(),
  mainPanel()
)

ui_model_overview <- mainPanel(
  tabsetPanel(
    tabPanel("Correlation", ui_model_correlation),
    tabPanel("Predictors overview", ui_model_predictors),
    tabPanel("Chosen predictors and class correlation", ui_model_class_predictors) 
  ))