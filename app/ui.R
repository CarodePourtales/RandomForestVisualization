# UI ----------------------------------------------------------------------

ui_data_loading <- sidebarLayout(
  sidebarPanel(
    fileInput('file', 'Choose CSV File',
              accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv')),
    hr(),
    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator',
                 c(Comma=',', Semicolon=';',Tab='\t'),','),
    radioButtons('quote', 'Quote',c(None='','Double Quote'='"','Single Quote'="'"), '"'),
    hr()),
  mainPanel(
  )
)

ui_data_summary <- sidebarLayout(
  sidebarPanel(),
  mainPanel()
)

ui_data_plot <- sidebarLayout(
  sidebarPanel(),
  mainPanel()
)

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

ui_model_result_summary <- sidebarLayout(
  sidebarPanel(
    uiOutput('class'),
    uiOutput('predictors'),
    sliderInput('ntree','Number of trees',min=10,max=1000,value=200,step=10),
    sliderInput('mtry','Number of trial at each node to choose one',min=1,max=20,value=2,step=1),
    sliderInput('nodesize','Maximal depth',min=1,max=50,value=10,step=1),
    actionButton(
      inputId = "submit_loc",
      label = "Submit"
    )
  ),
  mainPanel(
  )
)

ui_model_result_prediction <- sidebarLayout(
  sidebarPanel(),
  mainPanel()
)

ui_model_result_error <- sidebarLayout(
  sidebarPanel(),
  mainPanel()
)


ui_data_exploration <- mainPanel(
        tabsetPanel(
             tabPanel("Data Loading", ui_data_loading),
             tabPanel("Distribution", ui_data_summary), 
             tabPanel("Plot", ui_data_plot)
))

ui_model_overview <- mainPanel(
       tabsetPanel(
          tabPanel("Correlation", ui_model_correlation),
          tabPanel("Predictors overview", ui_model_predictors),
          tabPanel("Chosen predictors and class correlation", ui_model_class_predictors) 
))


ui_model_result <- mainPanel(
    tabsetPanel(
      tabPanel("Summary", ui_model_result_summary), 
      tabPanel("Prediction", ui_model_result_prediction),
      tabPanel("Confusion Matrix and Error", ui_model_result_error)
    ))
  
ui <- (fluidPage(
  
  titlePanel("Dashboard"),
  
  navbarPage("Menu",
             tabPanel("Data Exploration", ui_data_exploration),
             tabPanel("Model Overview", ui_model_overview),
             tabPanel("Model Result", ui_model_result))
))
