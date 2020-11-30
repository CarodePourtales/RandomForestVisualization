# UI ----------------------------------------------------------------------
ui_model <- sidebarLayout(
  sidebarPanel(
    fileInput('file', 'Choose CSV File',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
    hr(),
    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 ','),
    radioButtons('quote', 'Quote',
                 c(None='',
                   'Double Quote'='"',
                   'Single Quote'="'"),
                 '"'),
    hr(),
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
    tableOutput('contents'),
    textOutput('str'),
    tableOutput('randomForest')
  )
)

ui_data <- sidebarLayout(sidebarPanel(),
                         mainPanel())

ui_model_overview <- sidebarLayout(sidebarPanel(),
                                   mainPanel())
    
ui <- (fluidPage(
  
  titlePanel("Dashboard"),
  
  navbarPage("Menu",
             tabPanel("Data Overview", ui_data),
             tabPanel("Model Overview", ui_model_overview),
             tabPanel("Model Result", ui_model))
))
