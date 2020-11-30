#UI model Result - Caroline de POURTALES
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
    verbatimTextOutput("randomForest")
  )
)

ui_model_result_prediction <- sidebarLayout(
  sidebarPanel(),
  mainPanel(plotOutput("prediction"))
)

ui_model_result_error <- sidebarLayout(
  sidebarPanel(),
  mainPanel(
    verbatimTextOutput("confusionmatrix"),
    verbatimTextOutput("accuracy_rate"),
    verbatimTextOutput("sensitivity"),
    verbatimTextOutput("specificity"),
    verbatimTextOutput("precision"),
    verbatimTextOutput("fmesure")
  )
)

ui_model_result <- mainPanel(
  tabsetPanel(
    tabPanel("Summary", ui_model_result_summary), 
    tabPanel("Confusion Matrix and Error", ui_model_result_error),
    tabPanel("Prediction", ui_model_result_prediction)
  ))
