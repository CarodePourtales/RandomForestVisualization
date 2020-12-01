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
    h2("Model summary"),
    verbatimTextOutput("randomForest"),
    width = 12
  )
)

ui_model_result_prediction <- sidebarLayout(
  sidebarPanel(
    uiOutput('axis_x'),
    uiOutput('axis_y'),
    actionButton(
      inputId = "submit_loc2",
      label = "Submit"
    )
  ),
  mainPanel(
    fluidRow(
      verbatimTextOutput("confusionmatrix"),
      valueBoxOutput("accuracy_rate",
        width = 5),
      valueBoxOutput("sensitivity",
        width = 5),
      valueBoxOutput("specificity",
        width = 5),
      valueBoxOutput("precision",
        width = 5),
      valueBoxOutput("fmesure",
        width = 5)
      ),
    fluidRow(
      splitLayout(cellWidths = c("50%", "50%"), plotOutput("prediction"), plotOutput("missclassified_prediction"))
    ),
    width = 12
    )
)


ui_model_result <- tabItem(
  "model_result",
  tabsetPanel(
    tabPanel("Summary", ui_model_result_summary), 
    tabPanel("Prediction and accruracy", ui_model_result_prediction)
  ))
