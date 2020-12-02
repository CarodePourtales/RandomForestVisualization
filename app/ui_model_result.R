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
    tabsetPanel(
      tabPanel("Model Summary",
              h2("Model summary"),
              verbatimTextOutput("randomForest")
      ),
      tabPanel("Predictors' importance",
               h2("Predictors importance on the class"),
              plotOutput("influence")
      )
    )
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
      valueBoxOutput("accuracy_rate",
        width = 3),
      valueBoxOutput("sensitivity",
        width = 3),
      valueBoxOutput("specificity",
        width = 3)),
    fluidRow(
      valueBoxOutput("precision",
        width = 3),
      valueBoxOutput("fmesure",
        width = 3)),
    fluidRow(
      splitLayout(cellWidths = c("50%", "50%"), plotOutput("prediction"), plotOutput("missclassified_prediction"))
    )
    )
)


ui_model_result <- tabItem(
  "model_result",
  tabsetPanel(
    tabPanel("Summary", ui_model_result_summary), 
    tabPanel("Prediction and accruracy", ui_model_result_prediction)
  ))
