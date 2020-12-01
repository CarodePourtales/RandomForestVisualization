#UI model Result - Caroline de POURTALES

ui_model_result_summary <- sidebarLayout(
  sidebarPanel(
    uiOutput('class'),
    uiOutput('predictors'),
    sliderInput('ntree','Number of trees to grow',min=10,max=1000,value=200,step=10),
    sliderInput('mtry','Number of variables randomly sampled as candidates at each split',min=1,max=20,value=2,step=1),
    sliderInput('nodesize','Minimum size of terminal nodes',min=1,max=50,value=10,step=1),
    actionButton(
      inputId = "submit_loc",
      label = "Submit"
    )
  ),
  mainPanel(
    h2("Model summary"),
    verbatimTextOutput("randomForest"),
    fluidRow(
      plotOutput("influence"),
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
      h2("Confusion matrix"),
      verbatimTextOutput("confusionmatrix")),
    fluidRow(
      h2("Some measures")),
    fluidRow(
      valueBoxOutput("accuracy_rate",
        width = 2),
      valueBoxOutput("sensitivity",
        width = 2),
      valueBoxOutput("specificity",
        width = 2),
      valueBoxOutput("precision",
        width = 2),
      valueBoxOutput("fmesure",
        width = 2)
      ),
    fluidRow(
      splitLayout(cellWidths = c("50%", "50%"), plotOutput("prediction"), plotOutput("missclassified_prediction"))
    ),
    width = 15
    )
)


ui_model_result <- tabItem(
  "model_result",
  tabsetPanel(
    tabPanel("Summary", ui_model_result_summary), 
    tabPanel("Prediction and accruracy", ui_model_result_prediction)
  ))
