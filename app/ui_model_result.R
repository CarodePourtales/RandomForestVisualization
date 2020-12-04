#UI model Result - Caroline de POURTALES & Minh-Hoang DANG

ui_model_result_summary <- sidebarLayout(
  sidebarPanel(
    selectInput("class","Select class : ", choices = c("Loading...")),
    selectizeInput("predictors", "", c("Loading..."), multiple = T, options = list(plugins = list("remove_button"))),
    sliderInput('split','Train rate',min=0.50,max=1.0,value=0.75,step=0.01),
    sliderInput('ntree','Number of trees to grow',min=10,max=1000,value=200,step=10),
    sliderInput('mtry','Number of variables randomly sampled as candidates at each split',min=1,max=20,value=2,step=1),
    sliderInput('nodesize','Minimum size of terminal nodes',min=1,max=50,value=10,step=1),
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Model Summary",
              h2("Model summary"),
              verbatimTextOutput("randomForest"),
              fluidRow(
                valueBoxOutput("accuracy_rate", width = 3),
                valueBoxOutput("sensitivity", width = 3),
                valueBoxOutput("specificity", width = 3)),
              fluidRow(
                valueBoxOutput("precision", width = 3),
                valueBoxOutput("fmesure", width = 3)),
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
    selectInput("axis_x","Select variable x : ", choices = c("Loading...")),
    selectInput("axis_y","Select variable y : ", choices = c("Loading...")),
  ),
  mainPanel(
    fluidRow(
      splitLayout(cellWidths = c("50%", "50%"), plotOutput("prediction"), plotOutput("missclassified_prediction"))
    )
    )
)

ui_model_tree <- 
  mainPanel(
    fluidRow(
      h2("Tree with rpart library"),
      plotOutput("tree")
    )
)

ui_model_result <- tabItem(
  "model_result",
  tabsetPanel(
    tabPanel("Summary", ui_model_result_summary), 
    tabPanel("Prediction and accruracy", ui_model_result_prediction),
    tabPanel("Tree structure", ui_model_tree)
  ))
