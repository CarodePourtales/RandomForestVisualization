#UI model Result - Caroline de POURTALES & Minh-Hoang DANG

ui_model_result_summary <- sidebarLayout(
  sidebarPanel(
    selectInput("dtree_type", "Tree type", c("Single Tree" = "static", "Forest" = "dynamic")),
    conditionalPanel("input.dtree_type == 'dynamic'",
                     selectInput("dtree_package", "Using package", c("Random Forest" = "randomForest"), selected = "randomForest")),
    selectInput("class","Select class : ", choices = c("Loading...")),
    helpText("Select the predictors fo the model. Drag and drop to reorder."),
    selectizeInput("predictors", "", c("Loading..."), multiple = T, options = list(plugins = list("remove_button", "drag_drop"))),
    hr(),
    sliderInput('split','Train rate',min=0.50,max=1.0,value=0.75,step=0.01),
    
    conditionalPanel("input.dtree_type == 'dynamic'",
                    sliderInput('ntree','Number of trees to grow',min=10,max=1000,value=200,step=10),
                    sliderInput('mtry','Number of variables randomly sampled as candidates at each split',min=1,max=20,value=2,step=1),
                    conditionalPanel("input.dtree_package == 'randomForest'", 
                                     sliderInput('nodesize','Minimum size of terminal nodes',min=1,max=50,value=10,step=1)
                    ),
                    conditionalPanel("input.dtree_package == 'cforest'", 
                                     sliderInput("maxdepth", "Maximum depth of tree", min = 0, max = 10, value = 4, step = 1)
                    ),
    ),
    conditionalPanel("input.dtree_type == 'static'",
                     numericInput("cp", "Complexity parameter", min = 0.001, max = 1.0, step = 0.001, value = 0.01)
    ),
  ),
  mainPanel(
    verbatimTextOutput("warning"),
    tabsetPanel(
      tabPanel("Model Summary",
              h2("Model summary"),
              verbatimTextOutput("model_print"),
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
               conditionalPanel("input.dtree_type != 'dynamic' || input.dtree_package != 'randomForest'",
                                helpText("Choose 2 variables. Drag and drop to reorder."), 
                                selectizeInput("dtree_par2vars", "", c("Loading..."), multiple = T, options = list(plugins = list("remove_button")))
                    ),
              plotOutput("influence")
      )
    )
  )
)

ui_model_result_prediction <- sidebarLayout(
  sidebarPanel(
    selectInput("axis_x","Select variable x : ", choices = c("Loading...")),
    selectInput("axis_y","Select variable y : ", choices = c("Loading...")),
    hr(),
    conditionalPanel("input.dtree_type == 'static'",
                     numericInput("tweak", "Adjust the size of the decision tree", min = 0.01, max = 100, step = 0.01, value = 1)),
    conditionalPanel("input.dtree_type == 'dynamic'",
                     sliderInput("ktree", "Tree's number to plot", min = 1, max = Inf, value = 1, step = 1))
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Prediction Summary",
        splitLayout(cellWidths = c("50%", "50%"), plotOutput("wellclassified_prediction"), plotOutput("missclassified_prediction"))
      ),
      tabPanel("Decision Tree",
               h2("Visualise the decision making process"),
               plotOutput("dtree"))
    )
  )
)


ui_model_result <- tabItem(
  "model_result",
  tabsetPanel(
    tabPanel("Summary", ui_model_result_summary), 
    tabPanel("Prediction and accruracy", ui_model_result_prediction)
  ))
