library("randomForest")

# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  data_read <- function() {
    data <- input$file
    if (is.null(data))
      return(NULL)
    df <- read.csv(data$datapath, header=input$header, sep=input$sep, 
                   quote=input$quote)
    # clean up data frame to set types appropriately
    return(df)
  }
  
  output$predictors<-renderUI({
    checkboxGroupInput("predictors","Select predictors : ", choices = names(data_read()))
  })
  
  output$class<-renderUI({
    selectInput("class","Select class : ", choices = names(data_read()))
  })
  
  observeEvent(
    eventExpr = input[["submit_loc"]],
    handlerExpr = {
      output$randomForest <- renderPrint({
        df = data_read()
        # Build the training/validate/test datasets.
        nobs <- nrow(df)
        ntr <- 0.8*nobs # assumes first 20% of data tunes good system
        indices.train <- 1:ntr # 
        set.seed(42)
        model <-  randomForest(input$class ~ ., data=df[indices.train ,c(input$predictors, input$class)],mtry = as.integer(input$mtry), ntree = as.integer(input$ntree), nodesize=as.integer(input$nodesize))
        print(model)
      })
    }
  )
  
}
