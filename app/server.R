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
      
      output$execute <- renderPrint({
        df = reactive({data_read()})
        # Build the training/validate/test datasets.
        nobs <- nrow(df)
        ntr <- 0.8*nobs # assumes first 20% of data tunes good system
        indices.train <- 1:ntr # 
        indices.test <- (ntr+1):nobs
        set.seed(42)
        x <- input$class
        model <-  reactive({randomForest(as.factor(df[indices.train,x]) ~ ., data=df[indices.train ,c(input$predictors, input$class)],mtry = as.integer(input$mtry), ntree = as.integer(input$ntree), nodesize=as.integer(input$nodesize))})
        
        df.test <- df[indices.test ,c(input$predictors, input$class)]
        df$prediction <- predict(model, df.test , type="response",
                                   norm.votes=TRUE, predict.all=FALSE, proximity=FALSE, nodes=FALSE)
        
        })
      
      output$randomForest <- renderPrint({
        print(model)
      })
      
      output$cross_validation <- renderPlot({
        nobs <- nrow(df)
        ntr <- 0.8*nobs 
        indices.train <- 1:ntr # 
        x <- input$class
        set.seed(42)
        accuracyrate <- rep(NA,20)
        deg = 1:20
        for (d in deg) {
          model <- randomForest(as.factor(df[indices.train,x]) ~ ., 
                                data=df[indices.train ,c(input$predictors, input$class)], 
                                mtry = as.integer(input$mtry), ntree = as.integer(input$ntree), nodesize=d)
          model.confusion_matrix = model$confusion
          model.accuracyrate = (model.confusion_matrix[1,1] + model.confusion_matrix[2,2]) / (model.confusion_matrix[1,1] + model.confusion_matrix[1,2] + model.confusion_matrix[2,1] +model.confusion_matrix[2,2])
          accuracyrate[d] = model.accuracyrate
        }
        p <- plot(deg, accuracyrate, type = "l", lty = 1)
        print(p)
      })
      
      output$confusionmatrix <- renderPrint({
        model$confusion[1:2,1:2]
      })
      
      output$prediction <- renderPlot({
        nobs <- nrow(df)
        ntr <- 0.8*nobs
        indices.test <- (ntr+1):nobs
        p <- ggplot() + geom_point(aes(c(input$predictors), color = factor(df$prediction)), data = df[indices.test ,c(input$predictors)], ) + scale_color_manual(values = c("red", "green"))
        print(p)
        })
      
      output$accuracy_rate <- renderPrint({
        model.accuracyrate = (model$confusion[1,1] + model$confusion[2,2]) / (model$confusion[1,1] + model$confusion[1,2] + model$confusion[2,1] +model$confusion[2,2])
        model.accuracyrate
      })
      
    }
  )
  
}
