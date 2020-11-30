library("randomForest")
library("ggplot2")
library(ggfortify)
library(MASS)
library(dplyr)

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
  
  output$axis_x<-renderUI({
    selectInput("axis_x","Select variable x : ", choices = names(data_read()))
  })
  
  output$axis_y<-renderUI({
    selectInput("axis_y","Select variable y : ", choices = names(data_read()))
  })
  
  observeEvent(
    eventExpr = input[["submit_loc"]],
    handlerExpr = {
      
      nobs <- nrow(data_read())
      ntr <- 0.8*nobs
      indices.train <- 1:ntr # 
      indices.test <- (ntr+1):nobs
      model <-reactive({randomForest(as.factor(data_read()[indices.train,input$class]) ~ ., data=data_read()[indices.train ,c(input$predictors, input$class)],mtry = as.integer(input$mtry), ntree = as.integer(input$ntree), nodesize=as.integer(input$nodesize))})
      model.prediction <- reactive({predict(model(), newdata = data_read()[indices.test ,c(input$predictors, input$class)])})
      model.confusion_matrix <- reactive({table(model.prediction(), data_read()[indices.test ,input$class])})
      
      output$randomForest <- renderPrint({
        print(model())
      })
      
      output$confusionmatrix <- renderPrint({
        print(model.confusion_matrix())
      })
      
      
      output$accuracy_rate <- renderPrint({
        model.accuracyrate = (model.confusion_matrix()[1,1] + model.confusion_matrix()[2,2]) / (model.confusion_matrix()[1,1] + model.confusion_matrix()[1,2] + model.confusion_matrix()[2,1] +model.confusion_matrix()[2,2])
        model.accuracyrate
      })
      
      output$sensitivity <- renderPrint({
        model.sensitivity = model.confusion_matrix()[2,2]/(model.confusion_matrix()[1,2] + model.confusion_matrix()[2,2])
        model.sensitivity
      })
      
      output$specificity <- renderPrint({
        model.specificity = model.confusion_matrix()[1,1]/(model.confusion_matrix()[1,1] + model.confusion_matrix()[2,1])
        model.specificity
      })
      
      output$precision <- renderPrint({
        model.precision = model.confusion_matrix()[2,2]/(model.confusion_matrix()[2,1] + model.confusion_matrix()[2,2])
        model.precision
      })
      
      output$fmesure <- renderPrint({
        model.precision = model.confusion_matrix()[2,2]/(model.confusion_matrix()[2,1] + model.confusion_matrix()[2,2])
        model.sensitivity = model.confusion_matrix()[2,2]/(model.confusion_matrix()[1,2] + model.confusion_matrix()[2,2])
        
        model.fmesure = (2*model.precision*model.sensitivity)/(model.sensitivity + model.precision)
        model.fmesure
      })
      
      observeEvent(
          eventExpr = input[["submit_loc2"]],
          handlerExpr = { 
            output$prediction <- renderPlot({
              nobs <- nrow(data_read())
              ntr <- 0.8*nobs
              indices.test <- (ntr+1):nobs
              dftest <- data_read()[indices.test ,c(input$axis_x, input$axis_y, input$class)]
              dftest$prediction <- model.prediction()
              p <- plot(data_read()[indices.test ,c(input$axis_x, input$axis_y)], col = dftest$prediction, pch = 20, cex = 3) 
              print(p)
              })
            
            output$missclassified_prediction <- renderPlot({
              nobs <- nrow(data_read())
              ntr <- 0.8*nobs
              indices.test <- (ntr+1):nobs
              dftest <- data_read()[indices.test ,c(input$axis_x, input$axis_y, input$class)]
              dftest$prediction = model.prediction()
              dftrue = dftest[dftest$prediction!=data_read()[indices.test ,input$class],]
              if (nrow(dftrue)) {
                p <- plot(dftrue[1:nrow(dftrue) ,c(input$axis_x, input$axis_y)], col = dftrue$prediction, pch = 20, cex = 3) 
                print(p)}
        })
        })
      
      
    }
  )
  
}
