library(randomForest)
library(ggplot2)
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
      
      output$confusionmatrix <- renderValueBox({
        valueBox(
          "Confusion matrix",
          model.confusion_matrix(),
          icon = icon("angle-right")
        )
      })
      
      
      output$accuracy_rate <- renderValueBox({
        model.accuracyrate = (model.confusion_matrix()[1,1] + model.confusion_matrix()[2,2]) / (model.confusion_matrix()[1,1] + model.confusion_matrix()[1,2] + model.confusion_matrix()[2,1] +model.confusion_matrix()[2,2])
        valueBox(
          "Accuracy rate",
          model.accuracyrate,
          icon = icon("credit-card")
        )
      })
      
      output$sensitivity <- renderValueBox({
        model.sensitivity = model.confusion_matrix()[2,2]/(model.confusion_matrix()[1,2] + model.confusion_matrix()[2,2])
        valueBox(
          "Sensitivity",
          model.sensitivity,
          icon = icon("credit-card")
        )
      })
      
      output$specificity <- renderValueBox({
        model.specificity = model.confusion_matrix()[1,1]/(model.confusion_matrix()[1,1] + model.confusion_matrix()[2,1])
        valueBox(
          "Specificity",
          model.specificity,
          icon = icon("credit-card")
        )
      })
      
      output$precision <- renderValueBox({
        model.precision = model.confusion_matrix()[2,2]/(model.confusion_matrix()[2,1] + model.confusion_matrix()[2,2])
        valueBox(
          "Precision",
          model.precision,
          icon = icon("credit-card")
        )
      })
      
      output$fmesure <- renderValueBox({
        model.precision = model.confusion_matrix()[2,2]/(model.confusion_matrix()[2,1] + model.confusion_matrix()[2,2])
        model.sensitivity = model.confusion_matrix()[2,2]/(model.confusion_matrix()[1,2] + model.confusion_matrix()[2,2])
        
        model.fmesure = (2*model.precision*model.sensitivity)/(model.sensitivity + model.precision)
        valueBox(
          "F measure",
          model.fmesure,
          icon = icon("credit-card")
        )
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
