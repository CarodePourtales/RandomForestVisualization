# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # METHOD ====
  data_read <- function() {
    data <- input$file
    if (is.null(data))
      return(NULL)
    df <- read.csv(data$datapath, header=input$header, sep=input$sep, 
                   quote=input$quote)
    # clean up data frame to set types appropriately
    return(df)
  }
  
  numericColumns <- reactive({
    df <- data_read()
    colnames(df)[sapply(df, is.numeric)]
  })
  
  corTest <- function(mat, conf.level = 0.95) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    diag(lowCI.mat) <- diag(uppCI.mat) <- 1
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        try({
          tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
          p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
          lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
          uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
        }, silent = TRUE)
      }
    }
    return(list(p.mat, lowCI.mat, uppCI.mat))
  }
  
  sigConfMat <- reactive({
    val <- correlation()
    if(!is.null(val))
      corTest(val, input$corr_confLevel)
  })
  
  # ===== METHOD
  
  # OVERVIEW ========
  # https://github.com/saurfang/shinyCorrplot/
  
  # Correlation
  correlation <- reactive({
    data <- data_read()
    vars <- input$variables
    
    if(is.null(data) || !length(intersect(vars, colnames(data)))) {
      return(NULL)
    } else {
      tryCatch({
        res <- cor(data_read()[,input$variables], use = input$corr_use, method = input$corr_method)
        if(input$corr_precision == 0){
          return(res)
        }
        else{
          return(round(res, input$corr_precision))
        }
      }, error = function(e){
        return(NULL)
      })
    }
  })
  
  #Update hclust rect max
  observe({
    val <- correlation()
    if(!is.null(val))
      updateNumericInput(session, "plotHclustAddrect", max = nrow(val))
  })
  
  #Update variable selection
  observe({
    updateSelectInput(session, "variables", choices = numericColumns(), selected = numericColumns())
  })
  
  # Corr plot
  observe(tryCatch({
    output$ui_corr_plot <- renderPlot({
      val <- correlation()
      if(is.null(val)) return(NULL)
      
      val[is.na(val)] <- 0
      args <- list(val,
                   order = if(input$corr_plotOrder == "manual") "original" else input$corr_plotOrder, 
                   hclust.method = input$corr_plotHclustMethod, 
                   addrect = input$corr_plotHclustAddrect,
                   
                   p.mat = sigConfMat()[[1]],
                   sig.level = if(input$corr_sigTest) input$corr_sigLevel else NULL,
                   insig = if(input$corr_sigTest) input$corr_sigAction else NULL,
                   
                   lowCI.mat = sigConfMat()[[2]],
                   uppCI.mat = sigConfMat()[[3]],
                   plotCI = if(input$corr_showConf) input$corr_confPlot else "n")
      
      if(input$corr_showConf) {
        do.call(corrplot, c(list(type = input$corr_plotType), args))
      } else if(input$corr_plotMethod == "mixed") {
        do.call(corrplot.mixed, c(list(lower = input$corr_plotLower,
                                       upper = input$corr_plotUpper),
                                  args))
      } else {
        do.call(corrplot, c(list(method = input$corr_plotMethod, type = input$corr_plotType), args))
      }
    })
  }, error = function(e){
    output$ui_submit_cor_heatmap_err <- renderPrint({e})
  }))
  
  observe(tryCatch({
    corr <- correlation()
    output$ui_corr_tab <- DT::renderDataTable({
      as.data.frame(corr)
    }, ) 
  }, error = function(e){
    output$ui_corr_tab_err <- renderPrint({e})
  }))
  
  # Predictor overview
  observe({
    updateSelectInput(session, "ui_predictors", choices = numericColumns(), selected = numericColumns())
  })
  
  output$predictors_summary <- DT::renderDataTable({
    predictors <- input$ui_predictors
    
    if (length(predictors) == 0){
      return(NULL)
    }
    
    data <- apply(data_read()[predictors], 2, summary)
    return(as.data.frame(data))
  })
  
  #====== OVERVIEW
  
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
      model <-reactive({randomForest(as.factor(data_read()[indices.train,input$class]) ~ ., 
                                     data=data_read()[indices.train ,c(input$predictors)],
                                     mtry = as.integer(input$mtry), ntree = as.integer(input$ntree), 
                                     nodesize=as.integer(input$nodesize))})
      model.prediction <- reactive({predict(model(), newdata = data_read()[indices.test ,c(input$predictors, input$class)])})
      model.confusion_matrix <- reactive({table(model.prediction(), data_read()[indices.test ,input$class])})
      
      pred_influence <- function(pred, class) {
        do.call("partialPlot", list(x = model(), pred.data = data_read(), x.var = pred, which.class = class, main=paste("Partial Dependence on",  pred, "with class", class)))
      }
      
      predictors_influence <- reactive ({
        data <- data_read()
        vars = colnames(data[,c(input$predictors)])
        classes = c(unique(data[,input$class]))
        op <- par(mfrow=c(length(vars),length(classes)))
        for (var in vars) {
          for (class in classes) {
              pred_influence(var, class)
          }
        } 
        par(op)
      })
      
      observe({
        output$randomForest <- renderPrint({
          print(model())
        })
      })

      
      observe({
        output$influence <- renderPlot ({
          predictors_influence()
        })
      })
      
      observe({
        output$confusionmatrix <- renderPrint({
          model.confusion_matrix()
        })
      })
      
      observe({
        output$accuracy_rate <- renderValueBox({
          model.accuracyrate = (model.confusion_matrix()[1,1] + model.confusion_matrix()[2,2]) / (model.confusion_matrix()[1,1] + model.confusion_matrix()[1,2] + model.confusion_matrix()[2,1] +model.confusion_matrix()[2,2])
          model.accuracyrate =  round(model.accuracyrate, 3)
          valueBox(
            "Accuracy rate",
            model.accuracyrate,
            width = 2,
            icon = icon("credit-card")
          )
        })
      })
      
      observe({
        output$sensitivity <- renderValueBox({
          model.sensitivity = model.confusion_matrix()[2,2]/(model.confusion_matrix()[1,2] + model.confusion_matrix()[2,2])
          model.sensitivity =  round(model.sensitivity, 3)
          valueBox(
            "Sensitivity",
            model.sensitivity,
            width = 2,
            icon = icon("credit-card")
          )
        })
      })
      
      observe({
        output$specificity <- renderValueBox({
          model.specificity = model.confusion_matrix()[1,1]/(model.confusion_matrix()[1,1] + model.confusion_matrix()[2,1])
          model.specificity =  round(model.specificity, 3)
          valueBox(
            "Specificity",
            model.specificity,
            width = 2,
            icon = icon("credit-card")
          )
        })
      })
      
      observe({
        output$precision <- renderValueBox({
          model.precision = model.confusion_matrix()[2,2]/(model.confusion_matrix()[2,1] + model.confusion_matrix()[2,2])
          model.precision =  round(model.precision, 3)
          valueBox(
            "Precision",
            model.precision,
            width = 2,
            icon = icon("credit-card")
          )
        })
      })
      
      observe({
        output$fmesure <- renderValueBox({
          model.precision = model.confusion_matrix()[2,2]/(model.confusion_matrix()[2,1] + model.confusion_matrix()[2,2])
          model.sensitivity = model.confusion_matrix()[2,2]/(model.confusion_matrix()[1,2] + model.confusion_matrix()[2,2])
          
          model.fmesure = (2*model.precision*model.sensitivity)/(model.sensitivity + model.precision)
          model.fmesure =  round(model.fmesure, 3)
          valueBox(
            "F measure",
            model.fmesure,
            width = 2,
            icon = icon("credit-card")
          )
        })
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
              p <- plot(data_read()[indices.test ,c(input$axis_x, input$axis_y)], col = dftest$prediction, pch = 20, cex = 3, main=paste("Predictions")) 
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
                p <- plot(dftrue[1:nrow(dftrue) ,c(input$axis_x, input$axis_y)], col = dftrue$prediction, pch = 20, cex = 3, main=paste("Missclassified predictions")) 
                print(p)}
        })
        })
      
      
    }
  )
  
}
