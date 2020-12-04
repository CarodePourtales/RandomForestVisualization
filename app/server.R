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
  
  observe({
    tryCatch({
      corr <- correlation()
      output$ui_corr_tab <- DT::renderDataTable({as.data.frame(corr)}) 
    }, error = function(e){
      output$ui_corr_tab_err <- renderPrint({e})
    })
  })
  
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
  
  # RESULT ==============
  observe({
    choices <- setdiff(names(data_read()), input$class)
    updateSelectInput(session, "predictors", choices = choices)
  })
  
  observe({
    updateSelectInput(session, "class", choices = names(data_read()))
  })
  
  observe({
    updateSelectInput(session, "axis_x", choices = names(data_read()))
  })
  
  observe({
    updateSelectInput(session, "axis_y", choices = names(data_read()))
  })
  
  model <-reactive({
    df <- data_read()
    
    nobs <- nrow(df)
    ntr <- input$split *nobs
    indices.train <- 1:ntr # 
    
    y_train <- as.factor(df[indices.train,input$class]) ~ .
    x_train <- df[indices.train ,c(input$predictors)]

    randomForest(y_train, data = x_train,
                    mtry = as.integer(input$mtry), ntree = as.integer(input$ntree), 
                    nodesize=as.integer(input$nodesize))
  })
  
  model.prediction <- reactive({
    nobs <- nrow(data_read())
    ntr <- input$split *nobs
    indices.test <- (ntr+1):nobs
    
    predict(model(), newdata = data_read()[indices.test ,c(input$predictors, input$class)])
  })
  
  model.confusion_matrix <- reactive({
    nobs <- nrow(data_read())
    ntr <- input$split *nobs
    indices.test <- (ntr+1):nobs
    
    table(model.prediction(), data_read()[indices.test ,input$class])
  })
  
  
  observe({
    pixel2inch <- function(x, y){
      return( c(x, y) * 0.010416 )
    }
    
    pred_influence <- function(pred, class) {
      do.call("partialPlot", list(x = model(), pred.data = data_read(), 
                                  x.var = pred, which.class = class, 
                                  main=paste("Partial Dependence on",  pred, "with class", class)))
    }
    
    predictors_influence <- reactive ({
      data <- data_read()
      vars = c(input$predictors)
      classes = c(unique(data[, input$class]))
      
      op <- par(mfrow=c(length(vars),length(classes)), mar=c(1,1,1,1))
      for (var in vars) {
        for (class in classes) {
          # print(var)
          # print(class)
          pred_influence(var, class)
        }
      } 
      par(op)
    })
    
    observe({
      output$randomForest <- renderPrint({
        # print(model())
        tryCatch({
          print(model())
        }, error = function(e){
          print("You must choose at least 2 predictors...")
        })
      })
    })
    
    observe({
      output$influence <- renderPlot ({
        tryCatch({
          predictors_influence()
        }, error = function(e){
          message("Waiting for predictors...")
        })
      })
    })
    
    observe({
      output$confusionmatrix <- renderPrint({
        tryCatch({
          model.confusion_matrix()
        }, error = function(e){
          message("Waiting for predictors...")
        })
      })
    })
    
    make_value_box <- function(title, subtitle){
      return(valueBox(title, subtitle, width = 2, icon = icon("search",lib = "glyphicon")))
    }
    
    observe({
      output$accuracy_rate <- renderValueBox({
        tryCatch({
          cm <- model.confusion_matrix()
          model.accuracyrate = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[1,2] + cm[2,1] +cm[2,2])
          model.accuracyrate =  round(model.accuracyrate, 3)
          make_value_box(tags$p("Accuracy", style = "font-size: 50%;"), tags$p(model.accuracyrate, style = "font-size: 170%;"))
        }, error = function(e){
          make_value_box(tags$p("Accuracy", style = "font-size: 50%;"), "Waiting for model...")
        })
      })
    })
    
    observe({
      output$sensitivity <- renderValueBox({
        tryCatch({
          cm <- model.confusion_matrix()
          model.sensitivity = cm[2,2]/(cm[1,2] + cm[2,2])
          model.sensitivity =  round(model.sensitivity, 3)
          make_value_box(tags$p("Sensitivity", style = "font-size: 50%;"), tags$p(model.sensitivity, style = "font-size: 170%;"))
        }, error = function(e){
            make_value_box(tags$p("Sensitivity", style = "font-size: 50%;"), "Waiting for model...")
        })
      })
    })
    
    observe({
      output$specificity <- renderValueBox({
        tryCatch({
          cm <- model.confusion_matrix()
          model.specificity = cm[1,1]/(cm[1,1] + cm[2,1])
          model.specificity =  round(model.specificity, 3)
          make_value_box(tags$p("Specificity", style = "font-size: 50%;"), tags$p(model.specificity, style = "font-size: 160%;"))
        }, error = function(e){
          make_value_box(tags$p("Specificity", style = "font-size: 50%;"), "Waiting for model...")
        })
      })
    })
    
    observe({
      output$precision <- renderValueBox({
        tryCatch({
          cm <- model.confusion_matrix()
          model.precision = cm[2,2]/(cm[2,1] + cm[2,2])
          model.precision =  round(model.precision, 3)
          make_value_box(tags$p("Precision", style = "font-size: 50%;"), tags$p(model.precision, style = "font-size: 170%;"))
        }, error = function(e){
          make_value_box(tags$p("Precision", style = "font-size: 50%;"), "Waiting for model...")
        })
      })
    })
    
    observe({
      output$fmesure <- renderValueBox({
        tryCatch({
          cm <- model.confusion_matrix()
          model.precision = cm[2,2]/(cm[2,1] + cm[2,2])
          model.sensitivity = cm[2,2]/(cm[1,2] + cm[2,2])
          
          model.fmesure = (2*model.precision*model.sensitivity)/(model.sensitivity + model.precision)
          model.fmesure =  round(model.fmesure, 3)
          make_value_box(tags$p("F-Measure", style = "font-size: 50%;"), tags$p(model.fmesure, style = "font-size: 170%;"))
        }, error = function(e){
          make_value_box(tags$p("F-Measure", style = "font-size: 50%;"), "Waiting for model...")
        })
      })
    })
  })
  
  observe({ 
    
    output$prediction <- renderPlot({
      tryCatch({
        nobs <- nrow(data_read())
        ntr <- input$split * nobs
        indices.test <- (ntr+1):nobs
        dftest <- data_read()[indices.test ,c(input$axis_x, input$axis_y, input$class)]
        dftest$prediction <- model.prediction()
        p <- ggplot(dftest, aes(x=dftest[, input$axis_x], y=dftest[, input$axis_y], color = factor(dftest[, input$class]) )) +
          labs(x = input$axis_x, y = input$axis_y, color = input$class) + ggtitle("Predictions")+ 
          geom_point() 
        print(p)
      }, error = function(e){
        message("Waiting for model...")
      })
    })
    
    output$missclassified_prediction <- renderPlot({
      tryCatch({
        nobs <- nrow(data_read())
        ntr <- input$split * nobs
        indices.test <- (ntr+1):nobs
        dftest <- data_read()[indices.test ,c(input$axis_x, input$axis_y, input$class)]
        dftest$prediction = model.prediction()
        dftrue = dftest[dftest$prediction!=dftest[,input$class],]
        if (nrow(dftrue)>0) {
          p <- ggplot(dftrue, aes(x=dftrue[, input$axis_x], y=dftrue[, input$axis_y], color = factor(dftrue[, input$class]) )) +
            labs(x = input$axis_x, y = input$axis_y, color = input$class) + ggtitle("Missclassified Predictions")+
            geom_point()
          print(p)}
        }, error = function(e){
          message("Waiting for model...")
        })
      })
  })
  
  #========== RESULT
}
