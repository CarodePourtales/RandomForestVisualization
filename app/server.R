# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  # ATTRIBUTES
  saved_models <<- hash()
  current_model <<- hash()
  
  # METHOD ====
  data_read <- function() {
    data <- input$file
    if (is.null(data))
      return(NULL)
    df <- read.csv(data$datapath, header=input$header, sep=input$sep, 
                   quote=input$quote, check.names = TRUE)
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
  
  # ==== DATA 
  observe({
    choices <- names(data_read())
    updateSelectInput(session, "data_cols", choices = choices, selected = choices)
  })
  
  observe({
    data <- data_read()
    output$tbl <- DT::renderDataTable(data[, input$data_cols], options = list(lengthChange = FALSE), width = 'auto')
  })
  
  # ****
  
  observe({
    choices <- numericColumns()
    updateSelectInput(session, "distrib_box_vars", choices = choices, selected = choices)
  })
  
  observe({
    choices <- numericColumns()
    updateSelectInput(session, "distrib_hist_vars", choices = choices)
  })
  
  observe({
    choices <- numericColumns()
    updateSelectInput(session, "plot_scatter_x", choices = choices)
  })
  
  observe({
    choices <- numericColumns()
    updateSelectInput(session, "plot_scatter_y", choices = choices)
  })
  
  observe({
    choices <- numericColumns()
    updateSelectInput(session, "plot_scatter_clr", choices = choices)
  })
  
  observe({
    output$distrib_out <- renderPlot(
      switch(
        input$distrib_type,
        "boxplot" = {
          # boxplot(data_read()[, input$distrib_box_vars])
          df <- data_read()
          x <- df[, input$distrib_box_vars]
          
          plt <- ggplot(melt(x), aes(x = variable, y = value))
          if (input$distrib_box_form == "violin") 
            plt <- plt + geom_violin(aes(fill = variable)) 
          else 
            plt <- plt + geom_boxplot(aes(fill = variable))
          
          plt <- plt + scale_fill_discrete(type = "viridis") +
            theme(
                legend.position="none",
                plot.title = element_text(size=11)
            )
          plt
        },
        "histogram" = {
          # hist(data_read()[, input$distrib_hist_var], 
          #      main = paste("Histogram for", input$distrib_hist_var),
          #      xlab = input$distrib_hist_var, freq = (input$distrib_hist_freq == "Frequency"))
          
          stat = if(input$distrib_hist_space == "Continous") "bin" else "count"
          
          df <- data_read()
          
          if (length(input$distrib_hist_vars) == 0)
            return(NULL)
          
          x <- df[, input$distrib_hist_vars]
          
          plt <- NULL
          
          if(length(input$distrib_hist_vars) == 1){
            if(input$distrib_hist_freq == "Frequency")
              plt <- ggplot(df, aes(x = x, y = (..count..))) + geom_histogram(alpha = 0.6, stat = stat)
            else
              plt <- ggplot(df, aes(x = x, y = (..count..)/sum(..count..))) + geom_histogram(alpha = 0.6, stat = stat)
          }
          else{
            if(input$distrib_hist_freq == "Frequency")
              plt <- ggplot(melt(x)) + geom_histogram(aes(x = value, y = (..count..), fill = variable), alpha = 0.6, stat = stat)
            else
              plt <- ggplot(melt(x)) + geom_histogram(aes(x = value, y = (..count..)/sum(..count..), fill = variable), alpha = 0.6, stat = stat)
          }
          
          plt <- plt + labs(x = "values", y = input$distrib_hist_freq )
          plt
        }
      )
    )
  })
  
  observe({
    output$plot_out <- renderPlot({
      df <- data_read()
      xlab <- input$plot_scatter_x
      ylab <- input$plot_scatter_y
      
      switch(
        EXPR = input$plot_type,
        "Scatter" = {
          clr <- df[, input$plot_scatter_clr]

          ggplot(df, aes(x=df[, xlab], y=df[, ylab], color = factor(clr) )) +
            labs(x = xlab, y = ylab, color = input$plot_scatter_clr) +
            geom_point()
        },
        "2D Histogram" = {
          ggplot(df, aes(x=df[, xlab], y=df[, ylab])) + geom_bin2d() +
            labs(x = xlab, y = ylab) +
            scale_fill_continuous(type = "viridis") +
            theme_bw()
        },
        "Hexbin" = {
          ggplot(df, aes(x=df[, xlab], y=df[, ylab])) + geom_hex() +
            labs(x = xlab, y = ylab) +
            scale_fill_continuous(type = "viridis") +
            theme_bw()
        },
        "2D Density" = {
          geom <- input$plot_density_geom
          contour <- (input$plot_density_contour == "TRUE")
          plt <- ggplot(df, aes(x=df[, xlab], y=df[, ylab])) + geom_density_2d()
          
          if (geom == "raster"){
            plt <- plt + stat_density_2d(aes(fill = ..density..), geom = geom, contour = FALSE)
          }else{
            plt <- plt + stat_density_2d(aes(fill = ..level..), geom = geom)
          }

          plt <- plt +
            labs(x = xlab, y = ylab) +
            scale_fill_continuous(type = "viridis") +
            theme_bw()
          plt
        }
      )
    })
  })
  
  # DATA ====
  
  # ==== OVERVIEW
  # https://github.com/saurfang/shinyCorrplot/
  
  observe({
    updateSelectInput(session, "variables_transformation", choices = (names(data_read())))
  })
  
  observe({
    choices <- c("Root"="sqrt","Logarithm"="log","Exponential"="exp","Square product"="square", "Pow 3"="volume")
    updateSelectInput(session, "transformations", choices = choices)
  })
  
  observe({
    output$no_transformation_plot <- renderPlot ({
      
      df <- data_read()
      
      if (length(input$variables_transformation) == 0)
        return(NULL)
      
      x <- df[, input$variables_transformation]
      plt <- hist(x, main = 'Variable without transformation')
      plt
    })
  })
  
  observe({
    output$transformation_plot <- renderPlot ({
      
      df <- data_read()
      
      if (length(input$variables_transformation) == 0)
        return(NULL)
      
      x <- df[, input$variables_transformation]
      
      if (input$transformations == "sqrt") {
        plt <- hist(sqrt(x), main = 'Variable with transformation')
        plt
      }
      if (input$transformations == "log") {
        plt <- hist(log(x), main = 'Variable with transformation')
        plt
      }
      if (input$transformations == "exp") {
        plt <- hist(exp(x), main = 'Variable with transformation')
        plt
      }
      if (input$transformations == "square") {
        plt <- hist(x**2, main = 'Variable with transformation')
        plt
      }
      if (input$transformations == "volume") {
        plt <- hist(x**3, main = 'Variable')
        plt
      }
    })
  })
  
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
  
  observeEvent(input$distrib_hist_vars, {
    output$predictors_summary <- DT::renderDataTable({
      predictors <- input$distrib_hist_vars
      
      if (length(predictors) == 0){
        return(NULL)
      }
      
      data <- apply(data_read()[predictors], 2, summary)
      return(as.data.frame(data))
    })
  })
  
  observeEvent(input$distrib_box_vars, {
    output$predictors_summary <- DT::renderDataTable({
      predictors <- input$distrib_box_vars
      
      if (length(predictors) == 0){
        return(NULL)
      }
      
      data <- apply(data_read()[predictors], 2, summary)
      return(as.data.frame(data))
    })
  })
  
  # OVERVIEW ====

  # ==== RESULT
  observe({
    choices <- setdiff(names(data_read()), input$class)
    updateSelectInput(session, "predictors", choices = choices)
  })
  
  observe({
    updateSelectInput(session, "class", choices = names(data_read()))
  })
  
  observe({
    updateSelectInput(session, "axis_x", choices = numericColumns())
  })
  
  observe({
    updateSelectInput(session, "axis_y", choices = numericColumns())
  })
  
  observe({
    updateSliderInput(session, "ktree", max = as.integer(input$ntree))
  })
  
  model = reactive({
    df <- data_read()
    
    nobs <- nrow(df)
    ntr <- input$split * nobs
    indices.train <- c(1:ntr) # 
    
    y_train <- as.factor(df[indices.train, input$class]) ~ .
    x_train <- df[indices.train, c(input$predictors)]

    if(input$dtree_type == "dynamic" && input$dtree_package == "randomForest"){
      return(randomForest(y_train, x_train,
                      mtry = as.integer(input$mtry), ntree = as.integer(input$ntree),
                      nodesize=as.integer(input$nodesize))
      )
    }
    if(input$dtree_type == "dynamic" && input$dtree_package == "cforest"){
        return(cforest(y_train, x_train, 
                    control = cforest_unbiased(ntree = as.integer(input$ntree), mtry = as.integer(input$mtry), 
                   maxdepth = as.integer(input$maxdepth)))
        )
      }
    else{
      return(rpart(y_train, x_train, method = "class", control = rpart.control(cp = input$cp)))
    }
  })
  
  model.prediction <- reactive({
    df <- data_read()
    nobs <- nrow(df)
    ntr <- input$split *nobs
    indices.test <- (ntr+1):nobs
    
    x_test <- df[indices.test ,c(input$predictors, input$class)]
    
    m <- model()
    if(input$dtree_type == "dynamic") {
      if(input$dtree_package == "cforest"){
        current_model[["prediction"]] <<- predict(m, newdata = x_test, type = "response")
        current_model[["prediction"]]
      }
      if(input$dtree_package == "randomForest"){
        predict(m, newdata = x_test)
      }
    }
    else {
      predict(m, x_test, type = "class")
    }
  })
  
  model.confusion_matrix <- reactive({
    df <- data_read()
    nobs <- nrow(df)
    ntr <- input$split *nobs
    indices.test <- (ntr+1):nobs
    
    y_test <- df[indices.test ,input$class]
    
    table(model.prediction(), y_test)
  })

  observe({
    pixel2inch <- function(x, y){
      return( c(x, y) * 0.010416 )
    }

    observe({
      output$model_print <- renderPrint({
        print(model())
        
        # tryCatch({
        #   print(model())
        # }, error = function(e){
        #   print("You must choose at least 2 predictors...")
        # })
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
          current_model[["accuracyrate"]] <<- model.accuracyrate
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
          current_model[["sensitivity"]] <<- model.sensitivity
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
          current_model[["specificity"]] <<- model.specificity
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
          current_model[["precision"]] <<- model.precision
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
          
          model.fmeasure = (2*model.precision*model.sensitivity)/(model.sensitivity + model.precision)
          model.fmeasure =  round(model.fmeasure, 3)
          current_model[["fmeasure"]] <<- model.fmeasure
          make_value_box(tags$p("F-Measure", style = "font-size: 50%;"), tags$p(model.fmeasure, style = "font-size: 170%;"))
        }, error = function(e){
          make_value_box(tags$p("F-Measure", style = "font-size: 50%;"), "Waiting for model...")
        })
      })
    })
  })
  
  observe({ 
    
    output$wellclassified_prediction <- renderPlot({
      tryCatch({
        nobs <- nrow(data_read())
        ntr <- input$split * nobs
        indices.test <- (ntr+1):nobs
        dftest <- data_read()[indices.test ,c(input$axis_x, input$axis_y, input$class)]
        dftest$prediction = model.prediction()
        dftrue = dftest[dftest$prediction==dftest[,input$class],]
        if (nrow(dftrue)>0) {
          p <- ggplot(dftrue, aes(x=dftrue[, input$axis_x], y=dftrue[, input$axis_y], color = factor(dftrue[, input$class]) )) +
            labs(x = input$axis_x, y = input$axis_y, color = input$class) + ggtitle("Well-classified Predictions")+
            geom_point()
          print(p)}
      }, error = function(e){
        message("Waiting for model...")
      })
    })
  })
    
  observe({ 
    
    output$missclassified_prediction <- renderPlot({
      tryCatch({
        nobs <- nrow(data_read())
        ntr <- input$split * nobs
        indices.test <- (ntr+1):nobs
        dftest <- data_read()[indices.test ,c(input$axis_x, input$axis_y, input$class)]
        dftest$prediction = model.prediction()
        dffalse = dftest[dftest$prediction!=dftest[,input$class],]
        if (nrow(dffalse)>0) {
          p <- ggplot(dffalse, aes(x=dffalse[, input$axis_x], y=dffalse[, input$axis_y], color = factor(dffalse$prediction) )) +
            labs(x = input$axis_x, y = input$axis_y, color = input$class) + ggtitle("Missclassified Predictions")+
            geom_point()
          print(p)}
      }, error = function(e){
        message("Waiting for model...")
      })
    })
  })
  
  
  observe({
    updateSelectInput(session, "dtree_par2vars", choices = input$predictors)
  })
  
  pred_influence <- function(pred, class) {
    do.call("partialPlot", list(x = model(), pred.data = data_read(), 
                                x.var = pred, which.class = class, 
                                main=paste("Partial Dependence on",  pred, "with class", class)))
  }
  
  pred_influence_static <- function() {
    df <- data_read()
    nobs <- nrow(df)
    ntr <- input$split * nobs
    indices.train <- c(1:ntr) # 
    x_train <- df[indices.train, c(input$dtree_par2vars)]

    do.call("partial", list(model(), pred.var = input$dtree_par2vars, 
                            data = x_train, plot = TRUE, rug = TRUE, 
                            chull = TRUE, plot.engine = "ggplot2")) +
      labs(title = paste("Partial Dependence of", input$dtree_par2vars))
  }
  
  predictors_influence <- reactive ({
    data <- data_read()
    vars = c(input$predictors)
    classes = c(unique(data[, input$class]))
    
    op <- par(mfrow=c(length(vars),length(classes)), mar=c(1,1,1,1))
    for (var in vars) {
      for (class in classes) {
        pred_influence(var, class)
      }
    } 
    par(op)
  })
  
  predictors_influence_static <- reactive ({
    #op <- par(mfrow=c(1,1))
    #pred_influence_static()
    #par(op)
    barplot(unlist(model()$variable.importance/sum(model()$variable.importance)))
  })
  
  observe({
    output$influence <- renderPlot ({
        if(input$dtree_type == 'dynamic'){
            predictors_influence()
        } else {
            predictors_influence_static()
        }
    })
  })
  
  observe({
    output$dtree <- renderPlot({
      
      m <- model()
      
      if(input$dtree_type == "static"){
        rpart.plot(m, tweak = input$tweak)
      }else{
        
        if(input$dtree_package == "cforest"){
          nt <- new("BinaryTree")
          nt@tree <- prettytree(cf@ensemble[[1]], names(cf@data@get("input")))
          nt@data <- m@data
          nt@response <- m@responses
          return(plot(nt, type = "simple"))
        }
        
        if(input$dtree_package == "randomForest"){
          # Source: https://shiring.github.io/machine_learning/2017/03/16/rf_plot_ggraph
          tree <- getTree(m, k = as.integer(input$ktree), labelVar = TRUE) %>%
            tibble::rownames_to_column() %>%
            # make leaf split points to NA, so the 0s won't get plotted
            mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
          
          # prepare data frame for graph
          graph_frame <- data.frame(from = rep(tree$rowname, 2),
                                    to = c(tree$`left daughter`, tree$`right daughter`))
          
          # convert to graph and delete the last node that we don't want to plot
          graph <- graph_from_data_frame(graph_frame) %>%
            igraph::delete_vertices("0")
          
          # set node labels
          V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
          V(graph)$leaf_label <- as.character(tree$prediction)
          V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
          
          # plot
          plot <- ggraph(graph, 'dendrogram') + 
            theme_bw() +
            geom_edge_link() +
            geom_node_point() +
            geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
            geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
            geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                            repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
            theme(panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.background = element_blank(),
                  plot.background = element_rect(fill = "white"),
                  panel.border = element_blank(),
                  axis.line = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  plot.title = element_text(size = 18))
          
          return(plot)
        }
      }
    })
  })
  
  # RESULT ====
  
  # ==== COMPARISON
  get_checkpoint_list <- function(){
    #print(length(keys(saved_models)))
    lapply(X = keys(saved_models), function(hashcode) paste(hashcode, "(", saved_models[[hashcode]][["date"]], ")"))
  } 
  
  observeEvent(input$action_save_model, {
    cm <- hash()
    cm[["dtree_type"]] <- input$dtree_type
    cm[["dtree_package"]] <- ifelse(input$dtree_type == 'dynamic', input$dtree_package, input$dtree_package)
    cm[["class"]] <- input$class
    cm[["predictors"]] <- paste(input$predictors, collapse = ";")
    cm[["split"]] <- input$split
    cm[["ntree"]] <- ifelse(input$dtree_type == 'dynamic', input$ntree, input$ntree)
    cm[["mtry"]] <- ifelse(input$dtree_type == 'dynamic', input$mtry, input$mtry)
    cm[["nodesize"]] <- ifelse(input$dtree_package == 'randomForest', input$nodesize, input$nodesize)
    cm[["maxdepth"]] <- ifelse(input$dtree_package == 'cforest', input$maxdepth, input$maxdepth)
    cm[["cp"]] <- ifelse(input$dtree_type == 'static', input$cp, input$cp)
    cm[["accuracyrate"]] <- current_model[["accuracyrate"]]
    cm[["fmeasure"]] <- current_model[["fmeasure"]]        
    cm[["precision"]] <- current_model[["precision"]]  
    cm[["sensitivity"]] <- current_model[["sensitivity"]]       
    cm[["specificity"]] <- current_model[["specificity"]]
    
    ckp_name <- as.character(md5(paste(values(cm), collapse = "")))
    
    cm[["date"]] <- as.character(Sys.time())
  
    if(ckp_name %in% c(keys(saved_models))){
      date <- (saved_models[[ckp_name]])[["date"]]
      session$sendCustomMessage(type = "warning", 
                                message = paste("A model with current parameters is already saved at", date))
      return()
    }
    
    saved_models[[ckp_name]] <<- cm
    choices <- get_checkpoint_list()
    updateSelectInput(session, "model_to_load", choices = choices, selected = head(choices))
    updateSelectInput(session, "checkpoints", choices = choices)
  })
  
  observeEvent(input$action_load_model, {
    ckp <- input$model_to_load
    ckp <- unlist(strsplit(ckp, ' ', fixed = TRUE))[1]
    cm <- saved_models[[ckp]]
    
    updateSelectInput(session, "dtree_type", selected = cm[["dtree_type"]])
    updateSelectInput(session, "dtree_package", selected = cm[["dtree_package"]])
    updateSelectInput(session, "class", selected = cm[["class"]])
    updateSelectInput(session, "predictors", selected = unlist(strsplit(cm[["predictors"]], ';', fixed = TRUE)))
    updateSelectInput(session, "split", selected = cm[["split"]])
    updateSelectInput(session, "ntree", selected = cm[["ntree"]])
    updateSelectInput(session, "mtry", selected = cm[["mtry"]])
    updateSelectInput(session, "nodesize", selected = cm[["nodesize"]])
    updateSelectInput(session, "maxdepth", selected = cm[["maxdepth"]])
    updateSelectInput(session, "cp", selected = cm[["cp"]])
  })

  observeEvent(input$action_delete_model, {
    ckp <- input$model_to_load
    ckp <- unlist(strsplit(ckp, ' ', fixed = TRUE))[1]
    
    if(ckp %in% keys(saved_models)){
      del(ckp, saved_models)
      choices <- get_checkpoint_list()
      updateSelectInput(session, "model_to_load", choices = choices, selected = head(choices))
      updateSelectInput(session, "checkpoints", choices = get_checkpoint_list())
    }
  })
  
  param_list <- function(){
    c("dtree_type", "dtree_package", "class", "predictors", "split", "ntree", "mtry", "nodesize", "maxdepth", "cp")
  }
  
  measure_list <- function(){
    c("accuracyrate", "fmeasure", "precision", "sensitivity", "specificity")
  }
  
  observe({
    updateSelectInput(session, "params", choices = param_list())
  })
  
  observe({
    updateSelectInput(session, "measures", choices = measure_list())
  })
  
  as.data.frame.hash <- function(x) {
    vals <- paste(unlist(values(x)))
    cols <- keys(x) 
    df <- as.data.frame(matrix(data = vals, nrow = 1, ncol = length(cols)))
    colnames(df) <- cols
    df
  }

  observeEvent(input$checkpoints, {
    ckps <- lapply(X = input$checkpoints, function(name) unlist(strsplit(name, ' ', fixed = TRUE))[1])
    ckps <- lapply(X = ckps, function(ckp) saved_models[[ckp]])
    df <- bind_rows(lapply(ckps, function(ckp) as.data.frame.hash(ckp)))
    
    ggvis_measures <- reactive({
      xvar <- prop("x", as.symbol("date"))
      yvar <- prop("y", as.symbol(input$measures))
      plt <- df %>% ggvis(x = xvar, y = yvar)
      
      if(input$comparison_plot_type == "dot"){
        plt <- plt %>% layer_points(size := input_slider(100, 1000, value = 100), fill := "black")
      }
      
      if(input$comparison_plot_type == "line"){
        plt <- plt %>% layer_lines() %>% layer_points(size := input_slider(100, 1000, value = 100), fill := "black")
      }
      
      plt <- plt %>%
        add_tooltip(function(data){
          d <- df[ df$date == data$date, param_list()]
          paste(
            lapply(colnames(d), function(attr) paste0(attr, ": ", as.character(d[[attr]]))),
            collapse = "<br>"
          )
        }, "hover")
      
      plt
    })
    ggvis_measures %>% bind_shiny("ggvis_measures")
  
    ggvis_params <- reactive({
      xvar <- prop("x", as.symbol("date"))
      yvar <- prop("y", as.symbol(input$params))
      plt <- df %>% ggvis(x = xvar, y = yvar)
      
      if(input$comparison_plot_type == "dot"){
        plt <- plt %>% layer_points(size := input_slider(100, 1000, value = 100), fill := "red")
      }
    
      if(input$comparison_plot_type == "line"){
        plt <- plt %>% layer_lines() %>% layer_points(size := input_slider(100, 1000, value = 100), fill := "red")
      }
      
      plt <- plt %>%
        add_tooltip(function(data){
          d <- df[ df$date == data$date, measure_list()]
          paste(
            lapply(colnames(d), function(attr) paste0(attr, ": ", as.character(d[[attr]]))),
            collapse = "<br>"
          )
        }, "hover")
      
      plt
    })
    ggvis_params %>% bind_shiny("ggvis_params")
    
  })
  # COMPARISON ====
}
