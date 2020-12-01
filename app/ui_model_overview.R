library("DT")
#UI model Overview - ...

ui_model_correlation <- sidebarLayout(
  sidebarPanel(
    selectInput("corr_method", "Select method : ", choices = c("pearson", "kendall", "spearman")),
    sliderInput('corr_precision','Precision',min=0,max=10,value=2,step=1),
    selectInput("corr_use", "NA Action",
                c("everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs")),
    #Only works if we are not showing confidence interval
    conditionalPanel("!input.corr_showConf",
                     selectInput("corr_plotMethod", "Plot Method",
                                 list("mixed", all = eval(formals(corrplot)$method)), "circle"),
                     conditionalPanel("input.corr_plotMethod === 'mixed'",
                                      wellPanel(
                                        selectInput("corr_plotLower", "Lower Method", eval(formals(corrplot)$method)),
                                        selectInput("corr_plotUpper", "Upper Method", eval(formals(corrplot)$method)))
                     )
    ),
    conditionalPanel("input.corr_showConf || input.corr_plotMethod !== 'mixed'",
                     selectInput("corr_plotType", "Plot Type",
                                 eval(formals(corrplot)$type))),
    
    selectInput("corr_plotOrder", "Reorder Correlation", eval(formals(corrplot)$order)),
    conditionalPanel("input.corr_plotOrder === 'hclust'",
                     wellPanel(
                       selectInput("corr_plotHclustMethod", "Method",
                                   eval(formals(corrplot)$hclust.method)),
                       numericInput("corr_plotHclustAddrect", "Number of Rectangles", 3, 0, NA))),
    
    tags$hr(),
    checkboxInput("corr_sigTest", "Significance Test"),
    conditionalPanel("input.corr_sigTest",
                     numericInput("corr_sigLevel", "Significane Level",
                                  0.05, 0, 1, 0.01),
                     selectInput("corr_sigAction", "Insignificant Action",
                                 eval(formals(corrplot)$insig))),
    checkboxInput("corr_showConf", "Show Confidence Interval"),
    conditionalPanel("input.corr_showConf",
                     selectInput("corr_confPlot", "Ploting Method",
                                 eval(formals(corrplot)$plotCI)[-1]),
                     numericInput("corr_confLevel", "Confidence Level",
                                  0.95, 0, 1, 0.01)),
    tags$hr(),
    helpText("Choose the variables to display. Drag and drop to reorder."), 
    selectizeInput("variables", "", c("Loading..."), multiple = T, options = list(plugins = list("remove_button")))
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Correlation Plot", 
        plotOutput("ui_corr_plot", height = 600),
        verbatimTextOutput("ui_submit_cor_heatmap_err")
      ),
      tabPanel("Correlation Table",
        DT::dataTableOutput("ui_corr_tab"),
        verbatimTextOutput("ui_corr_tab_err"),
      )
    )
  )
)
  
ui_model_predictors <- sidebarLayout(
  sidebarPanel(
    selectizeInput("ui_predictors", "Select predictors", c("Loading..."), multiple = T, options = list(plugins = list("remove_button")))
  ),
  mainPanel(
    DT::dataTableOutput("predictors_summary")
  )
)

ui_model_overview <- tabItem('model_overview', tabsetPanel(
  tabPanel("Correlation", ui_model_correlation),
  tabPanel("Predictors overview", ui_model_predictors)
))