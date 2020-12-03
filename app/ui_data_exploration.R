#UI data exploration: Juliana METIVIER et Minh-Hoang DANG

ui_data_loading <- sidebarLayout(
  
  sidebarPanel(
    fileInput('file', 'Choose CSV File',
              accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv')),
    hr(),
    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator',
                 c(Comma=',', Semicolon=';',Tab='\t'),','),
    radioButtons('quote', 'Quote',c(None='','Double Quote'='"','Single Quote'="'"), '"'),
    hr(),
    helpText("Choose the variables to display. Drag and drop to reorder."),
    selectizeInput("data_cols", "", c("Loading..."), multiple = T, options = list(plugins = list("remove_button")))
  ),
  
  mainPanel(
    DT::dataTableOutput('tbl')
  )
)

ui_data_summary <- sidebarLayout(
  sidebarPanel(),
  mainPanel()
)

ui_data_plot <- sidebarLayout(
  sidebarPanel(
    # helpText("Choose plot types. Drag and drop to reorder."),
    # selectizeInput("plot_types", "", c("boxplot", "histogram"), multiple = T, options = list(plugins = list("remove_button"))),
    selectInput("plot_type", "Plot type", c("boxplot", "histogram", "scatter")),
    
    conditionalPanel("input.plot_type == 'boxplot'",
                     helpText("Choose the variable to plot. Drag and drop to reorder."),
                     selectizeInput("plot_box_vars", "", c("Loading..."), multiple = T, 
                                    options = list(plugins = list("remove_button", "drag_drop")))
    ),
    conditionalPanel("input.plot_type == 'histogram'",
                     helpText("Choose the variable to plot. Drag and drop to reorder."),
                     selectInput("plot_hist_var", "Variable", c("Loading...")),
                     selectInput("plot_hist_freq", "Frequency/Density", c("Frequency", "Density"))
    ),
    conditionalPanel("input.plot_type == 'scatter'",
                     helpText("Choose the variable to plot. Drag and drop to reorder."),
                     selectInput("plot_scatter_x", "x-axis", c("Loading...")),
                     selectInput("plot_scatter_y", "y-axis", c("Loading..."))
    )
  ),
  mainPanel(
    plotOutput("plot_out")
  )
)


ui_data_exploration <- tabItem(
  "data_exploration",
  tabsetPanel(
    tabPanel("Data Loading", ui_data_loading),
    tabPanel("Distribution", ui_data_summary), 
    tabPanel("Plot", ui_data_plot)
  ))