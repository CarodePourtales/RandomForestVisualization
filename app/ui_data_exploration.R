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
  sidebarPanel(
    selectInput("distrib_type", "Plot type", c("boxplot", "histogram")),
    
    conditionalPanel("input.distrib_type == 'boxplot'",
                     selectInput("distrib_box_form", "Form", c("box", "violin")),
                     helpText("Choose the variable to plot. Drag and drop to reorder."),
                     selectizeInput("distrib_box_vars", "", c("Loading..."), multiple = T, 
                                    options = list(plugins = list("remove_button", "drag_drop")))
    ),
    conditionalPanel("input.distrib_type == 'histogram'",
                     helpText("Choose the variable to plot. Drag and drop to reorder."),
                     selectizeInput("distrib_hist_vars", "", c("Loading..."), multiple = T, 
                                    options = list(plugins = list("remove_button", "drag_drop"))),
                     selectInput("distrib_hist_freq", "Frequency/Density", c("Frequency", "Density")),
                     selectInput("distrib_hist_space", "Continuous/Discrete", c("Continous", "Discrete"))
    )
  ),
  mainPanel(
    plotOutput("distrib_out")
  )
)

ui_data_plot <- sidebarLayout(
  sidebarPanel(
    selectInput("plot_type", "Plot type", c("Scatter", "2D Histogram", "Hexbin", "2D Density")),
    
    selectInput("plot_scatter_x", "x-axis", c("Loading...")),
    selectInput("plot_scatter_y", "y-axis", c("Loading...")),
    
    conditionalPanel("input.plot_type == 'Scatter'",
                     selectInput("plot_scatter_clr", "Color by variable", c("Loading...")),
    ),
    conditionalPanel("input.plot_type == '2D Density'",
                     selectInput("plot_density_geom", "Geometry", c("polygon", "raster"))
    ),
    conditionalPanel("input.plot_type == 'Hexbin' || input.plot_type == '2D Histogram'",
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