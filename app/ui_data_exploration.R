#UI data exploration

ui_data_loading <- sidebarLayout(
  
  sidebarPanel(
    fileInput('file', 'Choose CSV File',
              accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv')),
    hr(),
    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator',
                 c(Comma=',', Semicolon=';',Tab='\t'),','),
    radioButtons('quote', 'Quote',c(None='','Double Quote'='"','Single Quote'="'"), '"'),
    hr()),
  
  mainPanel(
    DT::DTOutput('tbl')
  )
)

ui_data_summary <- sidebarLayout(
  sidebarPanel(),
  mainPanel()
)

ui_data_plot <- sidebarLayout(
  sidebarPanel(),
  mainPanel()
)


ui_data_exploration <- tabItem(
  "data_exploration",
  tabsetPanel(
    tabPanel("Data Loading", ui_data_loading),
    tabPanel("Distribution", ui_data_summary), 
    tabPanel("Plot", ui_data_plot)
  ))