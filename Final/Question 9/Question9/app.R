library(tidyverse)
library(shiny)
library(DT)

ui <- fluidPage(
  
  titlePanel("Visualization"),
  
  sidebarLayout(
    
    sidebarPanel(
      fileInput('f1', label = 'Upload data for visualization', accept = '.csv'),
      
      selectInput('v1', label='Select a Numeric Variable', choices=''),
      selectInput('v2', label='Select another numeric Variable', choices=''),
      selectInput('V3', label='Select a Categorical variable', choices='')
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput(outputId = 'show_plot')
    )
  )
)

# server
server <- function(input, output, session) {
  
  myData <- reactive({
    inFile = input$f1
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath, header = TRUE)
    data
  }
  )
  
  output$show_plot <- renderPlot({
    
    inFile = input$f1
    if (!is.null(inFile))
    {
      v1 = input$v1
      d <- read.csv(inFile$datapath, header = TRUE)
      v1 = input$v1
      v2 = input$v2
      v3 = input$v3
      library(ggplot2)
      r = ggplot(d, aes(x = d[[v1]], y=d[[v2]], color = as.factor(d[[v3]])))+
        geom_bar()+
        labs(x = v1, y=v2,color = v3)
      return(r)
    }
    
    else{
      return(NULL)
    }
  })
  
  
  observeEvent(input$f1,{ 
    inFile = input$f1
    data <- read.csv(inFile$datapath, header = TRUE)   
    updateSelectInput(session, 'v1', choices = names(data))}
  )
  
  observeEvent(input$f1,{ 
    inFile = input$f1
    data <- read.csv(inFile$datapath, header = TRUE)   
    updateSelectInput(session, 'v2', choices = names(data))}
  )
  
  observeEvent(input$f1,{ 
    inFile = input$f1
    data <- read.csv(inFile$datapath, header = TRUE)   
    updateSelectInput(session, 'v3', choices = names(data))}
  )
  

  
}


shinyApp(ui = ui, server = server)
