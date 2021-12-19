library(tidyverse)
library(shiny)

d = read_csv('caffeine.csv')
numeric_variables = d %>% select_if(is.numeric) %>% names()

variables_names = names(d)

ui <- fluidPage(
  
  titlePanel("Caffeine data"),
  
  sidebarLayout(
    
    # Side Panel for reading inputs
    sidebarPanel(
      
      selectInput(
        inputId ="var1",
        label = "Select a variable",
        choices = numeric_variables, selected = "caffeinemg"
      ),
      
      selectInput(
        inputId ="var2",
        label = "Select a variable",
        choices = numeric_variables, selected = "Calories"
      ),
      
      checkboxGroupInput(inputId = "choices", label = "Select type of beverage",
                         choices = c('Tea','Coffee','Energy Drinks'), inline = TRUE),
      
      #dateRangeInput(inputId = "date", 
      #
      #strong("Date range"), 
      #               start = "2020-01-01", end = "2021-12-25",
     #                min = "2020-01-01", max = "2021-12-25"),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput(outputId = 'show_plot')
    )
  )
)

# server is a function! 
server <- function(input, output) {
  
  output$show_plot <- renderPlot({
    d = read_csv('caffeine.csv')
    v1 = input$var1
    v2 = input$var2
    choices = input$type
    print(v1)
    library(ggplot2)
    # r <- d %>% filter(Country %in% country) %>% 
    #   ggplot(aes(x = Date_reported, y = d[[v1]]))+
    #   geom_point()+
    #   labs(x = 'Date', y = d[[v1]])
    
    #d <- d %>% filter(C %in% country, Date_reported>input$date[1],  Date_reported<input$date[2]) 
    
    r <- d %>% ggplot(aes(x = d[[v1]], y = d[[v2]], color = choices))+
      geom_point()+
      labs(x = 'Choice 1', y = v1)
    
    return(r)
    
    
  })
  
}
# app
shinyApp(ui = ui, server = server)