library(tidyverse)
library(shiny)

d = read_csv('adult_modified.csv')
# drop some columns
d <- d %>% select(-occupation, -marital_status, -capital_gain,-capital_loss)

# convert categorical variables to character type
#d <- d %>% mutate_at(c('Survived', 'Pclass'), as.character)

# get variable names
variables_names = names(d)

# get names of numeric variables
variables_numeric = d %>% select_if(is.numeric) %>% names

# get names of categorical variables
variables_category = d %>% select_if(is.character) %>% names

ui <- navbarPage("Navbar!",
           tabPanel("Plot1",
                    
                    sidebarLayout(
                      sidebarPanel(
                        
                        selectInput(
                          inputId ="var1",
                          label = "Select a Numeric Variables",
                          choices = variables_names, selected = "education_num"
                        )
                      ),
                      
                      mainPanel(
                        plotOutput(outputId = 'show_plot')
                      )
                    )
           ),
           
           
           tabPanel("Plot2",
                    
                    sidebarLayout(
                      sidebarPanel(
                        
                        selectInput(
                          inputId ="var2",
                          label = "Select a Categorical Variables",
                          choices = variables_names, selected = "race"
                        )
                      ),
                      
                      mainPanel(
                        plotOutput(outputId = 'show_plot2')
                      )
                    )
           )
)

# server is a function! 
server <- function(input, output) {
  
  
  output$show_plot <- renderPlot({
    
    v1 = input$var1
    
    library(ggplot2)
    
      ggplot(d, aes(x = d[[v1]]))+
        geom_density()+
        labs(x = v1)
    
    
  })
  
  output$show_plot2 <- renderPlot({
    
    v2 = input$var2
    
    library(ggplot2)
    
    ggplot(d, aes(x = d[[v2]]))+
      geom_bar()+
      labs(x = v2)
    
    
  })
  
}
# app
shinyApp(ui = ui, server = server)