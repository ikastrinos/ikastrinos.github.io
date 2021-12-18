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


ui <- fluidPage(
  
  titlePanel("Education levels based off age and race "),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        inputId ="var1",
        label = "Select a Numeric Variables",
        choices = variables_numeric, selected = "education_num"
      ),
      
      selectInput(
        inputId ="var2",
        label = "Select a Categorical Variables",
        choices = variables_category,
        selected = "race"
      ), 
      
      sliderInput(inputId = "age",
                  "Input an age between 20 and 90 here:",
                  min = min(d$age, na.rm=TRUE),
                  max = max(d$age, na.rm=TRUE),
                  value= c(20, 90))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      
      plotOutput(outputId = 'show_plot')
    )
  )
)

# server is a function! 
server <- function(input, output) {
  
  
  output$show_plot <- renderPlot({
    
    v1 = input$var1
    v2 = input$var2
   
    
    library(ggplot2)
    
    d <- d %>% filter(age>input$age[1], age<input$age[2])
      
    ggplot(d, aes(x = d[[v1]], color = as.factor(d[[v2]])))+
        geom_density()+
        labs(x = v1, color = v2)
      
    
  })
  
}
# app
shinyApp(ui = ui, server = server)