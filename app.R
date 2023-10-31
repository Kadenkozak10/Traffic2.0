#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(readxl)
person <- read_xlsx("person.xlsx")
sex_choices <- unique(person$SEXNAME)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Accident Timing"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
      selectInput("variable", "Variable:",
                  c("HOUR","AGE","MINUTE","MONTHNAME", "INJ_SEVNAME")),
      
      selectInput(
        inputId = "SelectSex",
        label = "Select Sex",
        choices = sex_choices,
        selected = sex_choices[1],
        multiple = TRUE)
    
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  output$distPlot <- renderPlot({
    d <- subset(person, SEXNAME %in% input$SelectSex
                & person$AGE<100 & person$HOUR<25 & INJ_SEVNAME !="Died Prior to Crash*" )
    ggplot(d, aes_string(x = input$variable, fill=input$selectSex)) +
      geom_bar(fill="blue") +
      labs(x = input$variable, y = "Count") +
      theme_minimal()
     
  }, width=900, height=700)
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
