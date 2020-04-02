#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
library(shiny)
ui <- fluidPage(
    numericInput("num", "Maximum slider value", 5),
    uiOutput("slider")
)

server <- function(input, output) {
    output$slider <- renderUI({
        sliderInput("slider", "Slider", min = 0,
                    max = input$num, value = 0)
    })
}

shinyApp(ui = ui, server = server)