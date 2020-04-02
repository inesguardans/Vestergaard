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
ui <- navbarPage(
    "Application",
    tabPanel("General",
             sidebarLayout(
                 
                 sidebarPanel(
                     uiOutput("tex2"),
                     br(),
                     uiOutput("select")
                     
                     
                 ),
                 
                 mainPanel(
                     
                     
                 )
             )))


server <- function(input, output,session) {
    
    output$tex2<-renderUI({
        numericInput("text2","#tests",
                     value = 1,
                     min=1
        )
    })
    output$select<-renderUI({
        selectInput(
            inputId = "sel", 
            label = "Select Test",
            selected = input$text2,
            choices = rep(paste0("Test", 1:input$text2))
        )
    })
    
}
shinyApp(ui = ui, server = server)
