

library(shiny)
library(readxl)
library(shinythemes)
library(dplyr)
library(scales)
library(DT)
library(htmlwidgets)
library(htmltools)



input1 <- 0.1
input2 <- 0.2
input3 <- 0.3




# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  
  theme = shinythemes::shinytheme("flatly"),
  
  
  # App title ----
  titlePanel("Business Plan"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      

      
      #--------------------------------------------
      

      
      #-------------------------------------------
      selectInput("input1", "A",
                  choices = c("input1", "Other"),
                  selected = "input1"),
      
      
      uiOutput("other_input1"),
      
      #-------------------------------------------
      
      selectInput("input2", "B",
                  choices = c("input2", "Other"),
                  selected = "input2"),
      
      uiOutput("other_input2"),
      #-------------------------------------------
      
      selectInput("input3", "C",
                  choices = c("input3", "Other"),
                  selected = "input3"),
      
      uiOutput("other_input3"),
      
      #-------------------------------------------

      
      # Include clarifying text ----
      helpText("Note: Please specify the parameters you wish to display"),
      
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      tabsetPanel(
        tabPanel("Tab 1",
              
                 
                 h4("Data"),
                 DT::dataTableOutput("Table")
        )
      )
      
      
      
      
      
    )
    
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  
  
  output$other_input1 <- renderUI({
    if(input$input1 == "Other"){
      numericInput("A_other", "If other, please specify %:", 0)
    }
    else{
      return(NULL)
    }
    
  })
  
  
  output$other_input2 <- renderUI({
    if(input$input2 == "Other"){
      numericInput("B_other", "If other, please specify %:", 0)
    }
    else{
      return(NULL)
    }
  })
  
  output$other_input3<- renderUI({
    if(input$input3== "Other"){
      numericInput("C_other", "If other, please specify %:", 0)
    }
    else{
      return(NULL)
    }
  })
  
  
  
  
  # Return the requested dataset ----
  
  
  
  #---------------------------------
  Input1 <- reactive({
    if(input$input1 != "Other"){
      Input1 <- input1
    }
    else{
      Input1 <- as.numeric(input$A_other/100)
    }
  })
  
  #---------------------------------
  
  Input2 <- reactive({
    if(input$input2 != "Other"){
      Input2 <- input2
    }
    else{
      Input2 <- as.numeric(input$B_other/100)
    }
  })
  
  #---------------------------------
  Input3 <- reactive({
    if(input$input3 != "Other"){
      Input3 <- input3
    }
    else{
      Input3 <- as.numeric(input$C_other/100)
    }
  })

  #-----------------------------------
  data <- reactive({
    
    mtcars[2,2] <- Input1()
    mtcars[3,2] <- Input2()
    mtcars[4,2] <- Input3()

    
    mtcars
  })

  # Filter data based on selections
  
  output$Table <- 
    
    DT::renderDataTable({
      df <- data()
      
      datatable(df,
                rownames = TRUE,
                options = list(
                  autoWidth = TRUE,
                  pageLength = 23, info = FALSE
                ))%>%
        formatStyle("hp", target="row",
                    fontWeight = styleEqual(c(hp$mpg[2], hp$mpg[3]), c("bold", "bold")),
                    backgroundColor = styleEqual(c(hp$mpg[2], hp$mpg[3]), c("red", "red"))) #this is not working
      

    })
  
}

# Create Shiny app ----
shinyApp(ui, server)

