library(shiny)
library(readxl)
library(shinythemes)
library(dplyr)

b_plan <- read_xlsx("B_plan_data.xlsx")
CEO_price <- 2.58
PMI_distrib <- 0.29
lost_vect <- 0.123
usage_vect <- 0.4308
insecticide_eff_vect <- 0.0291
wear_tear_vect <- 0.0276


# Define UI for dataset viewer app ----
ui <- fluidPage(
    theme = shinythemes::shinytheme("flatly"),

    
    # App title ----
    titlePanel("Business Plan"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a where the % of cost of distrib of total budget comes from ----
            selectInput("distrib", "Cost of distribution %",
                        choices = c("PMI", "GF", "AMF", "Other"),
                        selected = "PMI"),
            
            uiOutput("other_distrib"),
            
            #--------------------------------------------
            
            selectInput("price", "Price",
                        choices = c("CEO", "MOP procurement", 
                                    "MOP procurement and distribution"),
                        selected = "CEO"),
            
            #-------------------------------------------
            selectInput("LLIN_lost", "LLIN lost %",
                        choices = c("Vectorworks", "Other"),
                        selected = "Vectorworks"),
            
           
            uiOutput("other_lost"),
            
            #-------------------------------------------
            
            selectInput("not_used", "Not used every night %",
                        choices = c("Vectorworks", "Other"),
                        selected = "Vectorworks"),
            
            uiOutput("other_not_used"),
            #-------------------------------------------
            
            selectInput("insecticide_efficacy", "Min. insecticide efficacy %",
                        choices = c("Vectorworks", "Other"),
                        selected = "Vectorworks"),
            
            uiOutput("other_insecticide"),
            
            #-------------------------------------------
            selectInput("wear_tear", "Wear and tear %",
                        choices = c("Vectorworks", "Other"),
                        selected = "Vectorworks"),
            
            uiOutput("other_wear_tear"),
            
            #-------------------------------------------
            numericInput("improvement", "Loss reduction by %:", 10),
            
            # Include clarifying text ----
            helpText("Note: Please specify the parameters you wish to display"),
            
            
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            tabsetPanel(
                tabPanel("Tab 1",
                         h4("Data"),
                         tableOutput("Bplan")
                         ),
                
                tabPanel("Tab 2", h4("Graphs"))
                )
            
                
        
    
           
        )
        
    )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
    
    
   
        output$other_distrib <- renderUI({
            if(input$distrib == "Other"){
            numericInput("dist_other", "If other, please specify:", 0)
            }
            else{
                return(NULL)
            }
            
        })
        
        
        output$other_lost <- renderUI({
            if(input$LLIN_lost == "Other"){
                numericInput("lost_other", "If other, please specify:", 0)
            }
            else{
                return(NULL)
            }
        })
        
        output$other_not_used <- renderUI({
            if(input$not_used == "Other"){
                numericInput("not_used_other", "If other, please specify:", 0)
            }
            else{
                return(NULL)
            }
        })
        
        output$other_insecticide <- renderUI({
            if(input$insecticide_efficacy == "Other"){
                numericInput("insecticide_other", "If other, please specify:", 0)
            }
            else{
                return(NULL)
            }
        })
        
        output$other_wear_tear <- renderUI({
            if(input$wear_tear == "Other"){
                numericInput("wear_tear_other", "If other, please specify:", 0)
            }
            else{
                return(NULL)
            }
        })
    
      
    
    
    # Return the requested dataset ----
        
        if(input$distrib != "Other"){
            distribInput <- reactive({
                switch(input$distrib,
                       "PMI" = PMI_distrib,
                       "GF" = GF,
                       "AMF" = AMF)
            })
        }
        else{
            distribInput <- input$dist_other
        }
        #-----------------------------------
        
        priceInput <- reactive({
            switch(input$price,
                   "CEO" = CEO_price,
                   "MOP procurement" = MOP_price,
                   "MOP procurement and distrib." = MOP_proc_dist_price)
        })
        
        #---------------------------------
        if(input$LLIN_lost != "Other"){
            lostInput <- lost_vect
        }
        else{
            lostInput <- input$lost_other
        }
        #---------------------------------
        
        if(input$not_used != "Other"){
            useInput <- usage_vect
        }
        else{
            useInput <- input$not_used_other
        }
        #---------------------------------
        
        if(input$insecticide_efficacy != "Other"){
            insecticide_effInput <- insecticide_eff_vect
        }
        else{
            insecticide_effInput <- input$insecticide_other
        }
        #---------------------------------
        
        if(input$wear_tear != "Other"){
            wear_tearInput <- wear_tear_vect
        }
        else{
            wear_tearInput <- input$wear_tear_other
        }
        #-------------------------------
        
        improvement <- input$improvement

       
        
       

    

    # Filter data based on selections
    output$Bplan <- renderTable(b_plan)
        #DT::renderDataTable(DT::datatable({
     
        # if (input$man != "All") {
        #     data <- data[data$manufacturer == input$man,]
        # }
        # if (input$cyl != "All") {
        #     data <- data[data$cyl == input$cyl,]
        # }
        # if (input$trans != "All") {
        #     data <- data[data$trans == input$trans,]
        # }
        #data
    #}))
    
}

# Create Shiny app ----
shinyApp(ui, server)

