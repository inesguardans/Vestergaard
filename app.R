library(shiny)
library(readxl)
library(shinythemes)
library(dplyr)
library(scales)
library(DT)


b_plan<- read_excel("B_plan_data.xlsx")%>%
    mutate(Amount = round(Amount, 0)
           )%>%
    as.data.frame()
# rownames(b_plan) <- b_plan$Variable
# b_plan[1] <- NULL

CEO_price <- 2.58
procurement_price <- 2.74
distribution_price <- 3.98

PMI_distrib <- 0.29
GF <- 0.35
AMF <- 0.4

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
            numericInput("loss_reduction", "Loss reduction by %:", 20),
            
            numericInput("improvement", "Improvement by %:", 10),
            
            # Include clarifying text ----
            helpText("Note: Please specify the parameters you wish to display"),
            
            
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
       
            tabsetPanel(
                tabPanel("Tab 1",
                         h4("Chosen values"),
                         verbatimTextOutput("summary"),
                         
                         h4("Data"),
                         DT::dataTableOutput("Bplan")
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
            numericInput("dist_other", "If other, please specify %:", 0)
            }
            else{
                return(NULL)
            }
            
        })
        
        
        output$other_lost <- renderUI({
            if(input$LLIN_lost == "Other"){
                numericInput("lost_other", "If other, please specify %:", 0)
            }
            else{
                return(NULL)
            }
        })
        
        output$other_not_used <- renderUI({
            if(input$not_used == "Other"){
                numericInput("not_used_other", "If other, please specify %:", 0)
            }
            else{
                return(NULL)
            }
        })
        
        output$other_insecticide <- renderUI({
            if(input$insecticide_efficacy == "Other"){
                numericInput("insecticide_other", "If other, please specify %:", 0)
            }
            else{
                return(NULL)
            }
        })
        
        output$other_wear_tear <- renderUI({
            if(input$wear_tear == "Other"){
                numericInput("wear_tear_other", "If other, please specify %:", 0)
            }
            else{
                return(NULL)
            }
        })
    
      
    
    
    # Return the requested dataset ----
        
       
        
        distribInput <- reactive({
            if(input$distrib != "Other"){
                switch(input$distrib,
                       "PMI" = PMI_distrib,
                       "GF" = GF,
                       "AMF" = AMF)
                
            }
            else{
                as.numeric(input$dist_other/100) 
            }
        })
        #-----------------------------------
        
        priceInput <- reactive({
            switch(input$price,
                   "CEO" = CEO_price,
                   "MOP procurement" = procurement_price,
                   "MOP procurement and distribution" = distribution_price)
        })
        
        #---------------------------------
        lostInput <- reactive({
            if(input$LLIN_lost != "Other"){
                lostInput <- lost_vect
            }
            else{
                lostInput <- as.numeric(input$lost_other/100)
            }
        })
        
        #---------------------------------
        
        useInput <- reactive({
            if(input$not_used != "Other"){
                useInput <- usage_vect
            }
            else{
                useInput <- as.numeric(input$not_used_other/100)
            }
        })
       
        #---------------------------------
        insecticide_effInput <- reactive({
            if(input$insecticide_efficacy != "Other"){
                insecticide_effInput <- insecticide_eff_vect
            }
            else{
                insecticide_effInput <- as.numeric(input$insecticide_other/100)
            }
        })
        
        #---------------------------------
        wear_tearInput <- reactive({
            if(input$wear_tear != "Other"){
                wear_tearInput <- wear_tear_vect
            }
            else{
                wear_tearInput <- as.numeric(input$wear_tear_other/100)
            }
        })
        
        #-------------------------------
        

    output$summary <- renderPrint({
       choice <- c(distribInput(), priceInput(), lostInput(), useInput(),
          insecticide_effInput(), wear_tearInput(), "", "")
       labels <- c(input$distrib, input$price, input$LLIN_lost, input$not_used, 
                   input$insecticide_efficacy,
                   input$wear_tear, input$loss_reduction, input$improvement)
       
       names <- c("Cost distrib.", "Price", "LLIN lost","Not used",
                  "Insecticide efficacy", "Attrition wear & tear", "Loss reduction %", "Improvement %")
       
       summary <- matrix(labels, nrow = 1, ncol = 8)
       summary <- rbind(summary, choice)
       
       colnames(summary) <- names
       rownames(summary) <- c("", "")
       
       summary

    })
        #-----------------------------------
        data <- reactive({
            
            b_plan[7,2] <- distribInput()
            b_plan[2,3] <- priceInput()
            b_plan[9,2] <- lostInput()
            b_plan[10,2] <- useInput()
            b_plan[11,2] <- insecticide_effInput()
            b_plan[12,2] <- wear_tearInput()
            b_plan[20,2] <- round(input$loss_reduction/100,2)
            b_plan[21,2] <- round(input$loss_reduction/100,2)
            b_plan[22,2] <- round(input$improvement/100, 2)
            
            
            b_plan[3,3] <- round(b_plan[2,3]*b_plan[1,3], 0)
            
            b_plan[6,3] <- round(b_plan[5,2]*b_plan[4,3],0)
            
            b_plan[7,3] <- round((b_plan[3,3]/0.71)*b_plan[7,2],0)
            
            b_plan[8,3] <- round(b_plan[3,3]+b_plan[7,3],0)
            
            b_plan[9,3] <- round(b_plan[9,2]*b_plan[8,3],0)
            
            b_plan[10,3] <- round(b_plan[10,2]*b_plan[9,3],0)
            
            b_plan[11,3] <- round(b_plan[11,2]*b_plan[9,3],0)
            
            b_plan[12,3] <- round(b_plan[12,2]*b_plan[9,3],0)
            
            b_plan[13,3] <- round(b_plan[16,2]*b_plan[1,3],0)
            
            b_plan[14,3] <- round(b_plan[1,3]-b_plan[13,3],0)
            
            b_plan[15,3] <- round(b_plan[15,2]*b_plan[9,3],0)
            
            b_plan[15,2] <- round(b_plan[11,2]+b_plan[12,2],2)
            
            b_plan[16,2] <- round(b_plan[9,2] + b_plan[10,2]+b_plan[15,2],2)
            
            b_plan[16,3] <- round(b_plan[16,2]*b_plan[9,3],0)
            
            b_plan[17,3] <- round(b_plan[1,3]*(1-b_plan[9,2])*(1-b_plan[10,2])*
                                    (1-b_plan[11,2])*(1-b_plan[12,2]),0)
            
            b_plan[17,2] <- round(b_plan[17,3]/b_plan[1,3],2)

            b_plan[18,2] <- b_plan[17,2]

            b_plan[18,3] <- round(b_plan[17,2]*b_plan[8,3],0)

            b_plan[19,3] <- round(b_plan[8,3] - b_plan[18,3],0)

            b_plan[19,2] <- round(b_plan[19,3]/b_plan[8,3],2)

            b_plan[20,3] <- round(b_plan[1,3]*(1-((1-b_plan[20,2])*b_plan[9,2]))*
                                    (1-b_plan[10,2])*(1-b_plan[15,2]),0)

            b_plan[21,3] <- round(b_plan[1,3]*(1-b_plan[9,2])*
                                    (1-(1-b_plan[21,2])*b_plan[10,2])*(1-b_plan[15,2]),0)

            b_plan[22,3] <- round(b_plan[1,3]*(1-b_plan[9,2])*
                                    (1-b_plan[10,2])*(1-0.9*b_plan[15,2]),0)
            
            b_plan[23,3] <- round(b_plan[20,3]+b_plan[21,3]+ b_plan[22,3], 0)

            b_plan$Amount <- sapply(b_plan$Amount, FUN=function(x) prettyNum(x, big.mark=","))
            
            b_plan[c(3:4,6:13,15,16,18),3] <- paste("$", b_plan[c(3:4,6:13,15,16,18),3])
            
            b_plan
        })
        
        
        

    # Filter data based on selections
      
    output$Bplan <- 
      
      DT::renderDataTable({
        df <- data()
        
        if(!is.na(df$Percentage)){
              df$Percentage <- sprintf("%.2f", df$Percentage)
        }
        
        colors <-  c(rep("'palegreen3,'", 8),
                     rep("'skyblue3,'", 8),
                     rep("'linen,'", 6),
                     "linen")

         datatable(df, 
                  rownames = FALSE,
                  options = list(
                    #autoWidth = TRUE,
                    columnDefs = list(list(width = "100px", targets = 2)),
                    pageLength = 23, info = FALSE, lengthMenu = 30,
                    paging = FALSE,
                    searching = FALSE,
                    lengthChange = FALSE
                  ))%>%
           formatStyle("Variable", target="row",
                   backgroundColor =  styleEqual(c(df$Variable[1:8], df$Variable[9:16], df$Variable[17:23]), 
                            c(c("lightgreen", "lightgreen", "lightgreen", "lightgreen"
                                , "lightgreen", "lightgreen", "lightgreen", "lightgreen"), 
                              c("lightblue","lightblue","lightblue","lightblue",
                                "lightblue","lightblue","lightblue","lightblue"),
                              c("orange", "orange","orange","orange","orange","orange","orange"))),
                   fontWeight = styleEqual(c(df$Variable[8], df$Variable[16],df$Variable[23]), c("bold", "bold", "bold")))
        }
        
        
      )

    
}

# Create Shiny app ----
shinyApp(ui, server)

