library(shiny)
library(readxl)
library(shinythemes)
library(tidyr)
library(dplyr)
library(scales)
library(DT)
library(plotly)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(foreign)

b_plan<- read_excel("B_plan_data.xlsx", )%>%
    mutate(Amount = round(Amount, 0)
           )%>%
    as.data.frame()


CEO_price <- 2.58
procurement_price <- 2.74
distribution_price <- 3.98

PMI_distrib <- 0.29
GF <- 0.15
AMF <- 0.04

lost_vect <- 0.123
usage_vect <- 0.4308
insecticide_eff_vect <- 0.0291
wear_tear_vect <- 0.0276

mycurrency <- function(x){
  return(paste("$", formatC(as.numeric(x), format="f", digits=0, big.mark=",")))
}


# Define UI for dataset viewer app ----
ui <- fluidPage(
  

    theme = shinythemes::shinytheme("flatly"),

    
    # App title ----
    titlePanel("Business Plan"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(


          style = "position: fixed; height: 90vh; overflow-y: auto;",
            
            # Input: Select a where the % of cost of distrib of total budget comes from ----
            selectInput("distrib", "Cost of distribution %",
                        choices = c("PMI; 29%", "GF; 15%", "AMF; 4%", "Other"),
                        selected = "PMI; 29%"),
            
            uiOutput("other_distrib"),
            
            #--------------------------------------------
            
            selectInput("price", "Price",
                        choices = c("Internal pricing; 2.58", "MOP procurement; 2.74", 
                                    "MOP procurement and distribution; 3.98"),
                        selected = "CEO; 2.58"),
            
            #-------------------------------------------
            selectInput("LLIN_lost", "LLIN lost %",
                        choices = c("Vectorworks; 12.3%", "Other"),
                        selected = "Vectorworks; 12.3%"),
            
           
            uiOutput("other_lost"),
            
            #-------------------------------------------
            
            selectInput("not_used", "Not used every night %",
                        choices = c("Vectorworks; 43.08%", "Other"),
                        selected = "Vectorworks; 43.08%"),
            
            uiOutput("other_not_used"),
            #-------------------------------------------
            
            selectInput("insecticide_efficacy", "Min. insecticide efficacy %",
                        choices = c("Vectorworks; 2.91%", "Other"),
                        selected = "Vectorworks; 2.91%"),
            
            uiOutput("other_insecticide"),
            
            #-------------------------------------------
            selectInput("wear_tear", "Wear and tear attrition %",
                        choices = c("Vectorworks; 2.76%", "Other"),
                        selected = "Vectorworks; 2.76%"),
            
            uiOutput("other_wear_tear"),
            
            #-------------------------------------------
            numericInput("loss_reduction", "Loss reduction by %:", 20),
            
            numericInput("usage_improvement", "Usage improvement by %:", 20),
            
            numericInput("bioefficacy_improvement", "Bioefficacy improvement by %:", 5),
            
            numericInput("wear_tear_improvement", "Wear and tear improvement by %:", 5),
            
            numericInput("effectiveness_improvement", "Effectiveness improvement by %:", 10),
            
            numericInput("coverage_improvement", "Regional coverage improvement by %:", 5),
            
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
                         DT::dataTableOutput("Bplan"),
                         br(),
                         br(),
                         br()
                         ),
                
                tabPanel("Tab 2", h4("Graphs"),
                         verbatimTextOutput("summary2"),
                         br(),
                         h3("Model assumptions and parameters"),
                         br(),
                         br(),
                         plotlyOutput("global_market"),
                         br(),
                         br(),
                         br(),
                         br(),
                         plotlyOutput("parameters"),
                         br(),
                         br(),
                         br(),
                         br(),
                         plotlyOutput("actual"),
                         br(),
                         br(),
                         br(),
                         br(),
                         h3("Improve efficiency"),
                         br(),
                         br(),
                         br(),
                         plotlyOutput("loss_usage"),
                         br(),
                         br(),
                         br(),
                         br(),
                         plotlyOutput("insecticide_wear"),
                         br(),
                         br(),
                         br(),
                         br(),
                         plotlyOutput("effectiveness_geo"),
                         br(),
                         br(),
                         br(),
                         br(),
                         plotlyOutput("value"),
                         br(),
                         br(),
                         br(),
                         br(),
                         plotlyOutput("differences"),
                         br(),
                         br(),
                         br(),
                         h3("Health impact"),
                         br(),
                         htmlOutput("cochrane"),
                         br(),
                         uiOutput("formulas"),
                         br(),
                         br(),
                         plotlyOutput("health_loss_usage"),
                         br(),
                         br(),
                         br(),
                         br(),
                         plotlyOutput("health_loss_usage2"),
                         br(),
                         br(),
                         br(),
                         br(),
                         plotlyOutput("health_insecticide_wear"),
                         br(),
                         br(),
                         br(),
                         br(),
                         plotlyOutput("health_insecticide_wear2"),
                         br(),
                         br(),
                         br(),
                         br(),
                         plotlyOutput("health_effectiveness_geo"),
                         br(),
                         br(),
                         br(),
                         br(),
                         plotlyOutput("health_effectiveness_geo2")
                )
            
                
          
    
           
        )
        
    )
))

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
                       "PMI; 29%" = PMI_distrib,
                       "GF; 15%" = GF,
                       "AMF; 40%" = AMF)
                
            }
            else{
                as.numeric(input$dist_other/100) 
            }
        })
        #-----------------------------------
        
        priceInput <- reactive({
            switch(input$price,
                   "Internal pricing; 2.58" = CEO_price,
                   "MOP procurement; 2.74" = procurement_price,
                   "MOP procurement and distribution; 3.98" = distribution_price)
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
       choice <- c(distribInput(), priceInput(), input$LLIN_lost, input$not_used, 
                   input$insecticide_efficacy,
                   input$wear_tear, input$loss_reduction, input$usage_improvement, input$bioefficacy_improvement,
                   input$wear_tear_improvement, input$effectiveness_improvement, input$coverage_improvement)
       
       # labels <- c(input$distrib, input$price, input$LLIN_lost, input$not_used, 
       #             input$insecticide_efficacy,
       #             input$wear_tear, input$loss_reduction, input$usage_improvement, input$bioefficacy_improvement,
       #             input$wear_tear_improvement, input$effectiveness_improvement, input$coverage_improvement
       #              )
       
       names <- c("Cost distrib.", "Price", "LLIN lost","Not used",
                  "Insecticide efficacy", "Wear & tear", "Loss reduction %", "Usage improvement %",
                  "Bioefficacy improvement %", "Wear and tear improvement", "Effectiveness improvement %", "Regional coverage improvement %")
       
       summary <- matrix(choice, nrow = 1, ncol = 12)
       #summary <- rbind(summary, choice)
       
       colnames(summary) <- names
       rownames(summary) <- c("")
       
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
            b_plan[25,2] <- round(input$usage_improvement/100,2)
            b_plan[30,2] <- round(input$bioefficacy_improvement/100,2)
            b_plan[35,2] <- round(input$wear_tear_improvement/100,2)
            b_plan[40,2] <- round(input$effectiveness_improvement/100,2)
            b_plan[45,2] <- round(input$coverage_improvement/100, 2)
            
            
            b_plan[3,3] <- round(b_plan[2,3]*b_plan[1,3], 0)
            
            b_plan[3,2] <- 1- distribInput()
            
            b_plan[7,3] <- round((b_plan[3,3]/b_plan[3,2])*b_plan[7,2],0)
            
            b_plan[8,3] <- round(b_plan[3,3]+b_plan[7,3],0)
            
            b_plan[8,2] <- round(b_plan[7,2]+b_plan[3,2],0)
            
            b_plan[9,3] <- round(b_plan[9,2]*b_plan[8,3],0)
            
            b_plan[10,3] <- round(b_plan[10,2]*b_plan[8,3],0)
            
            b_plan[11,3] <- round(b_plan[11,2]*b_plan[8,3],0)
            
            b_plan[12,3] <- round(b_plan[12,2]*b_plan[8,3],0)
            
            b_plan[13,3] <- round(b_plan[16,2]*b_plan[1,3],0)
            
            b_plan[14,3] <- round(b_plan[1,3]-b_plan[13,3],0)
            
            b_plan[15,3] <- round(b_plan[15,2]*b_plan[8,3],0)
            
            b_plan[15,2] <- round(b_plan[11,2]+b_plan[12,2],4)
            
            b_plan[16,2] <- round(b_plan[9,2] + b_plan[10,2]+b_plan[15,2],4)
            
            b_plan[16,3] <- round(b_plan[16,2]*b_plan[8,3],0)
            
            b_plan[17,3] <- round((1-b_plan[16,2])*b_plan[1,3],0)

            b_plan[18,2] <- round(b_plan[17,3]/b_plan[1,3],4)

            b_plan[18,3] <- round(b_plan[18,2]*b_plan[8,3],0)
            
            b_plan[19,3] <- round(b_plan[8,3] - b_plan[18,3],0)
            
            b_plan[19,2] <- round(b_plan[19,3]/b_plan[8,3],4)
            
            #Loss ----------------------------------------------------------------------
            b_plan[20,3] <- round(b_plan[1,3]*(1-((1-b_plan[20,2])*b_plan[9,2]+
                                                 b_plan[10,2]+b_plan[11,2]+ b_plan[12,2])),0)
            
            b_plan[21,2] <- round(b_plan[20,3]/b_plan[1,3], 4)
            
            b_plan[21,3] <- round(b_plan[21,2]*b_plan[8,3],0)
            
            b_plan[22,2] <- round(1-b_plan[21,2], 4)
            
            b_plan[22,3] <- round(b_plan[8,3]-b_plan[21,3])
            
            b_plan[23,3] <- round((b_plan[20,3]-b_plan[17,3])*2/1000*5.6, 0)
            
            b_plan[24,3] <- round((b_plan[20,3]-b_plan[17,3])*2/1000*128, 0)

            #Usage  ------------------------------------------------------------------------
            b_plan[25,3] <- round(b_plan[1,3]*(1-(b_plan[9,2]+
                                                    (1-b_plan[25,2])*b_plan[10,2]+b_plan[11,2]+b_plan[12,2])),0)
            
            b_plan[26,2] <- round(b_plan[25,3]/b_plan[1,3], 4)
            
            b_plan[26,3] <- round(b_plan[26,2]*b_plan[8,3],0)
            
            b_plan[27,2] <- round(1-b_plan[26,2], 4)
            
            b_plan[27,3] <- round(b_plan[8,3]-b_plan[26,3],0)
            
            b_plan[28,3] <- round((b_plan[25,3]-b_plan[17,3])*2/1000*5.6, 0)
            
            b_plan[29,3] <- round((b_plan[25,3]-b_plan[17,3])*2/1000*128, 0)
            
            #Bioefficacy -----------------------------------------------------------------------
            b_plan[30,3] <- round(b_plan[1,3]*(1-(b_plan[9,2]+
                                                    b_plan[10,2]+(1-b_plan[30,2])*b_plan[11,2]+b_plan[12,2])),0)
            
            b_plan[31,2] <- round(b_plan[30,3]/b_plan[1,3], 4)
            
            b_plan[31,3] <- round(b_plan[31,2]*b_plan[8,3],0)
            
            b_plan[32,2] <- round(1-b_plan[31,2], 4)
            
            b_plan[32,3] <- round(b_plan[8,3]-b_plan[31,3],0)
            
            b_plan[33,3] <- round((b_plan[30,3]-b_plan[17,3])*2/1000*5.6, 0)
            
            b_plan[34,3] <- round((b_plan[30,3]-b_plan[17,3])*2/1000*128, 0)
            
            #Wear tear----------------------------------------------------------------------
            b_plan[35,3] <- round(b_plan[1,3]*(1-(b_plan[9,2]+
                                                    b_plan[10,2]+b_plan[11,2]+(1-b_plan[35,2])*b_plan[12,2])),0)
            
            b_plan[36,2] <- round(b_plan[35,3]/b_plan[1,3], 4)
            
            b_plan[36,3] <- round(b_plan[36,2]*b_plan[8,3],0)
            
            b_plan[37,2] <- round(1-b_plan[36,2], 4)
            
            b_plan[37,3] <- round(b_plan[8,3]-b_plan[36,3],0)
            
            b_plan[38,3] <- round((b_plan[35,3]-b_plan[17,3])*2/1000*5.6, 0)
            
            b_plan[39,3] <- round((b_plan[35,3]-b_plan[17,3])*2/1000*128, 0)
            #Effectiveness -------------------------------------------------------------------
            b_plan[40,3] <- round(b_plan[1,3]*(1-(b_plan[9,2]+
                                                    b_plan[10,2]+(1-b_plan[40,2])*(b_plan[11,2]+b_plan[12,2]))),0)
            
            b_plan[41,2] <- round(b_plan[40,3]/b_plan[1,3], 4)
            
            b_plan[41,3] <- round(b_plan[41,2]*b_plan[8,3],0)
            
            b_plan[42,2] <- round(1-b_plan[41,2], 4)
            
            b_plan[42,3] <- round(b_plan[8,3]-b_plan[41,3],0)
            
            b_plan[43,3] <- round((b_plan[40,3]-b_plan[17,3])*2/1000*5.6, 0)
            
            b_plan[44,3] <- round((b_plan[40,3]-b_plan[17,3])*2/1000*128, 0)
            
            #Coverage-----------------------------------------------------------------
            b_plan[45,3] <- round(b_plan[45,2]*b_plan[1,3], 0)
            
            b_plan[46,3] <- round(b_plan[45,2]*b_plan[8,3],0)
           

            b_plan$Amount <- sapply(b_plan$Amount, FUN=function(x) prettyNum(x, big.mark=","))
            
            b_plan[c(3:4,6:13,15,16,18,19,21,22,26,27,31,32,36,37, 41, 42, 46),3] <- paste("$", b_plan[c(3:4,6:13,15,16,18,19,21,22,26,27,31,32,36,37, 41, 42, 46),3])
            
            b_plan
        })
        

    # Filter data based on selections
      
    output$Bplan <- 
      
      DT::renderDataTable({
        df <- data()
    colors <- c("lightgreen", "lightgreen", "lightgreen", "lightgreen"
                  , "lightgreen", "lightgreen", "lightgreen", "lightgreen",
                "lightblue","lightblue","lightblue","lightblue",
                  "lightblue","lightblue","lightblue","lightblue",
                "#F46D43","#F46D43","#F46D43",
                "#D94801", "#D94801","#D94801","#D94801","#D94801",
                "#F16913","#F16913","#F16913","#F16913","#F16913",
                "#FD8D3C", "#FD8D3C", "#FD8D3C", "#FD8D3C", "#FD8D3C", 
                "#FDAE6B","#FDAE6B","#FDAE6B", "#FDAE6B","#FDAE6B",
                "#FDD0A2","#FDD0A2","#FDD0A2","#FDD0A2","#FDD0A2",
                "#FFF5EB","#FFF5EB")
         datatable(df, 
                  rownames = TRUE,
                  options = list(
                    autoWidth = TRUE,
                    columnDefs = list(list(width = "250px", targets = c(1,2,3)), list(className = "dt-center", targets = c(2,3))),
                    pageLength = 23, info = FALSE, lengthMenu = 30,
                    paging = FALSE,
                    searching = FALSE,
                    lengthChange = FALSE,
                    ordering = FALSE
                    
                  ))%>%
           formatPercentage("Percentage", 2)%>% 
           formatStyle(columns = "Amount", target="row", valueColumns = 0,
                       backgroundColor =  styleEqual(seq_len(nrow(df)), colors)
                       )
         

        })
    #--------------------------------------------------------------------------------------------------------------------------------
    output$summary2 <- renderPrint({
      choice <- c(distribInput(), priceInput(), input$LLIN_lost, input$not_used, 
                  input$insecticide_efficacy,
                  input$wear_tear, input$loss_reduction, input$usage_improvement, input$bioefficacy_improvement,
                  input$wear_tear_improvement, input$effectiveness_improvement, input$coverage_improvement)

      names <- c("Cost distrib.", "Price", "LLIN lost","Not used",
                 "Insecticide efficacy", "Wear & tear", "Loss reduction %", "Usage improvement %",
                 "Bioefficacy improvement %", "Wear and tear improvement", "Effectiveness improvement %", "Regional coverage improvement %")
      
      summary <- matrix(choice, nrow = 1, ncol = 12)
      #summary <- rbind(summary, choice)
      
      colnames(summary) <- names
      rownames(summary) <- c("")
      
      summary
      
    })
    
    #################### PARAMETERS ######################################
    output$global_market <- renderPlotly({
      df <- b_plan %>%
        filter(Variable %in% c("Market size", "Cost of distribution", "Total Annual Cost"))%>%
        rbind(c("Market size", 1-GF, 545148835),
              c("Cost of distribution", GF, round(545148835.84*GF/(1-GF),0)),
              c("Total Annual Cost", 1, round(545148835.84+545148835.84*GF/(1-GF),0)),
              c("Market size", 1-AMF, 545148835),
              c("Cost of distribution", AMF, round(545148835.84*AMF/(1-AMF),0)),
              c("Total Annual Cost", 1, round(545148835.84+545148835.84*AMF/(1-AMF),0)))%>%
        mutate(Source = c("PMI", "PMI","PMI","GF", "GF", "GF", "AMF", "AMF", "AMF"),
               Percentage = as.numeric(Percentage),
               Amount = as.numeric(Amount))
      

      p1 <- df %>%
        ggplot(aes(x = Variable, y = Percentage*100, fill=Source,  text = paste("Percentage:", Percentage*100, "%"))) +
        geom_bar(stat = "identity", position = position_dodge(), width = 0.6)+
        #guides(fill = FALSE)+
        scale_y_continuous(expand = c(0,0),
                           limits = c(0,100))+
        scale_fill_manual(values =  c("#C7E9C0" ,"#74C476" ,"#41AB5D"))+
        labs(x = "Percentage of total budget", y = "")+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              text = element_text(size = 10), axis.text.x = element_text(angle = 45), axis.text.y = element_text(size = 8),
              plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10),
              axis.title.y = element_text(size = 10))
      
      ply1 <- ggplotly(p1, tooltip = c("text"), showlegend=FALSE)
      
      p2 <- df %>%
        ggplot(aes(x = Variable, y = mycurrency(Amount) ,fill = Source, text = paste("Value:", mycurrency(Amount)))) +
        geom_bar(stat = "identity", position = position_dodge(),  width = 0.6)+
        #guides(fill = FALSE)+
        scale_fill_manual(values = c("#DADAEB", "#BCBDDC" ,"#9E9AC8"))+
        ggtitle("Global Market: 211,297,998 LLINs")+ labs(x = "Budgets", y = "")+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              text = element_text(size = 10), axis.text.x = element_text(angle = 45), axis.text.y = element_text(size = 8),
              plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10),
              axis.title.y = element_text(size = 10))
      
      ply2 <- ggplotly(p2, tooltip = c("text"))
      
      
      plot <- plotly::subplot(ply1, ply2, shareX=TRUE, titleX = TRUE, margin = 0.07)
   
      plot
      
    })
    #---------------------------------------------------------------------------------------
    output$parameters <- renderPlotly({
      options(scipen = 1000000)
      df <- data()
      df1 <- df %>%
        filter(Variable %in% c("LLIN lost", "Not used every night", 
                               "Minimal insecticide efficacy loss", "Wear and tear loss"))
      
      p1 <- ggplot(df1, aes(x = Variable, y = Percentage*100, text = paste("Percentage:", Percentage*100, "%"))) +
        geom_bar(stat = "identity", fill = "cadetblue3", width = 0.5)+
        scale_y_continuous(expand = c(0,0),
                           limits = c(0,100))+
        labs(x = "Parameters (%)")+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              text = element_text(size = 10), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
              axis.title.x = element_text(size = 10))
      
      ply1 <- ggplotly(p1, tooltip = c("text"))
     
      
      df2 <- df %>%
        filter(Variable %in% c("LLIN lost", "Not used every night", 
                               "Minimal insecticide efficacy loss", "Wear and tear loss"))
      p2<- ggplot(df2, aes(x = Variable, y = Amount, text = paste("Value:", Amount))) +
        geom_bar( stat = "identity", fill = "darkslategray4", width = 0.5)+
        ggtitle("Parameters")+ labs(x = "Lost value per parameter ($)")+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              text = element_text(size = 10), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
              plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10))
      
      ply2 <- ggplotly(p2, tooltip = c("text"))
      
      
      plot <- plotly::subplot(ply1, ply2, titleX = TRUE, margin = 0.07)
  
      plot
      
        
    })
    
    #------------------------------------------------------------------------
    output$actual <- renderPlotly({
      df <- data()
      LLINs <- df[17,3]
      df1 <- df %>%
        filter(Variable %in% c("Total Annual Cost", "Actual value of LLINs purchased", "Lost value (all nets)"))
      
      
      p <-  ggplot(df1, aes(x = Variable, y = Amount, text = paste("Percentage:", Percentage*100, "%"))) +
        geom_bar( stat = "identity", fill = "#1B7837", width = 0.5)+
        ggtitle(paste("LLINs efficiently used:", LLINs))+
        labs(x = "Total degradation lost value ($)", y="")+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              text = element_text(size = 10), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
              plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10))
      
      ply <- ggplotly(p, tooltip = c("text"))
      
      ply
    })
   #---------------------------------------------------------------------------------------------
    #################### IMPROVEMENTS ######################################
    output$loss_usage <- renderPlotly({
      df <- data()

      reduction_loss <- df[20,2]*100
      improve_usage <- df[25,2]*100
      df1 <- df[21:22,]
      df1$Lost_value <- "After loss reduction"
      
      total <- df[19,] %>% mutate(Variable = case_when(Variable == "Lost value (all nets)" ~ "Initial lost value (all nets)"))
      total$Lost_value <- "Lost value withouth improvements"
      
      df3 <- rbind(df1, total)
      df3$lab <- paste("Reduce LLIN loss by:", reduction_loss, "%")
      #-----------------------------------------------------------------
      df2 <- df[26:27,]
      df2$Lost_value <- "After usage improvement"
    
      df4 <- rbind(df2, total)
      df4$lab <- paste("Improve LLIN usage by:", improve_usage, "%")
      

      
      p1 <- ggplot(df3, aes(x = Variable, y = Amount,  fill = Lost_value, text = paste("Percentage:", Percentage*100, "%", "\n Amount: ", Amount))) +
        geom_bar(stat = "identity", position = "dodge")+
        guides(fill = FALSE)+
        facet_wrap(~lab)+
        labs(y = "Value ($)")+
        scale_fill_manual(values=c("#99D8C9","#FBB4AE"))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              legend.position = "none",
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              text = element_text(size = 10), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
              plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background =element_rect(fill="white"),
              strip.text = element_text(size = 12, face = "bold", hjust = 0.5))
      
      ply1 <- ggplotly(p1, tooltip = c("text"))
      
      p2 <- ggplot(df4, aes(x = Variable, y = Amount,  fill = Lost_value, text = paste("Percentage:", Percentage*100, "%", "\n Amount: ", Amount))) +
        geom_bar(stat = "identity", position = "dodge")+
        guides(fill = FALSE)+
        facet_wrap(~lab)+
        labs(y = "Value")+
        scale_fill_manual(values=c("#41AE76","#FBB4AE"))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
              plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background = element_rect(fill="white"),
              strip.text = element_text(size = 12, face = "bold", hjust = 0.5))
      
      ply2 <- ggplotly(p2, tooltip = c("text"))
      
      plot <- plotly::subplot(ply1, ply2, titleY = TRUE, margin = 0.07)
      
      plot
    }) 
    #------------------------------------------------------------------------------------
    output$insecticide_wear <- renderPlotly({
      
      df <- data()

      improve_insecticide <- df[30,2]*100
      reduce_wear <- df[35,2]*100
      df1 <- df[31:32,]
      df1$Lost_value <- "After bioefficacy improvement"
      
      total <- df[19,] %>% mutate(Variable = case_when(Variable == "Lost value (all nets)" ~ "Initial lost value (all nets)"))
      total$Lost_value <- "Lost value withouth improvements"
      df3 <- rbind(df1, total)
      df3$lab <- paste("Reduce insecticide efficacy by:", improve_insecticide, "%")
      
      #-----------------------------------------------------------------------------------------
      df2 <- df[36:37,]
      df2$Lost_value <- "After wear and tear improvement"
      
      df4 <- rbind(df2, total)
      df4$lab <- paste("Reduce wear and tear by:", reduce_wear, "%")
      
      p1 <- ggplot(df3, aes(x = Variable, y = Amount,  fill = Lost_value, text = paste("Percentage:", Percentage*100, "%", "\n Amount: ", Amount))) +
        geom_bar(stat = "identity", position = "dodge")+
        facet_wrap(~lab)+
        guides(fill = FALSE)+
        labs(y = "Value")+
        scale_fill_manual(values=c("#C2A5CF","#FBB4AE"))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              text = element_text(size = 10), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
              plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background =element_rect(fill="white"),
              strip.text = element_text(size = 12, face = "bold", hjust = 0.5))
      
      ply1 <- ggplotly(p1, tooltip = c("text"))
      
      p2 <- ggplot(df4, aes(x = Variable, y = Amount,  fill = Lost_value, text = paste("Percentage:", Percentage*100, "%", "\n Amount: ", Amount))) +
        geom_bar(stat = "identity", position = "dodge")+
        facet_wrap(~lab)+
        guides(fill = FALSE)+
        labs(y = "Value")+
        scale_fill_manual(values=c("#9970AB","#FBB4AE"))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              text = element_text(size = 10), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
              plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background =element_rect(fill="white"),
              strip.text = element_text(size = 12, face = "bold", hjust = 0.5))
      
      ply2 <- ggplotly(p2, tooltip = c("text"))
      
      plot <- plotly::subplot(ply1, ply2, titleY = TRUE, margin = 0.07)
      
      plot
    })
    
    #-----------------------------------------------------------------------------------------
    output$effectiveness_geo <- renderPlotly({
      
      df <- data()
      improve_effectiveness<- df[40,2]*100
      coverage_need <- df[45,2]*100
      
      df1 <- df[41:42,]
      df1$Lost_value <- "After effectiveness improvement"
      
      total <- df[19,] %>% mutate(Variable = case_when(Variable == "Lost value (all nets)" ~ "Initial lost value (all nets)"))
      total$Lost_value <- "Lost value withouth improvements"
      df3 <- rbind(df1, total)%>%
        mutate(Amount = as.numeric(gsub(",", "", gsub("\\$", "", Amount))))
      df3$lab <- paste("Improve effectiveness by:", improve_effectiveness, "% \n (bioefficacy + wear and tear)")
      #-----------------------------------------------------------------------------------------
      # df2 <- df[36,]
      # df2$Lost_value <- "After regional coverage improvement"
      # 
      # df4 <- rbind(df2, total)%>%
      #   mutate(Amount = as.numeric(gsub(",", "", gsub("\\$", "", Amount))))
      # df4$lab <- paste("% Reduction in coverage need:", coverage_need, "%")
      

      
      p1 <- ggplot(df3, aes(x = Variable, y = Amount,  fill = Lost_value, text = paste("Percentage:", Percentage*100, "%", "\n Amount: ", mycurrency(Amount))))+
        geom_bar(stat = "identity", position = "dodge")+
        facet_wrap(~lab)+
        guides(fill = FALSE)+
        labs(y = "")+
        scale_y_continuous(limits = c(0, max(df3$Amount)), breaks =c(303417556, 468751216), labels = mycurrency(c(303417556, 468751216)))+
        scale_fill_manual(values=c("#4393C3" ,"#FBB4AE"))+        
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              text = element_text(size = 10), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
              plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_blank(), strip.background =element_rect(fill="white"),
              strip.text = element_text(size = 12, face = "italics", hjust = 0.5),  plot.margin = unit(c(1, 0, 0, 0), "cm"))
      
      ply1 <- ggplotly(p1, tooltip = c("text"))
      
      # p2 <- ggplot(df4, aes(x = Variable, y = Amount,  fill = Lost_value, text = paste("Percentage:", Percentage*100, "%"))) +
      #   geom_bar(stat = "identity", position = "dodge")+
      #   facet_wrap(~lab)+
      #   labs( y = "Value")+
      #   guides(fill = FALSE)+
      #   scale_y_continuous(labels = dollar_format())+
      #   scale_fill_manual(values=c("#92C5DE","#D53E4F"))+        
      #   scale_x_discrete(labels= "% Reduction in coverage need")+
      #   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
      #         panel.background = element_blank(), axis.line = element_line(colour = "black"),
      #         text = element_text(size = 10), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
      #         plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background =element_rect(fill="white"),
      #         strip.text = element_text(size = 12, face = "bold", hjust = 0.5))
      # 
      # ply2 <- ggplotly(p2, tooltip = c("text"))
      
      # plot <- subplot(ply1, ply2, titleY = TRUE, titleX = TRUE, margin = 0.07)
      
      ply1
    })
    
    #----------------------------------------------------------------
    
    output$value <- renderPlotly({
      df <- data()

      r1 <- df[19,]
      r2 <- df[22,]
      r3 <- df[27,]
      r4 <- df[32,]
      r5 <- df[37,]
      r6 <- df[42,]


      df1 <- rbind(r1, r2, r3, r4, r5, r6) %>%
        as.data.frame() 
      df1$lab <- "Total degradation lost value ($)"
      
      df1 <- df1 %>%
        mutate(axis = c("Baseline lost value (all nets)", "Lost value with loss reduction",
                        "Lost value with usage improvement", "Lost value with bioefficacy improvement", "Lost value with wear and tear improvement",
                        "Lost value with effectiveness improvement"),
               Amount = as.numeric(gsub(",", "", gsub("\\$", "", Amount))))
              
      p1 <- df1 %>%
        ggplot(aes(reorder(x = axis, -Amount), y = Amount, text = paste("Lost value of LLINs:", mycurrency(Amount)))) +
        geom_bar( stat = "identity", fill = "#FFD92F", width = 0.4)+
        facet_wrap(~lab)+
        scale_y_continuous(labels = dollar_format())+
        labs(x = "", y = "")+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              text = element_text(size = 10), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
              plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background =element_rect(fill="white"),
              strip.text = element_text(size = 12, face = "bold", hjust = 0.5))
      
      ply1 <- ggplotly(p1, tooltip = c("text"))
      ply1
      
      
    })

    output$differences <- renderPlotly({
      
      df <- data()
      
      r1 <- df[19,]
      r2 <- df[22,]
      r3 <- df[27,]
      r4 <- df[32,]
      r5 <- df[37,]
      r6 <- df[41,]
      r7 <- df[46,]
      
      df1 <- rbind(r1, r2, r3, r4, r5, r6, r7)
      
      df1 <- df1 %>%
        mutate(axis = c("Baseline lost value (all nets)", "Gain from loss reduction",
                        "Gain from usage improvement", "Gain from bioefficacy improvement", "Gain from wear and tear improvement",
                        "Gain from effectiveness improvement", "Gain from reduction in coverage need"),
               Amount = as.numeric(gsub(",", "", gsub("\\$", "", Amount))),
               gain = Amount[1]-Amount)
      
      df1$lab <- "Gains due to improvements from baseline loss ($)"
      df1[7,5] <- df1[7,3]
      
      p1 <- df1 %>%
        ggplot(aes(x = reorder(axis, -gain), y = gain, text = paste("Gain:", mycurrency(gain)))) +
        geom_bar( stat = "identity", fill = "#8DD3C7", width = 0.4)+
        facet_wrap(~lab)+
        labs(x = "", y = "")+
        scale_y_continuous(labels = dollar_format())+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              text = element_text(size = 10), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
              plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background =element_rect(fill="white"),
              strip.text = element_text(size = 12, face = "bold", hjust = 0.5))
      
      ply1 <- ggplotly(p1, tooltip = c("text"))
      ply1
      
    })
    
    #################### HEALTH IMPACT ######################################
    output$cochrane <- renderUI({
      str1 <- "For every 1,000 children protected with ITNs, 5.6 lives are saved."
      str2 <- "For every 1,000 people protected with ITNs, 128 malaria cases are prevented."
      source <- "Source: Pryce et al (2018) Cochrane Database"
      HTML(paste(str1, str2,source, sep = '<br/>'))
    })
    
    
    output$formulas <- renderUI({
      withMathJax(
        helpText('Number of children deaths prevented: $$(ActualLlinEffectivelyUsed - LlinEffectivelyUsedWithImprovement)*2/1000*5.6$$'),
        helpText('Number of malaria cases prevented: $$(ActualLlinEffectivelyUsed - LlinEffectivelyUsedWithImprovement)*2/1000*128$$')
      )
    })

    #################
    ### CASES ########

    output$health_loss_usage <- renderPlotly({
      df <- data() 
      df <- df %>%
        mutate(Amount = as.numeric(gsub(",", "", gsub("\\$", "", Amount))))

      improvement <- c(0.1, 0.15, 0.2, 0.25, 0.3)
      improvement_loss_df <- data.frame(improvement)%>%
        mutate(LLIN_loss = df[1,3]*(1-((1-improvement)*df[9,2]+df[10,2]+df[11,2]+df[12,2])),
              #deaths_prevented = round((LLIN_loss-df[17,3])*2/1000*5.6,0),
               cases_prevented = round((LLIN_loss-df[17,3])*2/1000*128,0),
               improvement = as.character(improvement),
               ) %>% gather(variable, amount, c(cases_prevented), -improvement, -LLIN_loss) %>%
        mutate(variable = case_when(
                                    variable == "cases_prevented" ~ "Malaria cases prevented"))

      improvement_loss_df$lab <- "Health impact from \n LLIN loss reduction"
      #-----------------------------------------------------------------
      improvement_usage_df <- data.frame(improvement)%>%
        mutate(LLIN_usage = df[1,3]*(1-(df[9,2]+(1-improvement)*df[10,2]+df[11,2]+df[12,2])),
               #deaths_prevented = round((LLIN_usage-df[17,3])*2/1000*5.6,0),
               cases_prevented = round((LLIN_usage-df[17,3])*2/1000*128,0),
               improvement = as.character(improvement)
        )%>%
        gather(variable, amount, c(cases_prevented), -improvement, -LLIN_usage) %>%
        mutate(variable = case_when(
                                    variable == "cases_prevented" ~ "Malaria cases prevented"))

      improvement_usage_df$lab <- "Health impact from \n LLIN usage improvement"

      p1 <- ggplot(improvement_loss_df, aes(x = improvement, y = amount,
                                            text = paste("Health impact:", prettyNum(amount, big.mark=",")))) +
        geom_bar(stat = "identity", fill = "#1B9E77")+
        guides(fill = FALSE)+
        facet_wrap(~lab)+
        scale_x_discrete(labels = c("0.1" = "10%", "0.15" = "15%", "0.2" = "20%", "0.25" = "25 %", "0.3" = "30 %"))+
        #scale_fill_manual(values=c("#1B9E77"))+
        scale_y_continuous(expand = c(0, 0),limits = c(0, 7000000), breaks = c(0,250000, 1000000, 2500000, 5000000, 6500000),
                           labels = formatC(as.numeric(c(0,250000, 1000000, 2500000, 5000000, 65000000)), format="f", digits=0, big.mark=","))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black", size = 1),
              text = element_text(size = 10), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
              plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background =element_rect(fill="white"),
              strip.text = element_text(size = 12, face = "bold", hjust = 0.5), plot.margin = unit(c(2, 2, 2, 2), "cm"))

      ply1 <- ggplotly(p1, tooltip = c("text"))

      p2 <- ggplot(improvement_usage_df, aes(x = improvement, y = amount, 
                                            text = paste("Health impact:", prettyNum(amount, big.mark=",")))) +
        geom_bar(stat = "identity", fill = "#1B9E77")+
        guides(fill = FALSE)+
        facet_wrap(~lab)+
        scale_x_discrete(labels = c("0.1" = "10%", "0.15" = "15%", "0.2" = "20%", "0.25" = "25 %", "0.3" = "30 %"))+
        
        scale_y_continuous(expand = c(0, 0),limits = c(0, 7000000), breaks = c(0,250000, 1000000, 2500000, 5000000, 6500000),
                           labels = formatC(as.numeric(c(0,250000, 1000000, 2500000, 5000000, 65000000)), format="f", digits=0, big.mark=","))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black", size = 1),
              text = element_text(size = 10), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
              plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background =element_rect(fill="white"),
              strip.text = element_text(size = 12, face = "bold", hjust = 0.5),plot.margin = unit(c(2, 2, 2, 2), "cm"))

      ply2 <- ggplotly(p2, tooltip = c("text"))

      plot <- plotly::subplot(style(ply1, showlegend=F), ply2, margin = 0.07, titleX = TRUE, titleY = TRUE) %>%
        layout(xaxis = list(title = "Loss reduction", titlefont = list(size = 12)),
               xaxis2 = list(title="Usage improvement", titlefont = list(size = 12)),
               yaxis = list(title = ""), yaxis2 = list(title = ""), title = "Malaria cases prevented")

      plot
    })
    #------------------------------------------------------------------------------------
    ############### DEATHS ###################
    output$health_loss_usage2 <- renderPlotly({
      df <- data() 
      df <- df %>%
        mutate(Amount = as.numeric(gsub(",", "", gsub("\\$", "", Amount))))
      
      improvement <- c(0.1, 0.15, 0.2, 0.25, 0.3)
      improvement_loss_df <- data.frame(improvement)%>%
        mutate(LLIN_loss = df[1,3]*(1-((1-improvement)*df[9,2]+df[10,2]+df[11,2]+df[12,2])),
               deaths_prevented = round((LLIN_loss-df[17,3])*2/1000*5.6,0),
               improvement = as.character(improvement),
        ) %>% gather(variable, amount, c(deaths_prevented), -improvement, -LLIN_loss) %>%
        mutate(variable = case_when(
          variable == "deaths_prevented" ~ "Children deaths prevented"))
      
      improvement_loss_df$lab <- "Health impact from \n LLIN loss reduction"
      #-----------------------------------------------------------------
      improvement_usage_df <- data.frame(improvement)%>%
        mutate(LLIN_usage = df[1,3]*(1-(df[9,2]+(1-improvement)*df[10,2]+df[11,2]+df[12,2])),
               deaths_prevented = round((LLIN_usage-df[17,3])*2/1000*5.6,0),
               improvement = as.character(improvement)
        )%>%
        gather(variable, amount, c(deaths_prevented), -improvement, -LLIN_usage) %>%
        mutate(variable = case_when(
          variable == "deaths_prevented" ~ "Children deaths prevented"))
      
      improvement_usage_df$lab <- "Health impact from \n LLIN usage improvement"
      
      p1 <- ggplot(improvement_loss_df, aes(x = improvement, y = amount,
                                            text = paste("Health impact:", prettyNum(amount, big.mark=",")))) +
        geom_bar(stat = "identity", fill = "#313695")+
        guides(fill = FALSE)+
        facet_wrap(~lab)+
        scale_x_discrete(labels = c("0.1" = "10%", "0.15" = "15%", "0.2" = "20%", "0.25" = "25 %", "0.3" = "30 %"))+
        scale_y_continuous(expand = c(0, 0),limits = c(0, 350000), breaks = c(0,50000, 100000, 200000, 300000, 350000),
                           labels = formatC(as.numeric(c(0,50000, 100000, 200000, 300000, 350000)), format="f", digits=0, big.mark=","))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black", size = 1),
              text = element_text(size = 10), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
              plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background =element_rect(fill="white"),
              strip.text = element_text(size = 12, face = "bold", hjust = 0.5), plot.margin = unit(c(2, 2, 2, 2), "cm"))
      
      ply1 <- ggplotly(p1, tooltip = c("text"))
      
      p2 <- ggplot(improvement_usage_df, aes(x = improvement, y = amount, 
                                             text = paste("Health impact:", prettyNum(amount, big.mark=",")))) +
        geom_bar(stat = "identity", fill = "#313695")+
        guides(fill = FALSE)+
        facet_wrap(~lab)+
        scale_x_discrete(labels = c("0.1" = "10%", "0.15" = "15%", "0.2" = "20%", "0.25" = "25 %", "0.3" = "30 %"))+
        scale_y_continuous(expand = c(0, 0),limits = c(0, 350000), breaks = c(0,50000, 100000, 200000, 300000, 350000),
                           labels = formatC(as.numeric(c(0,50000, 100000, 200000, 300000, 350000)), format="f", digits=0, big.mark=","))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black", size = 1),
              text = element_text(size = 10), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
              plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background =element_rect(fill="white"),
              strip.text = element_text(size = 12, face = "bold", hjust = 0.5),plot.margin = unit(c(2, 2, 2, 2), "cm"))
      
      ply2 <- ggplotly(p2, tooltip = c("text"))
      
      plot <- plotly::subplot(style(ply1, showlegend=F), ply2, margin = 0.07, titleX = TRUE, titleY = TRUE) %>%
        layout(xaxis = list(title = "Loss reduction", titlefont = list(size = 12)),
               xaxis2 = list(title="Usage improvement", titlefont = list(size = 12)),
               yaxis = list(title = ""), yaxis2 = list(title = ""), title = "Children deaths prevented")
      
      plot
    })
    #-------------------------------------------------------------------------
    ###### CASES #######
    output$health_insecticide_wear <- renderPlotly({
      df <- data() 
      df <- df %>%
        mutate(Amount = as.numeric(gsub(",", "", gsub("\\$", "", Amount))))
      
      improvement <- c(0.1, 0.15, 0.2, 0.25, 0.3)
      improvement_insecticide_df <- data.frame(improvement)%>%
        mutate(LLIN_loss = df[1,3]*(1-(df[9,2]+df[10,2]+(1-improvement)*df[11,2]+df[12,2])),deaths_prevented = round((LLIN_loss-df[17,3])*2/1000*5.6,0),
               cases_prevented = round((LLIN_loss-df[17,3])*2/1000*128,0),
               improvement = as.character(improvement),
        ) %>% gather(variable, amount, c(cases_prevented), -improvement, -LLIN_loss) %>%
        mutate(variable = case_when(variable == "cases_prevented" ~ "Malaria cases prevented"))
      
      improvement_insecticide_df$lab <- "Health impact from \n LLIN bioefficacy improvement"
      #-----------------------------------------------------------------
      improvement_wear_df <- data.frame(improvement)%>%
        mutate(LLIN_usage = df[1,3]*(1-(df[9,2]+df[10,2]+df[11,2]+(1-improvement)*df[12,2])),
               cases_prevented = round((LLIN_usage-df[17,3])*2/1000*128,0),
               improvement = as.character(improvement)
        )%>%
        gather(variable, amount, c(cases_prevented), -improvement, -LLIN_usage) %>%
        mutate(variable = case_when(variable == "cases_prevented" ~ "Malaria cases prevented"))
      
      improvement_wear_df$lab <- "Health impact from \n LLIN wear and tear improvement"
      
      p1 <- ggplot(improvement_insecticide_df, aes(x = improvement, y = amount,
                                            text = paste("Health impact:", prettyNum(amount, big.mark=",")))) +
        geom_bar(stat = "identity", fill = "#A50026")+
        guides(fill = FALSE)+
        facet_wrap(~lab)+
        scale_x_discrete(labels = c("0.1" = "10%", "0.15" = "15%", "0.2" = "20%", "0.25" = "25 %", "0.3" = "30 %"))+
        
        scale_y_continuous(expand = c(0, 0),limits = c(0, 500500), breaks = c(0, 50000, 100000, 250000, 500000),
                           labels = formatC(as.numeric(c(0,25000, 50000, 100000,  500000)), format="f", digits=0, big.mark=","))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black", size = 1),
              text = element_text(size = 10), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
              plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background =element_rect(fill="white"),
              strip.text = element_text(size = 12, face = "bold", hjust = 0.5), plot.margin = unit(c(2, 2, 2, 2), "cm"))
      
      ply1 <- ggplotly(p1, tooltip = c("text"))
      
      p2 <- ggplot(improvement_wear_df, aes(x = improvement, y = amount,
                                             text = paste("Health impact: ", prettyNum(amount, big.mark=",")))) +
        geom_bar(stat = "identity", fill = "#A50026")+
        guides(fill = FALSE)+
        facet_wrap(~lab)+
        scale_x_discrete(labels = c("0.1" = "10%", "0.15" = "15%", "0.2" = "20%", "0.25" = "25 %", "0.3" = "30 %"))+
        scale_y_continuous(expand = c(0, 0),limits = c(0, 500500), breaks = c(0,50000, 100000, 250000, 500000),
                           labels = formatC(as.numeric(c(0,25000, 50000, 100000, 500000)), format="f", digits=0, big.mark=","))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black", size = 1),
              text = element_text(size = 10), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
              plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background =element_rect(fill="white"),
              strip.text = element_text(size = 12, face = "bold", hjust = 0.5),plot.margin = unit(c(2, 2, 2, 2), "cm"))
      
      ply2 <- ggplotly(p2, tooltip = c("text"))
      
      plot <- plotly::subplot(style(ply1, showlegend=F), ply2, margin = 0.07, titleX = TRUE, titleY = TRUE) %>%
        layout(xaxis = list(title = "Bioefficacy improvement", titlefont = list(size = 12)),
               xaxis2 = list(title="Wear and tear improvement", titlefont = list(size = 12)),
               yaxis = list(title = ""), yaxis2 = list(title = ""), title = "Malaria cases prevented")
      
      plot
    })
    
    #-----------------------------------------------------------------------------------------
    
    ###### DEATHS #######
    output$health_insecticide_wear2 <- renderPlotly({
      df <- data() 
      df <- df %>%
        mutate(Amount = as.numeric(gsub(",", "", gsub("\\$", "", Amount))))
      
      improvement <- c(0.1, 0.15, 0.2, 0.25, 0.3)
      improvement_insecticide_df <- data.frame(improvement)%>%
        mutate(LLIN_loss = df[1,3]*(1-(df[9,2]+df[10,2]+(1-improvement)*df[11,2]+df[12,2])),
               deaths_prevented = round((LLIN_loss-df[17,3])*2/1000*5.6,0),
               improvement = as.character(improvement),
        ) %>% gather(variable, amount, c(deaths_prevented), -improvement, -LLIN_loss) %>%
        mutate(variable = case_when(variable == "deaths_prevented" ~ "Children deceased prevented"))
      
      improvement_insecticide_df$lab <- "Health impact from \n LLIN bioefficacy improvement"
      #-----------------------------------------------------------------
      improvement_wear_df <- data.frame(improvement)%>%
        mutate(LLIN_usage = df[1,3]*(1-(df[9,2]+df[10,2]+df[11,2]+(1-improvement)*df[12,2])),
               deaths_prevented = round((LLIN_usage-df[17,3])*2/1000*5.6,0),
               improvement = as.character(improvement)
        )%>%
        gather(variable, amount, c(deaths_prevented), -improvement, -LLIN_usage) %>%
        mutate(variable = case_when(variable == "deaths_prevented" ~ "Children deceased prevented"))
      
      improvement_wear_df$lab <- "Health impact from \n LLIN wear and tear improvement"
      
      p1 <- ggplot(improvement_insecticide_df, aes(x = improvement, y = amount, 
                                                   text = paste("Health impact:", prettyNum(amount, big.mark=",")))) +
        geom_bar(stat = "identity", fill = "#F46D43")+
        guides(fill = FALSE)+
        facet_wrap(~lab)+
        scale_x_discrete(labels = c("0.1" = "10%", "0.15" = "15%", "0.2" = "20%", "0.25" = "25 %", "0.3" = "30 %"))+
        
        scale_y_continuous(expand = c(0, 0),limits = c(0, 25000), breaks = c(0,5000, 10000, 15000, 20000),
                           labels = formatC(as.numeric(c(0,5000, 10000, 15000, 20000)), format="f", digits=0, big.mark=","))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black", size = 1),
              text = element_text(size = 10), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
              plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background =element_rect(fill="white"),
              strip.text = element_text(size = 12, face = "bold", hjust = 0.5), plot.margin = unit(c(2, 2, 2, 2), "cm"))
      
      ply1 <- ggplotly(p1, tooltip = c("text"))
      
      p2 <- ggplot(improvement_wear_df, aes(x = improvement, y = amount,
                                            text = paste("Health impact: ", prettyNum(amount, big.mark=",")))) +
        geom_bar(stat = "identity", fill = "#F46D43")+
        guides(fill = FALSE)+
        facet_wrap(~lab)+
        scale_x_discrete(labels = c("0.1" = "10%", "0.15" = "15%", "0.2" = "20%", "0.25" = "25 %", "0.3" = "30 %"))+
        scale_y_continuous(expand = c(0, 0),limits = c(0, 25000), breaks = c(0, 5000, 10000, 15000, 20000),
                           labels = formatC(as.numeric(c(0, 5000, 10000, 15000, 20000)), format="f", digits=0, big.mark=","))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black", size = 1),
              text = element_text(size = 10), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
              plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background =element_rect(fill="white"),
              strip.text = element_text(size = 12, face = "bold", hjust = 0.5),plot.margin = unit(c(2, 2, 2, 2), "cm"))
      
      ply2 <- ggplotly(p2, tooltip = c("text"))
      
      plot <- plotly::subplot(style(ply1, showlegend=F), ply2, margin = 0.07, titleX = TRUE, titleY = TRUE) %>%
        layout(xaxis = list(title = "Bioefficacy improvement", titlefont = list(size = 12)),
               xaxis2 = list(title="Wear and tear improvement", titlefont = list(size = 12)),
               yaxis = list(title = ""), yaxis2 = list(title = ""), title = "Children deaths prevented")
      
      plot
    })
    #-----------------------------------------------------------------------------------
    ### CASES
    output$health_effectiveness_geo <- renderPlotly({
      df <- data() 
      df <- df %>%
        mutate(Amount = as.numeric(gsub(",", "", gsub("\\$", "", Amount))))
      
      improvement <- c(0.1, 0.15, 0.2, 0.25, 0.3)
      improvement_effectiveness_df <- data.frame(improvement)%>%
        mutate(LLIN_loss = df[1,3]*(1-(df[9,2]+df[10,2]+(1-improvement)*(df[11,2]+df[12,2]))),
               cases_prevented = round((LLIN_loss-df[17,3])*2/1000*128,0),
               improvement = as.character(improvement),
        ) %>% gather(variable, amount, c(cases_prevented), -improvement, -LLIN_loss) %>%
        mutate(variable = case_when(variable == "cases_prevented" ~ "Malaria cases prevented"))
      
      improvement_effectiveness_df$lab <- "Health impact from \n LLIN effectiveness improvement"
      #-----------------------------------------------------------------------------------------

      p1 <- ggplot(improvement_effectiveness_df, aes(x = improvement, y = amount,  
                                                   text = paste("Health impact:", prettyNum(amount, big.mark=",")))) +
        geom_bar(stat = "identity", fill = "#67A9CF")+
        guides(fill = FALSE)+
        facet_wrap(~lab)+ labs(x = "", y = "")+
        scale_x_discrete(labels = c("0.1" = "10%", "0.15" = "15%", "0.2" = "20%", "0.25" = "25 %", "0.3" = "30 %"))+
        scale_y_continuous(expand = c(0, 0),limits = c(0, 1000200), breaks = c(0,100000, 250000, 500000, 750000, 1000000),
                           labels = formatC(as.numeric(c(0, 100000, 250000, 500000, 750000, 1000000)), format="f", digits=0, big.mark=","))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black", size = 1),
              text = element_text(size = 10), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
              plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background =element_rect(fill="white"),
              strip.text = element_text(size = 12, face = "bold", hjust = 0.5), plot.margin = unit(c(2, 2, 2, 2), "cm"))
      
      ply1 <- ggplotly(p1, tooltip = c("text")) %>%
        layout(xaxis = list(title = "Effectiveness improvement", titlefont = list(size = 12)),
               yaxis = list(title = ""), title = "Malaria cases prevented")
      
      ply1
    })

    #---------------------------------------------------------------------------------------
    ### DEATHS
    output$health_effectiveness_geo2 <- renderPlotly({
      df <- data() 
      df <- df %>%
        mutate(Amount = as.numeric(gsub(",", "", gsub("\\$", "", Amount))))
      
      improvement <- c(0.1, 0.15, 0.2, 0.25, 0.3)
      improvement_effectiveness_df <- data.frame(improvement)%>%
        mutate(LLIN_loss = df[1,3]*(1-(df[9,2]+df[10,2]+(1-improvement)*(df[11,2]+df[12,2]))),
               deaths_prevented = round((LLIN_loss-df[17,3])*2/1000*5.6,0),
               improvement = as.character(improvement),
        ) %>% gather(variable, amount, c(deaths_prevented), -improvement, -LLIN_loss) %>%
        mutate(variable = case_when(variable == "deaths_prevented" ~ "Children deceased prevented"))
      
      improvement_effectiveness_df$lab <- "Health impact from \n LLIN effectiveness improvement"
      #-----------------------------------------------------------------------------------------
      
      p1 <- ggplot(improvement_effectiveness_df, aes(x = improvement, y = amount, 
                                                     text = paste("Health impact:", prettyNum(amount, big.mark=",")))) +
        geom_bar(stat = "identity", fill = "#014636")+
        guides(fill = FALSE)+
        facet_wrap(~lab)+ labs(x = "", y = "")+
        scale_x_discrete(labels = c("0.1" = "10%", "0.15" = "15%", "0.2" = "20%", "0.25" = "25 %", "0.3" = "30 %"))+
        scale_y_continuous(expand = c(0, 0),limits = c(0, 40500), breaks = c(0, 10000, 20000, 30000, 40500),
                           labels = formatC(as.numeric(c(0, 10000, 20000, 30000, 40500)), format="f", digits=0, big.mark=","))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black", size = 1),
              text = element_text(size = 10), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
              plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background =element_rect(fill="white"),
              strip.text = element_text(size = 12, face = "bold", hjust = 0.5), plot.margin = unit(c(2, 2, 2, 2), "cm"))
      
      ply1 <- ggplotly(p1, tooltip = c("text")) %>%
        layout(xaxis = list(title = "Effectiveness improvement", titlefont = list(size = 12)),
               yaxis = list(title = ""), title = "Children deaths prevented")
      
      ply1
    })
    
 }

# Create Shiny app ----
shinyApp(ui, server)

