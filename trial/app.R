
    ui = fluidPage(
      fluidRow(
        column(12,
               dataTableOutput('table')
        )
      )
    )
    
    server = function(input, output) {
     output$table <- DT::renderDataTable({
       
        
        datatable(iris, 
                  rownames = FALSE,
                  options = list(
                    autoWidth = TRUE,
                    
                    columnDefs = list(list(width = "250px", targets = c(0,1,2)), list(className = "dt-center", targets = c(1,2))),
                    pageLength = 23, info = FALSE, lengthMenu = 30,
                    paging = FALSE,
                    searching = FALSE,
                    lengthChange = FALSE,
                    ordering = FALSE
                    
                  ))%>%
         formatStyle("Sepal.Width", target="row",
                     backgroundColor =  styleEqual(iris$Sepal.Width[2], 
                                                  "lightgreen")
                     ,
                     borderTop = styleEqual(c(iris$Sepal.Width[3], iris$Sepal.Width[5]), c("2px solid red", "2px solid red"))) #this is not working because the target is "row". But it want it to be "row"
       
        
        
      })
      
    }

    shinyApp(ui, server)
    
    #---------------------------------------------------------------------------
    
    ui = fluidPage(
      fluidRow(
        column(12,
               plotlyOutput('plot')
        )
      )
    )
    
    server = function(input, output) {
      output$plot <- renderPlotly({
        
        
        
        p1 <- ggplot(mtcars, aes(wt, mpg, text = row.names(mtcars))) + 
          geom_bar(stat = "identity")
        ply1<- ggplotly(p1, tooltip = c("text"))
        
        p2 <- ggplot(mtcars, aes(wt, cyl, text = paste("Disp:", disp))) + 
          geom_bar(stat = "identity")
        ply2<- ggplotly(p2, tooltip = c("text"))
        
        
        subp <- subplot(ply1, ply2)
        dev.off()
        subp
        
        
      })
      
    }
    
    shinyApp(ui, server)
    
    #-------------------------------------------------------------
    
    if (interactive()) {
      library(shiny)
      library(ECharts2Shiny)
      
      # Prepare sample data for plotting --------------------------
      dat <- data.frame(c(1, 2, 3),
                        c(2, 4, 6))
      names(dat) <- c("Type-A", "Type-B")
      row.names(dat) <- c("Time-1", "Time-2", "Time-3")
      
      # Server function -------------------------------------------
      server <- function(input, output) {
        # Call functions from ECharts2Shiny to render charts
        renderBarChart(div_id = "test", grid_left = '1%', direction = "vertical",
                       data = dat)
      }
      
      # UI layout -------------------------------------------------
      ui <- fluidPage(
        # We MUST load the ECharts javascript library in advance
        loadEChartsLibrary(),
        
        tags$div(id="test", style="width:50%;height:400px;"),
        deliverChart(div_id = "test")
      )
      
      # Run the application --------------------------------------
      shinyApp(ui = ui, server = server)
    }
    