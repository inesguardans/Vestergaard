

#-----------------------------------------------------------------------
test <- data.frame(test1 = c(1:3), test2 = c(4:6))
test[test$test1 == 1, "test1"] <- '<div style="width: 100%; height: 100%; z-index: 0; background-color: green; position:absolute; top: 0; left: 0; padding:5px;">
<span>1</span></div>'

library(shiny)

ui <- shinyUI(fluidPage(
  tableOutput("tt"),
  tags$head(tags$style("#tt td{
                       position:relative;
                       };

                       "))
)
)

server <- shinyServer(function(input, output) {
  
  output$tt <- renderTable({
    test
  }, sanitize.text.function = function(x) x)
})

shinyApp(ui = ui, server = server) 