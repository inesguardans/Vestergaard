

library(shiny)
library(readxl)
library(shinythemes)
library(dplyr)
library(scales)
library(DT)
library(htmlwidgets)
library(htmltools)



df_construction <- function(selected_month, selected_variable){
  airquality %>%
    dplyr::filter(Month == selected_month,
                  !is.na(!!rlang::sym(selected_variable)))%>%
    select(Month, Day, selected_variable)
}

double_plotting <- function(selected_month, selected_variable){


  #lab <- labels_df %>% filter(variable == selected_variable) %>% select(label)
  df1 <- df_construction(selected_month, selected_variable)%>%
    mutate(lab = "Ozone")

  
  p1 <- ggplot(data = df1,  aes(x = Day, y = !!rlang::sym(selected_variable), text = paste("Year: ", Day), 
                                #color = !!rlang::sym(as.name(selected_variable)),
                                group = 1)) +
    geom_line(size=1)+ 
    facet_wrap(~lab)+
    scale_color_brewer(name = "", palette = "Paired")+
    labs(fill = "")+ 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          text = element_text(size = 10), axis.text.x = element_text(angle = 45),  axis.text.y = element_text(size = 8),
          plot.title = element_text(size = 12, hjust = 0.5), axis.title.x = element_text(size = 10), strip.background =element_rect(fill="white"),
          strip.text = element_text(size = 12, face = "bold", hjust = 0.5))
  
  
  ply1 <- ggplotly(p1, tooltip = c("text"), showlegend = TRUE)

  
  print(ply1)
  
}


# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  
  theme = shinythemes::shinytheme("flatly"),

  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
     
      #-------------------------------------------
      pickerInput("month_select", "Month",
                  choices = 1:5,
                  selected = "1"),

      #-------------------------------------------
      
      pickerInput("variable_select", "Variable",
                  choices = c("Solar.R", "Ozone", "Wind", "Temp"),
                  selected = "Wind")),

    
    # Main panel for displaying outputs ----
    mainPanel(
  
        h4("Plot"),
        renderPlotly("plot")

    )
    
  )
)


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  
  
  selected_variable <- reactive({
    switch(input$variable_select,
           "Ozone" = "Ozone",
           "Wind" = "Wind",
           "Solar.R" = "Solar.R",
           "Temp" = "Temp")
  })
  
  df1 <- reactive({
    df_construction(input$month_select, selected_variable())
  })
  
  # Plot
  output$plot <- renderPlotly({
    # validate(
    #   need(
    #     expr = !empty(df1()), 
    #     message = "There is no data for this Month-variable combination. \n Please make a different choice.")
    # )
    # 
    plots <- double_plotting(input$month_select, selected_variable()) # This is a function to plot
    plots
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)

