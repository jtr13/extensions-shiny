library(shiny)
library(tidyverse)
library(DT)

# Example data ----
df <- readr::read_csv("in_grammar_all.csv")

# UI ---------------------------------------------------------------
ui <- fluidPage(
  titlePanel("ggplot2 extension component explorer"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("filter_by", "Filter by:",
                   choices = c("Package" = "package",
                               "Component" = "component")),
      
      uiOutput("value_picker"),
      
      uiOutput("plot_container")
    ),
    
    mainPanel(
      textOutput("package_header"),
      DTOutput("tbl")
    )
  )
)

# SERVER -----------------------------------------------------------
server <- function(input, output, session) {
  
  # Dynamic UI: choose value depending on filter type
  output$value_picker <- renderUI({
    if (input$filter_by == "package") {
      selectInput("value", "Choose package:", 
                  choices = c("All", sort(unique(df$package))))
    } else {
      selectInput("value", "Choose component:", choices = sort(unique(df$component)))
    }
  })
  
  # Filtered dataset
  filtered <- reactive({
    req(input$value)
    if (input$filter_by == "package" && input$value == "All") {
      df
    } else {
      df %>% filter(.data[[input$filter_by]] == input$value)
    }
  })
  
  # Package header
  output$package_header <- renderText({
    d <- filtered()
    if (nrow(d) > 0 && input$filter_by == "package") {
      if (input$value == "All") {
        "All Packages"
      } else {
        paste("Package:", input$value)
      }
    } else if (nrow(d) > 0) {
      paste("Package:", unique(d$package))
    }
  })
  
  # Table - remove package and dep_type columns
  output$tbl <- renderDT({
    data_to_show <- filtered() %>%
      arrange(package, fname)
    
    # If showing all packages, keep the package column; otherwise remove it
    if (input$filter_by == "package" && input$value == "All") {
      data_to_show <- data_to_show %>% select(-dep_type)
    } else {
      data_to_show <- data_to_show %>% select(-package, -dep_type)
    }
    
    datatable(
      data_to_show,
      options = list(pageLength = 15)
    )
  })
  
  # Conditionally render plot - only if more than one component type
  output$plot_container <- renderUI({
    d <- filtered()
    n_components <- n_distinct(d$component)
    
    if (n_components > 1) {
      plotOutput("plt")
    }
  })
  
  # Graph rendering
  output$plt <- renderPlot({
    d <- filtered()
    n_components <- n_distinct(d$component)
    
    if (n_components > 1) {
      ggplot(d, aes(y = fct_rev(fct_infreq(component)))) +
        geom_bar() +
        labs(y = NULL) +
        theme_minimal(16)
    }
  })
}

shinyApp(ui, server)
