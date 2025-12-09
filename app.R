library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)

# Load data
data <- read.csv("in_grammar_all.csv")

ui <- page_fluid(
  title = "Grammar Data Explorer",
  layout_sidebar(
    sidebar = sidebar(
      h3("Filters"),
      selectInput(
        "filter_type",
        "Filter by:",
        choices = c("Package", "Component", "Dependency Type"),
        selected = "Package"
      ),
      uiOutput("dynamic_filter"),
      hr(),
      p("Showing", textOutput("row_count", inline = TRUE), "records")
    ),
    
    # Main content with grid layout
    navset_card_tab(
      title = "Grammar Data Explorer",
      nav(
        "Data",
        layout_columns(
          # Left column: dropdown, graph
          col_widths = c(6, 6),
          
          # LEFT COLUMN
          card(
            full_screen = TRUE,
            card(
              class = "border-0",
              uiOutput("package_section"),
              uiOutput("component_section")
            )
          ),
          
          # RIGHT COLUMN
          layout_columns(
            col_widths = c(12, 12),
            
            # Top right: Data Summary
            card(
              h3("Data Summary"),
              verbatimTextOutput("summary_text"),
              full_screen = TRUE
            ),
            
            # Bottom right: Data Table
            card(
              h3("Data Table"),
              DT::dataTableOutput("data_table"),
              full_screen = TRUE
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Dynamically render the second dropdown based on filter type
  output$dynamic_filter <- renderUI({
    if (input$filter_type == "Package") {
      selectInput(
        "filter_value",
        "Select Package:",
        choices = c("All", unique(data$package)),
        selected = "All"
      )
    } else if (input$filter_type == "Component") {
      selectInput(
        "filter_value",
        "Select Component:",
        choices = c("All", unique(data$component)),
        selected = "All"
      )
    } else if (input$filter_type == "Dependency Type") {
      selectInput(
        "filter_value",
        "Select Dependency Type:",
        choices = c("All", unique(data$dep_type)),
        selected = "All"
      )
    }
  })
  
  # Reactive data based on filters
  filtered_data <- reactive({
    result <- data
    
    if (input$filter_value != "All") {
      if (input$filter_type == "Package") {
        result <- result |> filter(package == input$filter_value)
      } else if (input$filter_type == "Component") {
        result <- result |> filter(component == input$filter_value)
      } else if (input$filter_type == "Dependency Type") {
        result <- result |> filter(dep_type == input$filter_value)
      }
    }
    
    result
  })
  
  # Display filtered table
  output$data_table <- DT::renderDT({
    filtered_data()
  })
  
  # Show row count
  output$row_count <- renderText({
    nrow(filtered_data())
  })
  
  # Summary text
  output$summary_text <- renderText({
    df <- filtered_data()
    paste("Total Records:", nrow(df), "\n",
      "Unique Packages:", n_distinct(df$package), "\n",
      "Unique Components:", n_distinct(df$component), "\n",
      "Unique Functions:", n_distinct(df$fname))
  })
  
  # Package plot - only show if filtering by Package AND "All" is selected
  output$package_section <- renderUI({
    if (input$filter_type == "Package" && input$filter_value == "All") {
      tagList(
        h4("Top 15 Packages"),
        plotOutput("package_plot", height = "400px")
      )
    }
  })
  
  # Package plot
  output$package_plot <- renderPlot({
    filtered_data() |>
      group_by(package) |>
      summarize(count = n(), .groups = "drop") |>
      arrange(desc(count)) |>
      head(15) |>
      ggplot(aes(x = count, y = reorder(package, count))) +
      geom_col(fill = "steelblue") +
      labs(x = "Count", y = "Package") +
      theme_minimal()
  })
  
  # Component plot - only show if filtering by Component AND "All" is selected
  output$component_section <- renderUI({
    if (input$filter_type == "Component" && input$filter_value == "All") {
      tagList(
        h4("Components"),
        plotOutput("component_plot", height = "400px")
      )
    }
  })
  
  output$component_plot <- renderPlot({
    filtered_data() |>
      group_by(component) |>
      summarize(count = n(), .groups = "drop") |>
      arrange(desc(count)) |>
      ggplot(aes(x = count, y = reorder(component, count))) +
      geom_col(fill = "darkseagreen") +
      labs(x = "Count", y = "Component") +
      theme_minimal()
  })
  
}

shinyApp(ui, server)
