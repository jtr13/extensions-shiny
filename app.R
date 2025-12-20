# Created December 2025 with Positron Assistant
# Models: Claude Sonnet 4.5, Claude Opus 4.5, Claude Haiku 4.5

library(shiny)
library(tidyverse)
library(DT)

# Example data ----
df <- readr::read_csv("in_grammar_all_v2025_12_09.csv")

# UI ---------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      code {
        color: inherit;      /* Remove Bootstrap red */
        background-color: transparent;
      }
    "))
  ),
  titlePanel("ggplot2 ecosystem component explorer"),

  sidebarLayout(
    sidebarPanel(

      h4("Components"),
      uiOutput("component_checkboxes"),

      h4("Sort by:"),
      radioButtons("sort_method", NULL,
                   choices = c("Alphabetical" = "alpha",
                               "Function count" = "count"),
                   selected = "alpha",
                   inline = TRUE),

      uiOutput("value_picker"),

      uiOutput("plot_container"),

      width = 5
    ),

    mainPanel(
      textOutput("package_header"),
      DTOutput("tbl"),
      HTML(
        'Data: Exported functions that begin with <code>geom_</code>,
        <code>stat_</code>, <code>coord_</code>, <code>scale_</code>,
        <code>facet_</code> or <code>theme_</code> from <code>ggplot2</code> or
        any package on CRAN that imports
        or depends on <code>ggplot2</code>.'
      ),
      helpText(
        a("View source on GitHub",
          href = "https://github.com/jtr13/extensions-shiny",
          target = "_blank")
      ),
      width = 7
    )
  )
)

# SERVER -----------------------------------------------------------
server <- function(input, output, session) {

  # Component checkboxes - all checked by default
  output$component_checkboxes <- renderUI({
    components <- sort(unique(df$component))

    tagList(
      actionButton("check_all", "Check all", class = "btn-sm"),
      actionButton("uncheck_all", "Uncheck all", class = "btn-sm"),
      checkboxGroupInput("selected_components", NULL,
                         choices = components,
                         selected = components,
                         inline = TRUE)
    )
  })

  # Check all button
  observeEvent(input$check_all, {
    components <- sort(unique(df$component))
    updateCheckboxGroupInput(session, "selected_components",
                             selected = components)
  })

  # Uncheck all button
  observeEvent(input$uncheck_all, {
    updateCheckboxGroupInput(session, "selected_components",
                             selected = character(0))
  })

  # Filtered dataset based on selected components
  filtered_by_components <- reactive({
    req(input$selected_components)
    df |> filter(component %in% input$selected_components)
  })

  # Package dropdown - updates based on selected components
  output$value_picker <- renderUI({
    input$sort_method
    input$selected_components

    package_counts <- filtered_by_components() |>
      group_by(package) |>
      summarise(n_functions = n(), .groups = "drop")

    if (!is.null(input$sort_method) && input$sort_method == "count") {
      package_counts <- package_counts |> arrange(desc(n_functions))
      package_labels <- paste0(package_counts$package, " (", package_counts$n_functions, ")")
      package_choices <- setNames(package_counts$package, package_labels)
    } else {
      package_counts <- package_counts |>
        mutate(package_lower = tolower(package)) |>
        arrange(package_lower) |>
        select(-package_lower)
      package_choices <- setNames(package_counts$package, package_counts$package)
    }

    # Keep current selection if it's still valid, otherwise use "All"
    current_selection <- if (!is.null(input$value) && input$value %in% c("All", package_counts$package)) {
      input$value
    } else {
      "All"
    }

    selectInput("value", "Choose package:",
                choices = c("All" = "All", package_choices),
                selected = current_selection)
  })

  # Filtered dataset by selected package
  filtered <- reactive({
    req(input$value)
    data <- filtered_by_components()

    if (input$value == "All") {
      data
    } else {
      data |> filter(package == input$value)
    }
  })

  # Package header
  output$package_header <- renderText({
    d <- filtered()
    if (nrow(d) > 0) {
      if (input$value == "All") {
        "All Packages"
      } else {
        paste("Package:", input$value)
      }
    }
  })

  # Table
  output$tbl <- renderDT({
    data_to_show <- filtered() |> arrange(package, fname)

    # If showing all packages and sorting by function count, reorder by function count
    if (input$value == "All" && input$sort_method == "count") {
      package_order <- filtered_by_components() |>
        group_by(package) |>
        summarise(n_functions = n(), .groups = "drop") |>
        arrange(desc(n_functions)) |>
        pull(package)

      data_to_show <- data_to_show |>
        mutate(package = factor(package, levels = package_order)) |>
        arrange(package, fname) |>
        mutate(package = as.character(package))
    }

    # If showing all packages, keep the package column; otherwise remove it
    if (input$value == "All") {
      data_to_show <- data_to_show |> select(-dep_type)
    } else {
      data_to_show <- data_to_show |> select(-package, -dep_type)
    }

    datatable(
      data_to_show,
      options = list(pageLength = 10)
    )


  })


  # ...existing code...
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
    ggplot(d, aes(y = fct_rev(fct_infreq(component)))) +
      geom_bar() +
      labs(y = NULL) +
      theme_minimal(16)
  })
}
options(shiny.autoload.r=FALSE)
shinyApp(ui, server)
