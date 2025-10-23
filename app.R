#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# app.R
suppressPackageStartupMessages({
  library(shiny)
  library(visNetwork)
  library(tidyverse)
})

# load your module
source(file.path(dirname(getwd()), "OTxplore-R/scripts/get_data.R"))

ui <- fluidPage(
  titlePanel("OTxplore-R: Target–Disease–Drug Explorer"),
  sidebarLayout(
    sidebarPanel(
      textInput("gene_id", "Ensembl Gene ID", value = "ENSG00000169083"),
      textInput("disease_efo_id", "Disease EFO ID (optional)", value = "EFO_0000616"),
      sliderInput("min_score", "Minimum association score",
                  min = 0, max = 1, value = 0.2, step = 0.05),
      textInput("evidence_types", "Evidence types (comma-separated, optional)",
                value = "genetic_association,literature"),
      numericInput("page_size", "Page size", value = 200, min = 50, max = 1000, step = 50),
      checkboxInput("use_mock", "Use mock data (offline)", value = FALSE),
      actionButton("run", "Fetch & Plot", class = "btn btn-primary"),
      width = 4
    ),
    mainPanel(
      visNetworkOutput("net", height = "600px"),
      tags$hr(),
      strong("Meta:"), verbatimTextOutput("meta"),
      tags$hr(),
      fluidRow(
        column(6, downloadButton("dl_nodes", "Download nodes.csv")),
        column(6, downloadButton("dl_edges", "Download edges.csv"))
      ),
      width = 8
    )
  )
)

server <- function(input, output, session) {
  # store last result
  res_rv <- reactiveVal(NULL)
  
  parse_evidence_types <- function(x) {
    x <- trimws(x %||% "")
    if (x == "") return(NULL)
    unique(strsplit(x, "\\s*,\\s*")[[1]])
  }
  
  observeEvent(input$run, {
    withProgress(message = if (isTRUE(input$use_mock)) "Generating mock data..." else "Fetching from Open Targets...", value = 0.1, {
      # fetch data (mock or live)
      res <- tryCatch({
        if (isTRUE(input$use_mock)) {
          mock_fetch_ot_data()
        } else {
          fetch_ot_data(
            gene_id        = input$gene_id,
            disease_efo_id = if (nzchar(input$disease_efo_id)) input$disease_efo_id else NULL,
            min_score      = input$min_score,
            evidence_types = parse_evidence_types(input$evidence_types),
            page_size      = input$page_size,
            log            = TRUE
          )
        }
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 8)
        NULL
      })
      
      validate(need(!is.null(res), "No result returned. Check inputs or try mock mode."))
      res_rv(res)
      incProgress(0.7, detail = "Rendering network...")
    })
  })
  
  output$net <- renderVisNetwork({
    res <- res_rv()
    req(res, res$graph$nodes, res$graph$edges)
    validate(
      need(nrow(res$graph$nodes) > 0, "No nodes to plot."),
      need(nrow(res$graph$edges) > 0, "No edges to plot.")
    )
    
    visNetwork(res$graph$nodes, res$graph$edges) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLegend() %>%
      visPhysics(stabilization = TRUE)
  })
  
  output$meta <- renderPrint({
    res <- res_rv()
    req(res)
    res$meta
  })
  
  # downloads
  output$dl_nodes <- downloadHandler(
    filename = function() sprintf("nodes_%s.csv", format(Sys.time(), "%Y%m%d-%H%M%S")),
    content = function(file) {
      res <- res_rv(); req(res)
      readr::write_csv(res$graph$nodes, file)
    }
  )
  
  output$dl_edges <- downloadHandler(
    filename = function() sprintf("edges_%s.csv", format(Sys.time(), "%Y%m%d-%H%M%S")),
    content = function(file) {
      res <- res_rv(); req(res)
      readr::write_csv(res$graph$edges, file)
    }
  )
}


# Run the application 
shinyApp(ui = ui, server = server)
