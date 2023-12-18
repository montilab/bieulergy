tabPanel("Data",
  tags$head(includeCSS("www/fonts.css")),
  tags$head(includeCSS("www/reactable.css")),
  tags$head(includeHTML("www/gitter.html")),
  sidebarPanel(
    selectInput("T0_dataset", label="Datasets", choices=datasets, selected=dataset),
    selectInput("T0_network", label="Network", choices=networks),
    actionButton("T0_change", "Change Dataset"),
    tags$br(),
    tags$br(),
    tags$br(),
    fileInput("T0_upload_file", "Network Upload", buttonLabel = "Browse...", multiple=FALSE, accept=c(".rds")),
    tags$br(),
    downloadButton("T0_export", "Export Dataset"),
    width=3
  ),
  mainPanel(
    tabsetPanel(id="data_tabs",
      tabPanel("Upload Instructions",
        includeMarkdown("include/uploading-networks.html")
      ),
      tabPanel("View Loaded Dataset",
        tags$br(),
        uiOutput("T0_stats"),
        tags$br(),
        uiOutput("T0_nodes")
      )
    ),
    width=9
  )
)
