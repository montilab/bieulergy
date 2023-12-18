tabPanel("Visualize",
  tabsetPanel(id="visualize_tabs",
    tabPanel("Visualize Network",
      tags$br(),
      sidebarPanel(
        selectInput("T3_network", label="Network", choices=networks),
        selectInput("T3_nodes", label="Genes", multiple=TRUE, choices=nodes, selected=node),
        actionButton("T3_visualize", "Visualize"),
        tags$br(),
        tags$br(),
        selectInput("T3_options", label="Options", choices=c("Network Selection", "Styling Options")),
        hr(),
        conditionalPanel(
          condition = "input.T3_options == 'Network Selection'",
          sliderInput("T3_degree", label="Edge Degree", min=1, max=20, value=1, step=1),
          checkboxInput("T3_target_edges", label="Show Target Edges", value=FALSE),
          checkboxInput("T3_global", label="Show All Nodes", value=FALSE),
          checkboxInput("T3_remove_isolates", label="Remove Isolates", value=TRUE),
          checkboxInput("T3_symbols", label="Use Node Symbols", value=TRUE),
          hr(),
          checkboxInput("T3_rwr", label="Propogate"), 
          sliderInput("T3_restart", label="Restart Probability", min=0, max=1, value=0.5, step=0.01),
          numericInput("T3_background", label="Background (Hypergeometric Test)", value=23467),
          genesets_UI("T3_genesets"),
          tags$br(),
          tags$br(),
          actionButton("T3_hyper", "Enrichment")
        ),
        conditionalPanel(
          condition = "input.T3_options == 'Styling Options'",
          sliderInput("T3_node_size", label="Node Size", min=0, max=50, value=20, step=1, post=" px"),
          sliderInput("T3_edge_width", label="Edge Width", min=0, max=20, value=2, step=1, post=" px"), 
          sliderInput("T3_font_size", label="Font Size", min=0, max=25, value=25, step=1, post=" px"),
          selectInput("T3_layout", label="Algorithm", choices=network_layout),
          numericInput("T3_seed", label="Set Seed", value=1, step=1)
        ),
        hr(),
        downloadButton("T3_export", "Export"),
        width=4
      ),
      mainPanel(
        visNetworkOutput("T3_plot", height="1000px", width="100%"),
        width=8
      )
    ),
    tabPanel("Network Propagation",
      tags$br(),
      tags$br(),
      fixedRow(
        column(4,
          reactableOutput("T3_propagation")
        ),
        column(8,
          reactableOutput("T3_enrichment")
        )
      )
    ),
    tabPanel("Compare Networks",
      tags$br(),
      sidebarPanel(            
        selectizeInput("T3_c_networks", label="Networks", choices=networks, selected=networks[1:2], multiple=TRUE),
        selectInput("T3_c_nodes", label="Genes", multiple=TRUE, choices=nodes, selected=node),
        checkboxInput("T3_c_symbols", label="Show Symbols", value=TRUE),
        sliderInput("T3_c_network_size", label="Network Size", min=200, max=1000, value=450, post=" px"),
        sliderInput("T3_c_label_size", label="Label Size", min=1, max=100, value=20, step=1, post=" px"),
        width=3
      ),
      mainPanel(
        fixedRow(
          column(12,
            uiOutput("T3_c_plots"),
            align="center"
          )
        ),
        width=9
      )
    )
  )
)
