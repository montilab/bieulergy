tabPanel("Centrality",
  tabsetPanel(
    tabPanel("Distribution Visualizations",
      tags$br(),
      sidebarPanel(
        selectInput("T2_visualization_type", label="Type", choices=c("Centrality Distributions", "Important Nodes", "Hierarchical Clustering")),
        checkboxGroupInput("T2_visualization_networks", label="Networks", choices=networks, selected=networks),
        helpText("Plotting Options"),
        conditionalPanel(
          condition = "input.T2_visualization_type == 'Centrality Distributions'",
          selectInput("T2_cdistr_metric", label="Metric", choices=available_centrality_measures),
          selectInput("T2_cdistr_plot", label="Plot", choices=c("Boxplot"="boxplot", "Density"="density", "Cumulative"="ecdf")),
          checkboxInput("T2_cdistr_rescale", label="Rescale")
        ),
        conditionalPanel(
          condition = "input.T2_visualization_type == 'Important Nodes'",
          selectInput("T2_tnodes_metric", label="Metric", choices=available_centrality_measures),
          numericInput("T2_tnodes_top", label="Show Top", value=10, min=0, step=1),
          selectInput("T2_tnodes_selected", label="Select Nodes", multiple=TRUE, choices=nodes),
          checkboxInput("T2_tnodes_symbols", label="Show Symbols", value=TRUE),
          checkboxInput("T2_tnodes_log", label="Log Y-Axis", value=FALSE),
          checkboxInput("T2_tnodes_reverse", label="Reverse Y-Axis", value=FALSE),
          checkboxInput("T2_tnodes_intersect", label="Intersection Only", value=FALSE),
          sliderInput("T2_tnodes_size", label="Label Size", value=5, min=1, max=25, step=0.25, post=" px")
        ),
        conditionalPanel(
          condition = "input.T2_visualization_type == 'Hierarchical Clustering'",
          selectInput("T2_hclust_metric", label="Node Values", choices=available_centrality_measures),
          selectInput("T2_hclust_method", label="Distance Method", choices=c("Pearson"="pearson", "Kendall"="kendall"))
        ),
        actionButton("T2_visualization_button", "Plot")
      ),
      mainPanel(
        plotOutput("T2_visualization_plot", height="600px")
      )
    ),
    
    # Centrality Enrichment -------------------------------------------------- 
    
    tabPanel("Ranked Enrichment",
      tags$br(),
      sidebarPanel(
        selectInput("T2_enrichment_network", label="Network", choices=networks, selected=network),
        selectInput("T2_enrichment_metric", label="Rank Nodes By", choices=available_centrality_measures),
        helpText("Test Enrichment For..."),
        textAreaInput("T2_enrichment_symbols", label="Symbols", rows=5, placeholder="GENE1,GENE2,GENE3", resize="vertical"),
        actionButton("T2_enrichment_button", "Plot"),
        width=3
      ),
      mainPanel(
        fluidRow(
          column(4, reactableOutput("T2_enrichment_table")),
          column(8, plotOutput("T2_enrichment_plot"))
        ),
        width=9
      )
    ),

    # Differential Centrality --------------------------------------------------
    
    tabPanel("Differential Centrality Score",
      tags$br(),
      sidebarPanel(
        checkboxGroupInput("T2_dc_networks", label="Networks", choices=networks, selected=networks),
        selectInput("T2_dc_metric", label="Metric", choices=available_centrality_measures),
        numericInput("T2_dc_exponent", label="Exponent", value=5, min=0, max=100, step=1),
        numericInput("T2_dc_top", label="Top", value=50, min=1, max=Inf, step=1),
        selectInput("T2_dc_show", label="Show", choices=c("Ranking"="r", "Centrality"="v"), selected="Ranking"),
        actionButton("T2_dc_button", "Compute"),
        tags$br(),
        tags$br(),
        helpText("Please see the documentation to see how weighted rank scores are calculated."),
        plotOutput("T2_dc_ggwrs"),
        width=4
      ),
      mainPanel(
        uiOutput("T2_dc_plot_ui"),
        width=8
      )
    ),
    
    # Centrality Importance -------------------------------------------------- 
    
    tabPanel("Importance Measures",
      tags$br(),
      sidebarPanel(
        selectInput("T2_pca_network", label="Network", choices=networks),   
        helpText("Select a network and check which centrality measures are most important or are most similar."),
        width=3
      ),
      mainPanel(
        fluidRow(
          column(6, plotOutput("T2_pca_varexp", height="250px")),
          column(6, plotOutput("T2_pca_contri", height="250px"))
        ),
        fluidRow(
          tags$br(),
          column(6, plotOutput("T2_pca_pltvar", height="400px")),
          column(6, plotOutput("T2_pca_hclust", height="400px"))
        ),
        width=9
      )
    )
  )
)
