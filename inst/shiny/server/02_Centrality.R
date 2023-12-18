# Visualization -------------------------------------------------- 

do_visualization <- eventReactive(input$T2_visualization_button, {
    if (input$T2_visualization_type == "Centrality Distributions") {
        
        networks.cdistr(networks = oracle.networks(input$T2_visualization_networks), 
                        metric   = input$T2_cdistr_metric,
                        plot     = input$T2_cdistr_plot,
                        rescale  = input$T2_cdistr_rescale)
    }
    else if (input$T2_visualization_type == "Important Nodes") {
        
        networks.tnodes(networks = oracle.networks(input$T2_visualization_networks), 
                        metric   = input$T2_tnodes_metric, 
                        top      = input$T2_tnodes_top, 
                        size     = input$T2_tnodes_size, 
                        search   = input$T2_tnodes_selected,
                        log      = input$T2_tnodes_log,
                        reverse  = input$T2_tnodes_reverse,
                        symbols  = input$T2_tnodes_symbols, 
                        shared   = input$T2_tnodes_intersect) 
    }
    else if (input$T2_visualization_type == "Hierarchical Clustering") {
        
        networks.hclust(networks = oracle.networks(input$T2_visualization_networks),
                        metric   = input$T2_hclust_metric,
                        method   = input$T2_hclust_method)
    }
})

output$T2_visualization_plot <- renderPlot({
    do_visualization()
})

# Enrichment -------------------------------------------------- 

get_enrichment_terms <- reactive({
    symbols <- input$T2_enrichment_symbols %>%
        stringr::str_split(pattern=",", simplify=TRUE) %>%
        as.vector()
    data.frame(symbol=symbols, rank=seq(length(symbols)))
})
do_enrichment_plot <- eventReactive(input$T2_enrichment_button, {
    df <- get_enrichment_terms()
    network.kstest(network=oracle.network(input$T2_enrichment_network),
                   metric=input$T2_enrichment_metric, 
                   geneset=df$symbol)
})
output$T2_enrichment_plot <- renderPlot({
    do_enrichment_plot()
})
do_enrichment_table <- eventReactive(input$T2_enrichment_button, {
    df <- get_enrichment_terms()
    df %>%
    magrittr::set_colnames(stringr::str_to_title(colnames(.))) %>%
    reactable(compact=FALSE, 
              fullWidth=TRUE,
              resizable=FALSE,
              defaultPageSize=15,
              showPageSizeOptions=FALSE,
              striped=TRUE,
              style=list(backgroundColor="#FCFCFC"))
})
output$T2_enrichment_table <- renderReactable({
    do_enrichment_table()
})

# Differential -------------------------------------------------- 

do_differential_centrality_plot <- eventReactive(input$T2_dc_button, {
    networks.diffc(networks=oracle.networks(input$T2_dc_networks),
                   metric=input$T2_dc_metric,
                   p=input$T2_dc_exponent,
                   top=input$T2_dc_top,
                   show=input$T2_dc_show)
})
do_differential_centrality_size <- eventReactive(input$T2_dc_button, {
    size <- paste(floor(input$T2_dc_top*24), "px", sep="")
    plotOutput("T2_dc_plot", height=size)
})
output$T2_dc_plot_ui <- renderUI({
    do_differential_centrality_size()
})
output$T2_dc_plot <- renderPlot({
    do_differential_centrality_plot()
})
output$T2_dc_ggwrs<- renderPlot({
    ggwrs(n=length(oracle.get("nodes")), p=input$T2_dc_exponent) +
    theme_simplex(dark=TRUE)
})

# Importance -------------------------------------------------- 

output$T2_pca_varexp <- renderPlot({network.pca.varexp(oracle.network(input$T2_pca_network))}, bg="#FCFCFC")
output$T2_pca_contri <- renderPlot({network.pca.contri(oracle.network(input$T2_pca_network))}, bg="#FCFCFC")
output$T2_pca_pltvar <- renderPlot({network.pca.pltvar(oracle.network(input$T2_pca_network))}, bg="#FCFCFC")
output$T2_pca_hclust <- renderPlot({network.pca.hclust(oracle.network(input$T2_pca_network))}, bg="#FCFCFC")
