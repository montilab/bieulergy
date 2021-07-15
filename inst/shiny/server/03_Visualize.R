# Visualize -------------------------------------------------- 

get.network <- function(network,
                        ids=c(), 
                        degree=1, 
                        indirect.edges=FALSE, 
                        use.symbols=TRUE, 
                        global=FALSE,
                        rmv.isolates=TRUE) {
    
    ig <- network$ig
    
    # Node-level information
    nodes <- network$nodes
    nodes$degree <- igraph::degree(ig)
    
    # Sub-network
    edges <- network$get.subnetwork(ids, degree, indirect.edges)$edges
    targets <- network$get.interactors(ids, degree, remove.ids=TRUE) 

    if (!global) {
        nodes <- nodes[nodes$id %in% c(ids, targets),]
    }    
    if (rmv.isolates) {
        nodes <- nodes[nodes$degree > 0,]
    }
    if (use.symbols) {
        nodes$label <- nodes$symbol
    }

    # Styling
    nodes %<>%
        dplyr::mutate(color=NULL) %>%
        dplyr::mutate(color.background="rgba(255,255,255,0.3)") %>%
        dplyr::mutate(color.border="rgba(0,0,0,0.3)")

    nodes$color.background[nodes$id %in% targets] <- "#EEEEEE"
    nodes$color.background[nodes$id %in% ids] <- "#2C698D"
    
    return(list(nodes=nodes, edges=edges))
}

reactive_visualize <- eventReactive(input$T3_visualize, {
    network <- oracle.network(input$T3_network)
    
    vn <- get.network(network,
                      ids=input$T3_nodes, 
                      degree=input$T3_degree, 
                      indirect.edges=input$T3_target_edges, 
                      use.symbols=input$T3_symbols, 
                      global=input$T3_global,
                      rmv.isolates=input$T3_remove_isolates)

    # Network propagation
    if (input$T3_rwr) {
      seeds <- input$T3_nodes
      ig <- vn.to.ig(vn$nodes, vn$edges)
      p <- ig.rwr(ig, seeds, restart=input$T3_restart)
      vn$nodes$p <- p[match(vn$nodes$id, names(p))]
      vn$nodes$pc <- vn$nodes$p
      vn$nodes$pc[vn$nodes$id %in% seeds] <- 0 
      vn$nodes$color <- colorize.heat(vn$nodes$pc)
      vn$nodes$color[vn$nodes$id %in% seeds] <- "#2C698D"
    }

    return(vn)
})

output$T3_plot <- renderVisNetwork({
    vn <- reactive_visualize()
    set.seed(input$T3_seed)
    visNetwork(vn$nodes, vn$edges, width=NULL, height=NULL) %>%
    visNodes(size=input$T3_node_size, font=list(size=input$T3_font_size)) %>%
    visEdges(color="rgba(0,0,0,0.5)", width=input$T3_edge_width) %>%
    visPhysics(enabled=FALSE) %>%
    visEdges(smooth=FALSE) %>%
    visIgraphLayout(layout=input$T3_layout)
})

# Propagation -------------------------------------------------- 

propagation <- eventReactive(input$T3_visualize, {    
    vn <- reactive_visualize()
    tab <- vn$nodes %>%
           dplyr::select(id, symbol, p) %>%
           dplyr::arrange(desc(p)) %>%
           dplyr::mutate(p = signif(p, 3)) %>%
           magrittr::set_colnames(toupper(colnames(.))) %>%
           reactable(searchable=TRUE,
                     compact=FALSE, 
                     fullWidth=TRUE,
                     resizable=TRUE,
                     rownames=FALSE,
                     defaultPageSize=15,
                     pageSizeOptions=c(15, 25, 50, 100),
                     striped=TRUE,
                     style=list(backgroundColor="#FCFCFC"),
                     showPageSizeOptions=TRUE)

    return(tab)
})

output$T3_propagation <- renderReactable({
    propagation()
})

# Enrichment -------------------------------------------------- 

reactive_genesets <- genesets_Server("T3_genesets", clean=TRUE)

enrichment <- eventReactive(input$T3_hyper, {
    if (length(input$T3_nodes) == 0) return()
    if (!input$T3_rwr) return()

    vn <- reactive_visualize()
    signature <- vn$nodes$p 
    names(signature) <- vn$nodes$symbol
    signature.ranked <- names(signature[order(-signature)])
    genesets <- reactive_genesets()

    hyp <- hypeR(signature.ranked, genesets, test="kstest", plotting=FALSE)

    tab <- hyp$data %>%
           magrittr::set_colnames(toupper(colnames(.))) %>%
           reactable(searchable=TRUE,
                     compact=FALSE, 
                     fullWidth=TRUE,
                     resizable=TRUE,
                     rownames=FALSE,
                     defaultPageSize=15,
                     pageSizeOptions=c(15, 25, 50, 100),
                     striped=TRUE,
                     style=list(backgroundColor="#FCFCFC"),
                     showPageSizeOptions=TRUE)

    return(tab)
})

output$T3_enrichment <- renderReactable({
    enrichment()
})

observeEvent(input$T3_hyper, {
    if (length(input$T3_nodes) == 0) return()
    if (!input$T3_rwr) return()
    updateTabsetPanel(session, "visualize_tabs", selected="Network Propagation")
})

# Export -------------------------------------------------- 

output$T3_export <- downloadHandler(
    filename = function() {
        paste0(paste(input$T3_nodes, collapse="_"), ".rds")
    },
    content = function(file) {
        saveRDS(reactive_visualize(), file)
    }
)

# Compare -------------------------------------------------- 

compare.plot <- function(np, no, origins, font.size, symbols) {
    
    network.p <- oracle.network(np)
    networks.o <- oracle.networks(no)

    targets.primary <- network.p$get.interactors(origins)  
    targets.others <- lapply(networks.o, function(n) n$get.interactors(origins)) %>%
                      unlist() %>%
                      unique()

    targets.union <- unique(c(targets.primary, targets.others))
    targets.unique <- setdiff(targets.primary, targets.others)

    net <- network.p$get.subnetwork(origins)
    edges <- net$edges
    nodes <- net$nodes %>%
             mutate(color.border=col.silver.d) %>%
             mutate(font.family="Helvetica") %>%
             mutate(font.size=font.size) %>%
             mutate(color.background=case_when(label %in% origins ~ col.blue,
                                               label %in% targets.unique ~ col.red,
                                               TRUE ~ col.silver.l))

    if (symbols) {
        nodes$label <- nodes$symbol
    }

    visNetwork(nodes, edges) %>% 
    visIgraphLayout(layout="layout_nicely", randomSeed=123) %>%
    visNodes(labelHighlightBold=TRUE)
}

compare.plots <- function(name.primary) {
    if (length(input$T3_c_nodes) == 0) return()
    if (!(name.primary %in% input$T3_c_networks)) return()
    name.others <- setdiff(input$T3_c_networks, name.primary)
    compare.plot(name.primary, name.others, input$T3_c_nodes, input$T3_c_label_size, input$T3_c_symbols)
}

output$T3_c_plots <- renderUI({
    size <- paste(input$T3_c_network_size, "px", sep="")
    input$T3_c_networks %>%
    lapply(function(x) {
        div(
            tags$b(x),
            visNetworkOutput(paste("compare_plot", x, sep=""), 
                             width=size, 
                             height=size), 
                             style = "display: inline-block;")
    }) %>%
    do.call(tagList, .)
})

observeEvent(input$T3_c_networks, {
    for (i in input$T3_c_networks) {
        local({
            my_i <- i # Need this for some reason
            plotname <- paste("compare_plot", my_i, sep="")
            output[[plotname]] <- renderVisNetwork({
                compare.plots(my_i)
            })
        })
    }
})
