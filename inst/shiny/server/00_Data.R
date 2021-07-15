update.inputs <- function(session) {
    
    # Data
    updateSelectInput(session, "T0_network", choices=oracle$networks)

    # Centrality
    updateSelectInput(session, "T2_pca_network", choices=oracle$networks, selected=oracle$network)
    updateCheckboxGroupInput(session, "T2_visualization_networks", choices=oracle$networks, selected=oracle$networks)
    updateSelectInput(session, "T2_tnodes_selected", choices=oracle$nodes)
    updateSelectInput(session, "T2_enrichment_network", choices=oracle$networks, selected=oracle$network)
    updateCheckboxGroupInput(session, "T2_dc_networks", choices=oracle$networks, selected=oracle$networks)
    updateSelectInput(session, "T2_cdistr_metric", choices=oracle.centrality())
    updateSelectInput(session, "T2_tnodes_metric", choices=oracle.centrality())
    updateSelectInput(session, "T2_hclust_metric", choices=oracle.centrality())
    updateSelectInput(session, "T2_enrichment_metric", choices=oracle.centrality())
    updateSelectInput(session, "T2_dc_metric", choices=oracle.centrality())
    
    # Visualize
    updateSelectInput(session, "T3_network", choices=oracle$networks)
    updateSelectInput(session, "T3_nodes", choices=oracle$nodes, selected=oracle$node)
    updateSelectizeInput(session, "T3_c_networks", choices=oracle$networks, selected=oracle$networks[1:2])
    updateSelectInput(session, "T3_c_nodes", choices=oracle$nodes, selected=oracle$node)
}

observeEvent(input$T0_upload_file, {
    fn <- input$T0_upload_file$datapath
    label <- gsub(".rds", "", input$T0_upload_file$name)

    tryCatch(
        {
            file.obj <- readRDS(fn)
            
            # Must be a list of one or more interactive network objects
            stopifnot(is(file.obj, "list"))
            stopifnot(sapply(file.obj, is.ionet))

            datasets <- c(datasets, label)

            print(datasets)
            updateSelectInput(session, "T0_dataset", choices=datasets, selected=label)
            oracle$data     <- file.obj
            oracle$networks <- names(file.obj)
            oracle$network  <- names(file.obj)[[1]]
            oracle$nodes    <- file.obj[[1]]$ids
            oracle$node     <- file.obj[[1]]$ids[[1]]
            update.inputs(session)
        },
        error = function(e) {
            stop(safeError(e))
        }
    )
})

observeEvent(input$T0_change, {
    dat <- storage[[input$T0_dataset]]
    oracle$data     <- dat$data
    oracle$networks <- names(dat$data)
    oracle$network  <- names(dat$data)[[1]]
    oracle$nodes    <- dat$data[[1]]$ids
    oracle$node     <- dat$data[[1]]$ids[[1]]
    update.inputs(session)
    updateTabsetPanel(session, "data_tabs", selected="View Loaded Dataset")
})

output$T0_export <- downloadHandler(
    filename = function() {
        paste(input$T0_dataset, "rds", sep=".")
    },
    content = function(file) {
        saveRDS(oracle$data, file)
    }
)

output$T0_stats <- renderUI({
    data <- oracle.data()
    stats <- lapply(data, function(x) x$properties)

    df <- stats %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(network=names(data)) %>%
        dplyr::select(c("network", colnames(.))) %>%
        dplyr::mutate_if(function(x) {is.numeric(x) && x >= 1}, round, digits=2) %>%
        dplyr::mutate_if(function(x) {is.numeric(x) && x < 1}, signif, digits=3)

    tbl <- reactable(df, 
              searchable=FALSE,
              resizable=TRUE,
              compact=TRUE, 
              fullWidth=TRUE,
              defaultPageSize=25,
              defaultColDef=colDef(headerClass="rctbl-header"),
              style=list(backgroundColor="#FCFCFC"),
              showPageSizeOptions=TRUE,
              rowStyle=list(cursor="pointer"))

    dat <- htmltools::div(class="rctbl-obj-teeny", tbl)
    return(dat)
})

output$T0_nodes <- renderUI({
    nodes <- oracle.find(input$T0_network, "nodes")
    if (!is.null(nodes)) {
        df <- nodes %>%
            dplyr::arrange(desc(degree)) %>%
            dplyr::select(-one_of(c("label"))) %>% 
            dplyr::mutate_if(function(x) {is.numeric(x) && x >= 1}, round, digits=2) %>%
            dplyr::mutate_if(function(x) {is.numeric(x) && x < 1}, signif, digits=3)
        
        tbl <- reactable(df,
                  searchable=TRUE,
                  resizable=TRUE, 
                  compact=TRUE, 
                  fullWidth=TRUE,
                  defaultPageSize=15,
                  defaultColDef=colDef(headerClass="rctbl-header"),
                  pageSizeOptions=c(15, 25, 50, 100),
                  style=list(backgroundColor="#FCFCFC"),
                  showPageSizeOptions=TRUE,
                  rowStyle=list(cursor="pointer"))

        dat <- htmltools::div(class="rctbl-obj-teeny", tbl)
        return(dat)
    }
})
