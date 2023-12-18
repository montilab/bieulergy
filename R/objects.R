#' Create interactive omics network objects
#' 
#' @param x Network data
#' @param type Type of input data
#' 
#' @return A interactive omics networks object
#' 
#' @importFrom igraph graph_from_adjacency_matrix is_simple simplify
#' 
#' @export
create.ionet <- function(x, type=c("ig", "adj", "wadj", "onet"), ...) {
    if (type == "ig") {
        ig <- x
    } else if (type == "adj") {
        ig <- igraph::graph_from_adjacency_matrix(x, mode="undirected", diag=FALSE)
    } else if (type == "wadj") {
        ig <- igraph::graph_from_adjacency_matrix(x, mode="lower", diag=FALSE, weighted=TRUE)
    } else if (type == "onet") {
        ig <- x$ig
    } else {
        stop("Unsupported network type, please manually create interactive network object.")
    }
    if (!igraph::is_simple(ig)) {
        warning("Removing multi-edges and self loops...")
        ig <- igraph::simplify(ig, remove.multiple=TRUE, remove.loops=TRUE)
    }
    return(interactive.omics.network$new(ig, ...))
}

#' Create a list of interactive omics network objects
#' 
#' @param x A list of network data
#' @param type Type of input data
#' 
#' @return A list of interactive omics networks objects
#' 
#' @export
create.ionets <- function(x, type=c("ig", "adj", "wadj", "onet"), ...) {
    stopifnot(is(x, "list"))
    return(lapply(x, function(y) create.ionet(y, type, ...)))
}

#' Check if object is an interactive omics network objects
#' 
#' @return A bool
#' 
#' @export
is.ionet <- function(x) {
    is(x, "interactive.omics.network")
}

#' @title Interaction Omics Network Object
#' 
#' @importFrom R6 R6Class
#' 
#' @export
interactive.omics.network <- R6Class("interactive.omics.network", list(
    
    #' @field ig An igraph object
    #' @field pca Dimensionality reduction on node measures
    #' @field nodes A data frame of labeled nodes
    #' @field edges A data frame of undirected edges
    #' @field properties A list of graph-level properties
    ig         = NULL,    
    pca        = NULL,
    nodes      = NULL,
    edges      = NULL,
    properties = list(),

    #' Init
    #' @param ig An undirected igraph object
    #' @param symbols A mapping of node identifiers to symbols
    #' @param graph.measures A vector of graph measures to compute
    #' @param node.measures A vector of node measures to compute
    #' @param quiet Use true to silence messages
    #'
    #' @importFrom visNetwork toVisNetworkData
    #' @importFrom igraph delete_edge_attr
    #'
    #' @return A new network object
    initialize = function(ig,
                          symbols=NULL, 
                          graph.measures=c("nodes", "edges", "density", "clustering"), 
                          node.measures=c("degree", "eigen", "betweenness", "stress"),
                          quiet=TRUE) {
        
        stopifnot(is(ig, "igraph"))
        
        vn <- visNetwork::toVisNetworkData(ig)
        self$edges <- vn$edges
        self$nodes <- vn$nodes
    
        if (igraph::is_weighted(ig)) {
            ig <- igraph::delete_edge_attr(ig, "weight")
        }

        # Core graph object
        self$ig <- ig      
        
        self$init.properties(graph.measures, quiet)
        self$init.nodes(symbols, node.measures, quiet)
        self$init.edges()
    },
    #' @description
    #' Print network information
    #'
    #' @return NULL
    print = function() {
        print(self$ig)
        invisible(self)
    },
    
    #' @description
    #' Computes graph-level properties
    #'
    #' @param measures A vector of measures to compute
    #' @param quiet Use true to silence messages
    #'
    #' @return NULL
    init.properties = function(measures=c(), quiet=TRUE) {
        fx <- ig.properties()
        fx.do <- intersect(names(fx), measures)
        if (!quiet) cat("Computing graph-level statistics", "\n")
        for (i in fx.do) {
            if (!quiet) cat(" - ", i, "\n")
            v <- fx[[i]](self$ig)
            self$properties[[i]] <- v
        }
    },
    #' @description
    #' Computes node-level properties
    #'
    #' @param symbols A mapping of node identifers to symbols
    #' @param measures A vector of measures to compute
    #' @param quiet Use true to silence messages
    #'
    #' @return NULL
    #'
    #' @importFrom FactoMineR PCA
    init.nodes = function(symbols=NULL, measures=c(), quiet=TRUE) {
        if (is.null(symbols)) {
            self$nodes$symbol <- self$nodes$id
        } else {
            self$nodes$symbol <- symbols[self$nodes$id]
        }

        fx <- ig.centrality()
        fx.do <- intersect(names(fx), measures)
        if (!quiet) cat("Computing node-level statistics", "\n")
        for (i in fx.do) {
            if (!quiet) cat(" - ", i, "\n")
            v <- fx[[i]](self$ig)
            stopifnot(names(v) == self$nodes$id)
            self$nodes[,i] <- v
        }

        x <- self$nodes[,fx.do,drop=FALSE]
        x <- na.omit(x)
        self$pca <- FactoMineR::PCA(x, scale.unit=TRUE, ncp=10, graph=FALSE, axes=c(1:2))
    },
    #' @description
    #' Computes edge-level properties
    #'
    #' @return NULL
    init.edges = function() {
        if ("weight" %in% colnames(self$edges)) {
            self$edges$val <- self$edges$weight
            self$edges$weight <- 2
            #self$edges$color <- ifelse(self$edges$val > 0, "firebrick", "deepskyblue")
        } else {
            self$edges$weight <- 2
            self$edges$color  <- "grey"
        }
    },
    
    #' @description
    #' Maps ids to symbols
    #'
    #' @param ids A vector of node identifiers
    #'
    #' @return A vector of symbols
    get.symbols = function(ids) {
        symbols <- self$nodes$symbol[match(ids, self$nodes$id)]
        return(symbols)
    },

    #' @description
    #' Get interactors
    #'
    #' @param ids Node identifiers
    #' @param degree Edge degree to include in subset
    #' @param remove.ids Include original ids
    #' @param use.symbols Return node symbols instead of ids
    #'
    #' @return A vector of interactors
    #'
    #' @importFrom dplyr filter select
    get.interactors = function(ids, 
                               degree=1, 
                               remove.ids=TRUE, 
                               use.symbols=FALSE)
    {
        stopifnot(degree >= 1)
        if (degree == 1) {
            interactors <- 
                self$edges %>%
                dplyr::filter(from %in% ids | to %in% ids) %>%
                dplyr::select(from, to) %>%
                unlist(use.names=FALSE) %>%
                unique()
        } else {
            interactors <- ids
            for (i in seq_len(degree)) {
                branch <- self$get.interactors(interactors, remove.ids=FALSE)
                interactors <- unique(c(interactors, branch))
            }
        }
        if (remove.ids) {
            interactors <- setdiff(interactors, ids)
        }
        if (use.symbols) {
            interactors <- self$get.symbols(interactors)
        }
        return(interactors)
    },

    #' @description
    #' Get subnetwork
    #'
    #' @param ids Node identifiers
    #' @param degree Edge degree to include in subset
    #' @param indirect.edges Include edges between non-queried nodes
    #'
    #' @return A list of nodes and edges
    #'
    #' @importFrom dplyr filter select
    get.subnetwork = function(ids, 
                              degree=1, 
                              indirect.edges=FALSE) 
    {
        if (degree == 0) {
            edges <- 
                self$edges %>%
                dplyr::filter(from %in% ids & to %in% ids)
        }
        else if (degree == 1 & indirect.edges == FALSE) {
            edges <- 
                self$edges %>%
                dplyr::filter(from %in% ids | to %in% ids)
        } 
        else {
            ids <- self$get.interactors(ids, degree, remove.ids=FALSE)
            edges <-
                self$edges %>%
                dplyr::filter(from %in% ids & to %in% ids)
        }

        nodes <- 
            self$nodes %>%
            dplyr::filter(id %in% edges$to | id %in% edges$from)
        
        return(list(nodes=nodes, edges=edges))
    },
    
    #' @description
    #' Plot subnetwork
    #'
    #' @param ids Node identifiers
    #' @param degree Edge degree to include in subset
    #' @param indirect.edges Include edges between non-queried nodes
    #' @param use.symbols Show node symbols instead of ids
    #' @param node.color Node column to determine node background color
    #' @param node.border Node column to determine node border color
    #' @param node.size Node column to determine node size
    #' @param node.shape Node column to determine node shape
    #' @param layout An igraph layout
    #' @param node.attr Node attributes passed to `visNodes()`
    #' @param edge.attr Edge attributes passed to `visEdges()`
    #' @param ... Additional keyword arguments passed to `visNetwork()`
    #'
    #' @return A visNetwork object
    #'
    #' @importFrom dplyr mutate
    #' @importFrom visNetwork visNetwork visIgraphLayout visNodes visEdges
    plt.subnetwork = function(ids, 
                              degree=1, 
                              indirect.edges=FALSE,
                              use.symbols=FALSE,
                              node.color=NULL,
                              node.border=NULL,
                              node.size=NULL,
                              node.shape=NULL,
                              layout="layout_nicely",
                              node.attr=list(), 
                              edge.attr=list(),
                              ...) {
        
        shapes <- c("dot", "triangle", "box", "dot", "star", "diamond", "ellipse", "square")
        check.node.metric <- function(metric) {
            if (is(metric, "character")) {
                if (metric %in% colnames(nodes)) {
                    return(colorize(nodes[,metric]))
                }
            }
            if (is(metric, "list")) { 
                if (metric[[1]] %in% colnames(nodes) & is(metric[[2]], "function")) {
                    return(colorize(nodes[,metric[[1]]], pal=metric[[2]]))
                }
            }
        }
        
        data <- self$get.subnetwork(ids, degree, indirect.edges)
        
        nodes <- data$nodes
        edges <- data$edges
        
        if (nrow(nodes) > 0) {
            nodes %<>%
                dplyr::mutate(color=NULL) %>%
                dplyr::mutate(color.background=ifelse(id %in% ids, "#2c698d", "#e9e9e9")) %>%
                dplyr::mutate(color.border="#cfcfcf")
            
            if (!is.null(node.color)) {
                nodes$color.background <- check.node.metric(node.color)
            }
            if (!is.null(node.border)) {
                nodes$color.border <- check.node.metric(node.border)
                nodes$borderWidth <- 5
            }
            if (!is.null(node.size)) {
                if (node.size %in% colnames(nodes)) {
                    nodes$size <- normalize.range(nodes[,node.size], a=15, b=50)   
                }
            }
            if (!is.null(node.shape)) {
                if (node.shape %in% colnames(nodes)) {
                    shape.factor <- as.factor(nodes[,node.shape])
                    if (length(unique(shape.factor)) <= length(shapes)) {
                        nodes$shape <- shapes[shape.factor]
                    }
                }
            }
            if (use.symbols) {
                nodes$label <- nodes$symbol
            }
        }
        
        vn <- visNetwork::visNetwork(nodes, edges, ...) %>%
              visNetwork::visIgraphLayout(layout=layout)
        
        vn <- do.call(visNetwork::visNodes, c(list(graph=vn), node.attr))
        vn <- do.call(visNetwork::visEdges, c(list(graph=vn), edge.attr))
        return(vn)
    }
))
