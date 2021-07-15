# Available datasets
datasets <- c("Yeast-Networks")

# Loaded by default
dataset <- "Yeast-Networks"

storage <- list()

store.data <- function(storage, dataset, data, node=NULL) {
    storage[[dataset]] <- list()
    storage[[dataset]]$data <- data
    storage[[dataset]]$networks <- names(data)
    storage[[dataset]]$network <- storage[[dataset]]$networks[[1]]
    storage[[dataset]]$nodes <- data[[1]]$nodes$id
    storage[[dataset]]$node <- ifelse(is.null(node), storage[[dataset]]$nodes[[1]], node)
    return(storage)
}

storage <- store.data(storage, datasets[[1]], data=readRDS(file.path(system.file("extdata", package="bieulergy"), "yeast-networks.rds")))

# Variables used to intialize ui data fields (obsolete on dataset change)
data     <- storage[[dataset]]$data
networks <- storage[[dataset]]$networks
network  <- storage[[dataset]]$network
nodes    <- storage[[dataset]]$nodes
node     <- storage[[dataset]]$node

# Check which centrality measures are available
available_centrality_measures <- node_centrality[node_centrality %in% colnames(data[[1]]$nodes)]

# The oracle manages reactive data (server side)
oracle <- reactiveValues(
    data     = data,
    networks = networks,
    network  = network,
    nodes    = nodes,
    node     = node
)

# Helper functions for communicating with the oracle
oracle.get <- function(x) {
    oracle[[x]]
}
oracle.data <- function() {
    oracle.get("data")
}
oracle.network <- function(network) {
    oracle.data()[[network]]
}
oracle.networks <- function(networks) {
    oracle.data()[networks]
}
oracle.find <- function(network, item) {
    oracle.network(network)[[item]]
}
oracle.centrality <- function() {
    node_centrality[node_centrality %in% colnames(oracle.data()[[1]]$nodes)]
}
