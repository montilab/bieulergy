#' Helper function for creating graph objects
#'
#' @param data Data object
#' @param type Type of object
#' 
#' @return A list of named functions
#' 
#' @importFrom igraph graph_from_adjacency_matrix
#' 
#' @export
ig.create <- function(data, type=c("ig", "adj", "wadj")) {
    switch (match.arg(type),
        "ig"   = data,
        "adj"  = igraph::graph_from_adjacency_matrix(data, mode="undirected", diag=FALSE),
        "wadj" = igraph::graph_from_adjacency_matrix(data, mode="lower", diag=FALSE, weighted=TRUE),
    )
}

#' Convert ig to vn
#'
#' @param ig An ig object
#' 
#' @return An vn object
#' 
#' @importFrom visNetwork toVisNetworkData
#' 
#' @export
ig.to.vn <- function(ig) {
    visNetwork::toVisNetworkData(ig)
}

#' Convert vn to ig
#'
#' @param nodes A dataframe of nodes
#' @param edges A dataframe of edges
#' 
#' @return An ig object
#' 
#' @importFrom igraph graph_from_data_frame
#' 
#' @export
vn.to.ig <- function(nodes, edges) {
    igraph::graph_from_data_frame(edges, directed=FALSE, vertices=nodes$id)
}

#' Functions for calculating graph-level properties
#'
#' @return A list of named functions
#' 
#' @export
ig.properties <- function() {
    list(
        "nodes"        = ig.nodes,
        "edges"        = ig.edges,
        "diameter"     = ig.diameter,
        "components"   = ig.components,
        "isolated"     = ig.isolated,
        "density"      = ig.density,
        "transitivity" = ig.transitivity,
        "clustering"   = ig.clustering,
        "assortivity"  = ig.assortivity,
        "efficiency"   = ig.efficiency
    )
}

#' Functions for calculating node-level centrality measures
#'
#' @return A list of named functions
#' 
#' @export
ig.centrality <- function() {
    list(
        "degree"       = ig.degree,
        "eigen"        = ig.eigen,
        "betweenness"  = ig.betweeness,
        "stress"       = ig.stress,
        "eccentricity" = ig.eccentricity,
        "laplacian"    = ig.laplacian,
        "dangalchev"   = ig.dangalchev,
        "diffusion"    = ig.diffusion,
        "topoco"       = ig.topoco,
        "leverage"     = ig.leverage,
        "katz"         = ig.katz
    )
}
