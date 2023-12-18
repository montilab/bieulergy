#' Graph-level vertex count
#'
#' @param ig An igraph object
#' 
#' @return A value
#' 
#' @importFrom igraph vcount
#' 
#' @export
ig.nodes <- function(ig) {
    igraph::vcount(ig)
}

#' Graph-level edge count
#'
#' @param ig An igraph object
#' 
#' @return A value
#' 
#' @importFrom igraph ecount
#' 
#' @export
ig.edges <- function(ig) {
    igraph::ecount(ig)
}

#' Graph-level density
#'
#' @param ig An igraph object
#' 
#' @return A value
#' 
#' @importFrom igraph ecount
#' 
#' @export
ig.density <- function(ig) {
    igraph::edge_density(ig)
}

#' Graph-level transitivity
#'
#' @param ig An igraph object
#' 
#' @return A value
#' 
#' @importFrom igraph transitivity
#' 
#' @export
ig.transitivity <- function(ig) {
    igraph::transitivity(ig, type="globalundirected")
}

#' Graph-level clustering
#'
#' @param ig An igraph object
#' 
#' @return A value
#' 
#' @importFrom igraph transitivity
#' 
#' @export
ig.clustering <- function(ig) {
    mean(igraph::transitivity(ig, type="localundirected"), na.rm=TRUE)
}

#' Graph-level diameter
#'
#' @param ig An igraph object
#' 
#' @return A value
#' 
#' @importFrom igraph diameter
#' 
#' @export
ig.diameter <- function(ig) {
    igraph::diameter(ig, directed=FALSE)
}

#' Graph-level components
#'
#' @param ig An igraph object
#' 
#' @return A value
#' 
#' @importFrom igraph count_components
#' 
#' @export
ig.components <- function(ig) {
    igraph::count_components(ig)    
}

#' Graph-level assortivity
#'
#' @param ig An igraph object
#' 
#' @return A value
#' 
#' @importFrom igraph assortativity_degree
#' 
#' @export
ig.assortivity <- function(ig) {
    igraph::assortativity_degree(ig, directed=FALSE)
}

#' Graph-level efficiency
#'
#' @param ig An igraph object
#' 
#' @return A value
#' 
#' @importFrom igraph distances
#' 
#' @export
ig.efficiency <- function(ig) {
    d <- igraph::distances(ig)
    diag(d) <- NA
    mean(1/d, na.rm=TRUE)
}

#' Graph-level isolated
#'
#' @param ig An igraph object
#' 
#' @return A value
#' 
#' @importFrom igraph degree
#' 
#' @export
ig.isolated <- function(ig) {
    sum(igraph::degree(ig) == 0)
}

#' Node-level degree centrality
#'
#' @param ig An igraph object
#' 
#' @return A vector
#' 
#' @importFrom igraph degree
#' 
#' @export
ig.degree <- function(ig) {
    igraph::degree(ig)
}

#' Node-level eigenvector centrality
#'
#' @param ig An igraph object
#' 
#' @return A value
#' 
#' @importFrom igraph eigen_centrality
#' 
#' @export
ig.eigen <- function(ig) {
    igraph::eigen_centrality(ig, directed=FALSE, scale=TRUE)$vector
}

#' Node-level eccentricity centrality
#'
#' @param ig An igraph object
#' 
#' @return A vector
#' 
#' @importFrom igraph eccentricity
#' 
#' @export
ig.eccentricity <- function(ig) {
    igraph::eccentricity(ig)
}

#' Node-level laplacian centrality
#'
#' @param ig An igraph object
#' 
#' @return A vector
#' 
#' @importFrom centiserve laplacian
#' 
#' @export
ig.laplacian <- function(ig) {
    centiserve::laplacian(ig, loops=FALSE)   
}

#' Node-level leverage centrality
#'
#' @param ig An igraph object
#' 
#' @return A vector
#' 
#' @importFrom centiserve leverage
#' 
#' @export
ig.leverage <- function(ig) {
    centiserve::leverage(ig, loops=FALSE) 
}

#' Node-level dangalchev closeness centrality
#'
#' @param ig An igraph object
#' 
#' @return A vector
#' 
#' @importFrom centiserve closeness.residual
#' 
#' @export
ig.dangalchev <- function(ig) {
    centiserve::closeness.residual(ig)
}

#' Node-level diffusion centrality
#'
#' @param ig An igraph object
#' 
#' @return A vector
#' 
#' @importFrom centiserve diffusion.degree
#' 
#' @export
ig.diffusion <- function(ig) {
    centiserve::diffusion.degree(ig)
}

#' Node-level topological coefficient centrality
#'
#' @param ig An igraph object
#' 
#' @return A vector
#' 
#' @importFrom centiserve topocoefficient
#' 
#' @export
ig.topoco <- function(ig) {
    centiserve::topocoefficient(ig)
}

#' Node-level katz centrality
#'
#' @param ig An igraph object
#' 
#' @return A vector
#' 
#' @importFrom centiserve katzcent
#' 
#' @export
ig.katz <- function(ig) {
    centiserve::katzcent(ig)
}

#' Node-level betweeness centrality
#'
#' @param ig An igraph object
#' 
#' @return A vector
#' 
#' @importFrom igraph V as_ids as_adjacency_matrix
#' @importFrom magrittr %>% set_names
#' @importFrom sna betweenness
#' 
#' @export
ig.betweeness <- function(ig) {
    ig %>%
    igraph::as_adjacency_matrix() %>%
    as.matrix() %>%
    sna::betweenness(gmode="graph", rescale=FALSE) %>%
    magrittr::set_names(as_ids(V(ig)))
}

#' Node-level stress centrality
#'
#' @param ig An igraph object
#' 
#' @return A vector
#' 
#' @importFrom igraph V as_ids as_adjacency_matrix
#' @importFrom magrittr %>% set_names
#' @importFrom sna stresscent
#' 
#' @export
ig.stress <- function(ig) {
    ig %>%
    igraph::as_adjacency_matrix() %>%
    as.matrix() %>%
    sna::stresscent(gmode="graph", rescale=FALSE) %>%
    magrittr::set_names(as_ids(V(ig)))
}
