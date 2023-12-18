#' Random walk with restart
#'
#' @param ig An igraph object
#' @param seeds One or more seed node ids
#' @param restart Restart probability between 0-1
#' 
#' @return A vector
#' 
#' @importFrom igraph V vcount
#' @importFrom dnet dRWR
#' 
#' @export
ig.rwr <- function(ig, seeds, restart=0.5) {
    mat <- matrix(0, nrow=igraph::vcount(ig), ncol=1)
    rownames(mat) <- igraph::V(ig)$name
    mat[,1] <- as.integer(rownames(mat) %in% seeds)
    rwr <- dnet::dRWR(g=ig, normalise="laplacian", setSeeds=mat, restart=restart, verbose=FALSE)
    p <- rwr[,1]
    names(p) <- rownames(mat)
    return(p)
}
