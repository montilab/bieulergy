resources <- list()

col.blue <- "#2c698d"
col.red <- "#cc0022"
col.silver.l <- "#e9e9e9"
col.silver.d <- "#cfcfcf"

network_format <- c("dot", "gml", "graphml", "ncol")

network_layout <- c("Nicely" = "layout_nicely",
    "Tree" = "layout_as_tree",
    "Grid" = "layout_on_grid",
    "Star" = "layout_as_star",
    "Sphere" = "layout_on_sphere",
    "Circle" = "layout_in_circle",
    "Random" = "layout_randomly",
    "Davidson-Harel" = "layout_with_dh",
    "Fruchterman-Reingold" = "layout_with_fr",
    "Gem" = "layout_with_gem",
    "Graphopt" = "layout_with_graphopt",
    "Kamada-Kawai" = "layout_with_kk",
    "Sugiyama" = "layout_with_sugiyama",
    "Lgl" = "layout_with_lgl",
    "Mds" = "layout_with_mds")

node_centrality <- c("Degree" = "degree",
     "Eigen" = "eigen",
     "Betweenness" = "betweenness",
     "Dangalchev Closeness" = "dangalchev", 
     "Stress" = "stress",
     "Eccentricity" = "eccentricity",
     "Laplacian" = "laplacian",
     "Diffusion" = "diffusion",
     "Topological Coefficient" = "topoco",
     "Leverage" = "leverage",
     "Katz" = "katz")

node_similarity <- c("Jaccard" = "jaccard", 
     "Dice" = "dice", 
     "Inverse Log-Weighted" = "invlogweighted")
