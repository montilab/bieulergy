# Simulated networks from yeast data by Kristina Hanspers
# @UUID: 7831a991-5767-11ea-bfdc-0ac135e8bacf
# https://www.ndexbio.org/viewer/networks/7831a991-5767-11ea-bfdc-0ac135e8bacf
# @Nodes: 331
# @Edges: 362
# @Created: Feb 24, 2020 7:40:49 PM
# @Last Modified: Feb 28, 2020 1:22:00 PM
library(ndexr)
ndexcon <- ndex_connect()
data <- ndex_get_network(ndexcon, "7831a991-5767-11ea-bfdc-0ac135e8bacf")

library(igraph)
mat <- as.matrix(data$edges)
storage.mode(mat) <- "character" # Important for igraph
ig <- igraph::graph_from_edgelist(mat[,c("s", "t")], directed=FALSE)
ig <- igraph::simplify(ig, remove.multiple=TRUE, remove.loops=TRUE)
ids <- data$nodes[match(as.numeric(igraph::as_ids(V(ig))), data$nodes[,"@id"]), "n"] # Yeast systematic name
yeast.genome <- read.delim2(url("https://downloads.yeastgenome.org/curation/chromosomal_feature/SGD_features.tab"), quote="", header=FALSE)
symbols <- yeast.genome$V5[match(ids, yeast.genome$V4)]
symbols[is.na(symbols)] <- ""
V(ig)$symbol <- symbols
names(symbols) <- V(ig)$name

# Add in real transcription factors (TFs)
# Transcription Factor Consensus List from Yeastract
# Source: http://www.yeastract.com/consensuslist.php
tfs.data <- read.delim("inst/script/data/yeast-tfs.csv", sep=",", header=F)
tfs <- toupper(tfs.data$V1)
tfs <- gsub('.{1}$', '', tfs)
V(ig)$is_tf <- !is.na(match(V(ig)$symbol, tfs))

# Simulated Differential Expression
set.seed(1)
vals <- rnorm(igraph::vcount(ig), mean=0, sd=0.7)
plot(density(vals))
V(ig)$lfc_mrna <- vals

# Simulated SNP Frequencies
set.seed(1)
vals <- abs(rnorm(igraph::vcount(ig), mean=0, sd=0.3))^2
plot(density(vals))
V(ig)$snp_frq <- vals

igraph::as_data_frame(ig, what="vertices") %>%
head()

yeast.0 <- ig
yeast.1 <- rewire(ig, igraph::each_edge(p=0.15, loops = FALSE))
yeast.2 <- rewire(ig, igraph::each_edge(p=0.30, loops = FALSE))
ionet <- create.ionet(yeast.0, type="ig", symbols=symbols)

yeasts <- list("Yeast_1"=yeast.0, "Yeast_2"=yeast.1, "Yeast_3"=yeast.2)
ionets <- create.ionets(yeasts, type="ig", symbols=symbols)

#saveRDS(ionets, file.path(system.file("extdata", package="bieulergy"), "yeast-networks.rds"))
