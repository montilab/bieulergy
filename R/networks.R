#' Checks if object is a interactive omics network object
#'
#' @param network An object
#' 
#' @return A bool
#' 
#' @export
is.network <- function(network) {
    if (!is.ionet(network)) {
        stop("Detected a non-network object ")
    }
    return(TRUE)
}

#' Checks if object is a list of interactive omics network objects
#'
#' @param networks A list of objects
#' 
#' @return A bool
#' 
#' @export
is.networks <- function(networks) {
    if (!is(networks, "list")) {
        stop("Expected a list of network objects")
    }
    if (length(networks) == 0) {
        stop("Expected at least one network objects")
    }
    sh <- lapply(networks, is.network)
    return(TRUE)
}

#' PCA - Hierarchical Clustering
#' 
#' @param network An interactive omics network object
#' @return A ggplot object
#' 
#' @importFrom factoextra get_dist hcut fviz_dend
#' @importFrom ggplot2 theme element_text
#' 
#' @export
network.pca.hclust <- function(network) {
    nodes <- network$nodes
    available.measures <- base::intersect(names(ig.centrality()), colnames(nodes))
    x <- nodes[,available.measures]
    x <- na.omit(x)
    x.scaled <- scale(x, center=TRUE, scale=TRUE)
    x.dist <- factoextra::get_dist(t(x.scaled), method="euclidean")
    k.colors <- c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07")
    n.measures <- length(available.measures)
    if (n.measures >= 6) {
        k <- 4
    } else if (n.measures >= 4) {
        k <- 3
    } else {
        k <- 2
    }
    x.clust <- factoextra::hcut(x.dist, k=k, stand=TRUE)
    p <- factoextra::fviz_dend(x.clust, horiz=TRUE, rect=TRUE, cex=0.7, k_colors=k.colors[1:k]) +
         ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5)) +
         theme_simplex()

    return(p)
}

#' PCA - Variance Explained
#' 
#' @param network An interactive omics network object
#' 
#' @return A ggplot object
#' 
#' @importFrom factoextra fviz_screeplot
#' @importFrom ggplot2 labs theme_bw theme element_text
#' 
#' @export
network.pca.varexp <- function(network) {
    pca <- network$pca
    p <- factoextra::fviz_screeplot(pca,
                                    barfil="#E7B800", 
                                    barcol="#E7B800") +
         ggplot2::theme_bw() +
         ggplot2::labs(title="Variance Explained", x="PC", y="Percent") +
         ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5)) +
         theme_simplex()
    
    return(p)
}

#' PCA - Contibutors
#' 
#' @param network An interactive omics network object
#' 
#' @return A ggplot object
#' 
#' @importFrom factoextra fviz_contrib
#' @importFrom ggplot2 labs theme_bw theme element_text
#' 
#' @export
network.pca.contri <- function(network) {
    pca <- network$pca
    p <- factoextra::fviz_contrib(pca, 
                                  choice="var", 
                                  axes=c(1,2), 
                                  color="#00AFBB", 
                                  fill="#00AFBB") +
    ggplot2::theme_bw() +
    ggplot2::labs(title="Contribution to Principal Components", x="", y="Contribution") + 
    ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.5),
          axis.text.x=ggplot2::element_text(angle=45, vjust=0.5)) +
    theme_simplex()
    
    p$layers[[2]] <- NULL # Remove line
    return(p)
}

#' PCA - Show Contributors
#' 
#' @param network An interactive omics network object
#' 
#' @return A ggplot object
#' 
#' @importFrom factoextra fviz_pca_var
#' @importFrom ggplot2 labs theme_bw theme element_text element_blank
#' 
#' @export
network.pca.pltvar <- function(network) {
    pca <- network$pca
    p <- factoextra::fviz_pca_var(pca, 
                                  axes=c(1,2),
                                  col.var="contrib", 
                                  repel=TRUE,
                                  gradient.cols=c("#00AFBB", "#E7B800", "#FC4E07")) +
         ggplot2::theme_bw() +
         ggplot2::labs(title="Variable Contributions") +
         theme(plot.title=ggplot2::element_text(hjust=0.5),
               legend.title=ggplot2::element_blank()) +
         theme_simplex()
    
    return(p)
}

#' Centrality-based enrichment
#' 
#' @param network An interactive omics network object
#' @param metric Centrality measure
#' @param geneset A list of genes or other omics features
#' 
#' @return A ggplot object
#' 
#' @importFrom dplyr arrange select sym
#' @importFrom tibble deframe
#' @importFrom magrittr %>%
#' 
#' @export
network.kstest <- function(network, metric, geneset) {
    signature <- network$nodes %>%
                 dplyr::arrange(desc(!!sym(metric))) %>%
                 dplyr::select(symbol) %>%
                 tibble::deframe()
    
    ranks <- match(geneset, signature)
    ranks <- ranks[!is.na(ranks)]
    results <- kstest(n.x=length(signature), y=ranks, plotting=TRUE)
    return(results$plot)
}

#' Centrality distributions
#' 
#' @param networks A named list of interactive omics network objects
#' @param metric Centrality measure
#' @param plot Plot type
#' @param rescale Rescale values between 0-1
#' 
#' @return A ggplot object
#' 
#' @importFrom dplyr select bind_rows
#' @importFrom stringr str_to_title
#' @importFrom ggpubr ggboxplot ggdensity ggecdf stat_compare_means
#' @importFrom magrittr %>%
#' @importFrom ggplot2 theme element_blank
#' 
#' @export
networks.cdistr <- function(networks, 
                            metric, 
                            plot=c("boxplot", "density", "ecdf"),
                            rescale=FALSE) {
    
    # Check for network object(s)
    is.networks(networks)

    # Default args
    plot <- match.arg(plot)

    df <- mapply(function(x, y) {
        df <- dplyr::select(x$nodes, (!!metric)) %>%
              magrittr::set_colnames("value")
        if (rescale) {
            df$value <- df$value/sum(df$value)
        }
        colnames(df) <- metric
        df$network <- y
        return(df)
    }, networks, names(networks), SIMPLIFY=FALSE) %>%
    dplyr::bind_rows()

    if (plot == "boxplot") {
        p <- ggpubr::ggboxplot(df, x="network", y=metric, fill="network", add="jitter") + 
             ggpubr::stat_compare_means() +
             labs(x="", y=stringr::str_to_title(metric))
    }
    if (plot == "density") {
        p <- ggpubr::ggdensity(df, x=metric, color="network", fill="network", add="mean", rug=TRUE) +
             labs(y="Density", x=stringr::str_to_title(metric))
    }
    if (plot == "ecdf") {
        p <- ggpubr::ggecdf(df, x=metric, color="network") + 
             labs(x=stringr::str_to_title(metric))
    }

    p +
    ggplot2::theme(legend.title=ggplot2::element_blank()) +
    theme_simplex()
}

#' Important Nodes
#' 
#' @param networks A named list of interactive omics network objects
#' @param metric Centrality measure
#' @param top Number of nodes to label
#' @param size Size of labels
#' @param search Include specific nodes for labeling
#' @param log Log-transform y-axis
#' @param reverse Flip y-axis
#' @param symbols Show symbols instead of node identifiers
#' @param shared Highly shared nodes only
#' 
#' @return A ggplot object
#' 
#' @importFrom dplyr select filter bind_cols mutate if_else
#' @importFrom plyr rbind.fill
#' @importFrom reshape2 melt
#' @importFrom ggpubr ggboxplot ggdensity ggecdf stat_compare_means
#' @importFrom magrittr %>% set_colnames
#' @importFrom ggrepel geom_label_repel
#' @importFrom ggplot2 aes geom_jitter xlab ylab theme theme_bw element_text element_blank scale_y_continuous scale_y_reverse
#' @importFrom stringr str_to_title
#' 
#' @export
networks.tnodes <- function(networks, 
                            metric, 
                            top=15, 
                            size=3,
                            search=NULL,
                            log=FALSE, 
                            reverse=FALSE,
                            symbols=FALSE, 
                            shared=FALSE) {

    # Check for network object(s)
    is.networks(networks)
    
    # Data
    df <- mapply(function(x, y) {
        dplyr::select(x$nodes, (!!metric)) %>%
        magrittr::set_colnames(y)
    }, networks, names(networks), SIMPLIFY=FALSE) %>%
    dplyr::bind_cols()

    p <- df %>%
         reshape2::melt(id.vars=NULL) %>%
         ggplot2::ggplot(aes(x=variable, y=value)) + 
         ggplot2::geom_jitter() +
         ggplot2::xlab("") +
         ggplot2::ylab(stringr::str_to_title(metric)) +
         ggplot2::theme_bw() +
         ggplot2::theme(text=ggplot2::element_text(size=18),
                        legend.title=ggplot2::element_blank()) + 
         theme_simplex()
    
    # Labeling
    if (is.null(search)) {
        labeling <- mapply(function(x, label) {
            nodes <- x$nodes
            if (reverse) {
                nodes.sorted <- nodes[order(nodes[,metric]),]
            } else {
                nodes.sorted <- nodes[order(-nodes[,metric]),]
            }
            nodes.top <- nodes.sorted[seq(top),]
            data.frame(id=nodes.top$id, variable=label, value=nodes.top[,metric])
        }, networks, names(networks), SIMPLIFY=FALSE)
        intersecting <- Reduce(intersect, lapply(labeling, function(x) x$id))
    } else {
        labeling <- mapply(function(x, label) {
            nodes <- x$nodes
            nodes.top <- nodes[nodes$id %in% search,]
            data.frame(id=nodes.top$id, variable=label, value=nodes.top[,metric])
        }, networks, names(networks), SIMPLIFY=FALSE)
        intersecting <- c()
    }
    
    df <- labeling %>%
          plyr::rbind.fill() %>%
          dplyr::mutate(group=dplyr::if_else(id %in% intersecting, "Shared", variable)) %>%
          dplyr::mutate(group=as.factor(group))
    
    if (shared) {
        df %<>% dplyr::filter(group == "Shared")
    }

    if (symbols) {
        df$label <- networks[[1]]$get.symbols(df$id)
    } else {
        df$label <- df$id
    }

    if (log & reverse) {
        p <- p + ggplot2::scale_y_continuous(trans=pseudo.log.trans.rev())
    } else if (reverse) {
        p <- p + ggplot2::scale_y_reverse()
    } else if (log) {
        p <- p + ggplot2::scale_y_continuous(trans=pseudo.log.trans())
    }

    p + 
    ggrepel::geom_label_repel(data=df, 
                              aes(x=variable, y=value, label=label, color=group),
                              label.size=0.1,
                              size=size,
                              label.padding=0.15,
                              box.padding=0.1,
                              min.segment.length=0,
                              segment.alpha=0.25,
                              direction="both")
}

#' Hierarchical Clustering of Networks by Centrality
#' 
#' @param networks A named list of interactive omics network objects
#' @param metric Centrality measure
#' @param method Correlation method
#' 
#' @return A ggplot object
#' 
#' @importFrom dplyr select bind_cols
#' @importFrom tidyr drop_na
#' @importFrom factoextra fviz_dend
#' @importFrom magrittr %>% set_colnames
#' @importFrom ggplot2 labs
#' 
#' @export
networks.hclust <- function(networks, 
                            metric, 
                            method=c("pearson", "kendall")) {
    
    # Check for network object(s)
    is.networks(networks)

    # Default args
    method <- match.arg(method)

    df <- mapply(function(x, y) {
        dplyr::select(x$nodes, (!!metric)) %>%
        magrittr::set_colnames(y)
    }, networks, names(networks), SIMPLIFY=FALSE) %>%
    dplyr::bind_cols() %>%
    tidyr::drop_na()

    dd <- factoextra::get_dist(t(df), method=method)
    hc <- hclust(dd, method="ward.D2")
    factoextra::fviz_dend(hc, horiz=TRUE) +
    ggplot2::labs(title="") + 
    theme_simplex()
}

#' Differential Centrality Rankings Across Networks
#' 
#' @param networks A named list of interactive omics network objects
#' @param metric Centrality measure
#' @param p Scaling exponent
#' @param top Number of nodes to include
#' @param show Show rank or metric value
#' 
#' @return A ggplot object
#' 
#' @importFrom dplyr select mutate arrange pull desc
#' @importFrom magrittr %>% set_colnames
#' @importFrom ggplot2 ggplot aes geom_tile geom_text theme scale_fill_gradient labs theme_classic theme element_blank element_text element_rect geom_bar coord_flip
#' @importFrom ggpubr ggarrange
#' @importFrom reshape2 melt
#' @importFrom glue glue
#' 
#' @export
networks.diffc <- function(networks, metric, p=1, top=25, show=c("r", "v")) {
    show <- match.arg(show)
    data <-  mapply(function(x) {
        df <- dplyr::select(x$nodes, id, {{metric}}) %>%
            magrittr::set_colnames(c("n", "v")) %>%
            dplyr::mutate(r = rank(-v)) %>%
            dplyr::mutate(rsw = (nrow(.)-r+1)^p) %>%       
            dplyr::mutate(rswn = rsw/sum(rsw)) %>%
            dplyr::select(n, v, r, rswn) 
        
        rownames(df) <- df$n
        return(df)
    }, networks, SIMPLIFY=FALSE)
    
    # Compute difference between normalized ranks
    dfs.rswn <- lapply(data, function(x) dplyr::select(x, rswn))
    df.rswn <- do.call(cbind, dfs.rswn)
    colnames(df.rswn) <- names(dfs.rswn)
    
    df.s <- data.frame(avg=apply(df.rswn, 1, mean),
                       max=apply(df.rswn, 1, max),
                       min=apply(df.rswn, 1, min))
    
    df.s$mxabv <- df.s$max-df.s$avg
    df.s$mxblw <- df.s$avg-df.s$min
    df.s <- dplyr::select(df.s, mxabv, mxblw)
    df.delta <- data.frame(delta=apply(df.s, 1, max), node=rownames(df.s), row.names=rownames(df.s))
    
    # Grab nodes with largest differences
    nodes.ranked <- dplyr::arrange(df.delta, (desc(delta))) %>% dplyr::pull(node)
    
    # What values to what want to show
    dfs <- lapply(data, function(x) dplyr::select(x, {{show}}))
    df <- do.call(cbind, dfs)
    colnames(df) <- names(dfs)
    
    # Data for plotting
    df <- head(df[nodes.ranked,,drop=F], top)
    df.rswn <- head(df.rswn[nodes.ranked,,drop=F], top)
    df.delta <- head(df.delta[nodes.ranked,,drop=F], top)
    
    # Use normalized ranks for coloring
    df.rswn.s <- t(apply(df.rswn, 1, scale))
    colnames(df.rswn.s) <- colnames(df.rswn)
    
    # Paste in colors
    df$node <- rownames(df)
    df.m <- reshape2::melt(df)
    df.rswn.s.m <- reshape2::melt(df.rswn.s)
    colnames(df.rswn.s.m) <- c("v1","v2","rwsn_scaled")
    df.m.cbind <- cbind(df.m, df.rswn.s.m$rwsn_scaled)
    colnames(df.m.cbind) <- c("node", "variable", "value", "rwsn_scaled")
    df.m.cbind$node <- factor(df.m.cbind$node, levels=rev(nodes.ranked))
    
    p1 <- ggplot2::ggplot(df.m.cbind, mapping=aes(x=variable, y=node, fill=rwsn_scaled)) +
        ggplot2::geom_tile(color="#030303") +
        ggplot2::geom_text(aes(label=round(value, 0))) +
        ggplot2::theme(axis.text.x=ggplot2::element_blank()) +
        ggplot2::scale_fill_gradient(low="#555AE6", high="#FFFFFF") +
        ggplot2::labs(title="Differential Centrality", subtitle=glue("Ranked by {metric}"), x="") +
        ggplot2::theme_classic() +
        ggplot2::theme(axis.title.y=ggplot2::element_blank(),
                      axis.line.x=ggplot2::element_blank(),
                      axis.ticks.x=ggplot2::element_blank(),
                      axis.text.x=ggplot2::element_text(angle=45, hjust=1),
                      legend.position="none",
                      strip.background=ggplot2::element_rect(fill="#EEEEEE", color="#FFFFFF"))
    
    # Side bar for ranking
    df.delta.m <- reshape2::melt(df.delta)
    df.delta.m$node <- factor(df.delta.m$node, levels=rev(nodes.ranked))
    
    p2 <- df.delta.m %>%
        ggplot2::ggplot(aes(x=node, y=value)) +
        ggplot2::geom_bar(stat="identity", width=0.5, fill="#555AE6", color="#030303") +
        ggplot2::coord_flip() +
        ggplot2::theme_classic() +
        ggplot2::labs(title="", subtitle="DCS", y="") +
        ggplot2::theme(axis.text.x=ggplot2::element_blank(),
                       axis.line.x=ggplot2::element_blank(),
                       axis.ticks.x=ggplot2::element_blank(),
                       axis.title.y=ggplot2::element_blank(),
                       axis.text.y=ggplot2::element_blank(),
                       strip.background=ggplot2::element_rect(fill="#EEEEEE", color="#FFFFFF"))    
    
    ggpubr::ggarrange(p1, p2, align="h", widths=c(0.8, 0.2))
}
