#' An ggplot theme for simplex applications
#'
#' @param dark Background color
#' 
#' @return A ggplot object
#' 
#' @importFrom ggplot2 theme element_rect
#' 
#' @export
theme_simplex <- function(dark=FALSE) {
    hex <- ifelse(dark, "#F2F2F2", "#FCFCFC")
    ggplot2::theme(plot.background=ggplot2::element_rect(fill=hex),
                   panel.background=ggplot2::element_rect(fill=hex),
                   legend.background=ggplot2::element_rect(fill=hex))
}

#' An empty ggplot
#'
#' @return A ggplot object
#' 
#' @importFrom ggplot2 ggplot theme_void
#' 
#' @export
ggempty <- function() {
    ggplot2::ggplot() + 
    ggplot2::theme_void()  
}

#' Enrichment plot implemented in ggplot
#'
#' @param n The length of a ranked list
#' @param positions A vector of positions in the ranked list
#' @param x_axis The x-axis of a running enrichment score
#' @param y_axis The y-axis of a running enrichment score
#' @param title Plot title
#' 
#' @return A ggplot object
#' 
#' @importFrom ggplot2 qplot geom_rug geom_hline geom_vline annotate theme_classic theme element_text element_rect
#' 
#' @export
ggeplot <- function(n, positions, x_axis, y_axis, title="") {
    score <- which.max(abs(y_axis))
    ggplot2::qplot(x_axis, 
                   y_axis,
                   main=title,
                   ylab="Running Enrichment Score", 
                   xlab="Position in Ranked List of Genes",
                   geom="line")+
    ggplot2::geom_rug(data=data.frame(positions), aes(x=positions), inherit.aes=FALSE)+
    ggplot2::geom_hline(yintercept=0) +
    ggplot2::geom_vline(xintercept=n/2, linetype="dotted") +
    ggplot2::annotate("point", x=x_axis[score], y=y_axis[score], color="red") +
    ggplot2::annotate("text", x=x_axis[score]+n/20, y=y_axis[score], label=round(y_axis[score],2)) +
    ggplot2::annotate("point", x=x_axis[score], y=y_axis[score], color="red") +
    ggplot2::theme_classic() + 
    ggplot2::theme(plot.title=ggplot2::element_text(hjust=1, face="italic"),
                   axis.line=ggplot2::element_line(color="black"),
                   panel.border=ggplot2::element_rect(color="black", fill=NA, size=1)) +
    theme_simplex()
}

#' Weighted rank scores plot implement in ggplot
#'
#' @param n The number of ranks
#' @param p Weighting exponent
#' 
#' @return A ggplot object
#' 
#' @importFrom dplyr mutate 
#' @importFrom reshape2 melt 
#' @importFrom ggpubr ggscatter 
#' @importFrom ggplot2 scale_colour_manual labs
#' 
#' @export
ggwrs <- function(n, p=1) {
    data.frame(r=seq_len(n)) %>%
    dplyr::mutate(rsu = (nrow(.)-r+1)^1) %>%
    dplyr::mutate(rsw = (nrow(.)-r+1)^p) %>%    
    dplyr::mutate(Unweighted = rsu/sum(rsu)) %>%
    dplyr::mutate(Weighted = rsw/sum(rsw))  %>%
    reshape2::melt(measure.vars=c("Unweighted", "Weighted")) %>%
    ggpubr::ggscatter(x="r", y="value", size=1, color="variable", alpha=0.5) +
    ggplot2::scale_colour_manual(values = c("#A8A7A7", "#2F9599")) +
    ggplot2::labs(x="Rank", y="Normalized Rank Score", color="Rank Score") +
    theme_simplex()
}
