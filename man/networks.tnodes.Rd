% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/networks.R
\name{networks.tnodes}
\alias{networks.tnodes}
\title{Important Nodes}
\usage{
networks.tnodes(
  networks,
  metric,
  top = 15,
  size = 3,
  search = NULL,
  log = FALSE,
  reverse = FALSE,
  symbols = FALSE,
  shared = FALSE
)
}
\arguments{
\item{networks}{A named list of interactive omics network objects}

\item{metric}{Centrality measure}

\item{top}{Number of nodes to label}

\item{size}{Size of labels}

\item{search}{Include specific nodes for labeling}

\item{log}{Log-transform y-axis}

\item{reverse}{Flip y-axis}

\item{symbols}{Show symbols instead of node identifiers}

\item{shared}{Highly shared nodes only}
}
\value{
A ggplot object
}
\description{
Important Nodes
}
