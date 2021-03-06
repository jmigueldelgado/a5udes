% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/river_topology.r
\name{river_upstream}
\alias{river_upstream}
\title{calculate contributing river network to a given river reach}
\usage{
river_upstream(reach_id, riv_i, graph)
}
\arguments{
\item{reach_id}{from \code{data(river_geometry)}}

\item{riv_i}{a subset of river reaches of class sf from \code{data(river_geometry)}}

\item{graph}{a graph of class igraph produced with \code{riv2graph()}}
}
\value{
riv_upstr the contributing river network
}
\description{
calculate contributing river network to a given river reach
}
