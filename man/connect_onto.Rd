% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/edge.R
\name{connect_onto}
\alias{connect_onto}
\title{Create one-way connection between pair of nodes}
\usage{
connect_onto(x, y, weight)
}
\arguments{
\item{x}{the sending Node}

\item{y}{the receiving Node}

\item{weight}{weight of the connection.}
}
\value{
nothing; the receiving node is updated.
}
\description{
Two nodes can be connected (by a non-zero weight) if they overlap in time
}

