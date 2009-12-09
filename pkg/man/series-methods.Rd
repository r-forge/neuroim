\name{series-methods}
\docType{methods}
\alias{series-methods}
\alias{series,SparseBrainVector,matrix-method}
\alias{series,SparseBrainVector,numeric-method}
\alias{series,TiledBrainVector,matrix-method}
\alias{series,TiledBrainVector,numeric-method}
\title{ selects a series from a set of indices or grid locations }
\description{
	This function takes  a set of indices or grid locations retrieves a set of vector series from a 4-dimensional image
}
\section{Methods}{
\describe{

\item{x = "SparseBrainVector", i = "matrix"}{ extract time series with matrix of coordinates }

\item{x = "SparseBrainVector", i = "numeric"}{ extract time series with indices}

\item{x = "TiledBrainVector", i = "matrix"}{ extract time series with matrix of coordinates }

\item{x = "TiledBrainVector", i = "numeric"}{ extract time series with indices }
}}

\keyword{methods}

