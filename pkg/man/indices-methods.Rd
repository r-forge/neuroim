\name{indices-methods}
\docType{methods}
\alias{indices-methods}
\alias{indices,IndexLookupVolume-method}
\alias{indices,SparseBrainVector-method}
\alias{indices,TiledBrainVector-method}
\title{ generic method to extract 1-dimensional indices from data structure }

\description{
 function that retrieves the 1-dimensional indices of a sparse data structure
}
\section{Methods}{
\describe{

\item{x = "IndexLookupVolume"}{ returns the 1-d indices for lookup table }

\item{x = "SparseBrainVector"}{ returns the 1-d indices for the underlying lookup table (see above) }

\item{x = "TiledBrainVector"}{ not currently implemented }
}}
\keyword{methods}

