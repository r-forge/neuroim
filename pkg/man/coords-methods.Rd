\name{coords-methods}
\docType{methods}
\alias{coords-methods}
\alias{coords,BrainRegion3D-method}
\alias{coords,IndexLookupVolume-method}
\alias{coords,SparseBrainVector-method}
\alias{coords,TiledBrainVector-method}
\title{ ~~ Methods for Function coords  ~~}
\description{
extracts the coordinates of a a coordnate-based image data structure as a matrix
}
\section{Methods}{
\describe{

\item{x = "BrainRegion3D"}{ returns the coordinates comprising the region of interest }

\item{x = "IndexLookupVolume"}{ returns the coordinate corresponding to the indices in the volume }

\item{x = "SparseBrainVector"}{ returns the coordinates corresponding to the indices in the volume }

\item{x = "TiledBrainVector"}{ not currently implemented }
}}
\keyword{methods}

