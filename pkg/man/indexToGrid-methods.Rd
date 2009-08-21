\name{indexToGrid-methods}
\docType{methods}
\alias{indexToGrid-methods}
\alias{indexToGrid,BrainSlice,index-method}
\alias{indexToGrid,BrainSpace,index-method}
\alias{indexToGrid,BrainVolume,index-method}
\title{ converts a 1-dimensional index vector to a matrix of coordinates }
\description{
function to convert from 1-dimensional index coordinates to an n-dimensional coordinate representation.
}
\section{Methods}{
\describe{

\item{x = "BrainSlice", idx = "index"}{  converts a 1-dimensional index vector to a two-column matrix of coordinates }

\item{x = "BrainSpace", idx = "index"}{ converts a 1-dimensional index vector to a n-column matrix of coordinates  }

\item{x = "BrainVolume", idx = "index"}{ converts a 1-dimensional index vector to a three-column matrix of coordinates }
}}

\keyword{methods}

