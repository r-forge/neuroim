\name{concat-methods}
\docType{methods}
\alias{concat-methods}
\alias{concat,BrainVector,BrainVector-method}
\alias{concat,BrainVector,BrainVolume-method}
\alias{concat,BrainVolume,BrainVolume-method}
\alias{concat,SparseBrainVector,SparseBrainVector-method}
\title{ concatenate two image objects }

\description{
 concatenate two image objects in the nth dimension (usualy time)
}

\section{Methods}{
\describe{

\item{x = "BrainVector", y = "BrainVector"}{ concatenates two BrainVectors in the fourth dimension }

\item{x = "BrainVector", y = "BrainVolume"}{ concatenates a BrainVector and a BrainVolume in the fourth dimension }

\item{x = "BrainVolume", y = "BrainVolume"}{ concatenates two BrainVolumes, to form a BrainVector }

\item{x = "SparseBrainVector", y = "SparseBrainVector"}{ concateaetes two SparseBrainVectors }
}}

\keyword{methods}

