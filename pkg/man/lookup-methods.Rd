\name{lookup-methods}
\docType{methods}
\alias{lookup-methods}
\alias{lookup,IndexLookupVolume,numeric-method}
\alias{lookup,SparseBrainVector,numeric-method}
\title{lookup indices}
\description{
 a function that is used internally to convert between 1d indices and lookup kys for sparse data structures 
}
\section{Methods}{
\describe{

\item{x = "IndexLookupVolume", i = "numeric"}{ takes 1d index vector and returns a sequence of lookup indices }

\item{x = "SparseBrainVector", i = "numeric"}{ takes 1d index vector and returns a sequence of lookup indices  }
}}
\keyword{methods}
