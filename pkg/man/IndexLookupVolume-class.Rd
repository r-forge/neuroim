\name{IndexLookupVolume-class}
\alias{IndexLookupVolume-class}
\alias{IndexLookupVolume}
\title{IndexLookupVolume...}
\description{IndexLookupVolume}
\details{\code{IndexLookupVolume-class}: Three-dimensional brain image that can be used as a map between 1D grid indices and a table of values
Currently used in the \code{\linkS4class{SparseBrainVector}} class.

}
\section{Extends}{\code{\linkS4class{BrainVolume}}}
\section{Methods}{\describe{\item{\code{\link[=initialize,IndexLookupVolume-method]{initialize}}}{}\item{\code{\link[=indices,IndexLookupVolume-method]{indices}}}{}\item{\code{\link[=lookup,IndexLookupVolume,numeric-method]{lookup}}}{}\item{\code{\link[=space,IndexLookupVolume-method]{space}}}{}}}
\alias{IndexLookupVolume}
\usage{IndexLookupVolume(space, indices)
}
\arguments{\item{space}{a BrainSpace object}
\item{indices}{the set of 1-d indices defining the lookup map}
}

