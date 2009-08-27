\name{IndexLookupVolume-class}
\Rdversion{1.1}
\docType{class}
\alias{IndexLookupVolume-class}
\alias{initialize,IndexLookupVolume-method}
\alias{space,IndexLookupVolume-method}

\title{ A class used to map from 1d indices to lookup table indices}

\description{ 
	An IndexLookupVolume maps from 1-d volume indices to an ordered set of indices that map to indices ina lookup table.  
	This class is currently used in the implementation of \code{SparseBrainVector}  }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("IndexLookupVolume", space, indices)}.
}
\section{Slots}{
	 \describe{
    \item{\code{space}:}{Object of class \code{"BrainSpace"} }
    \item{\code{indices}:}{Object of class \code{"integer"} }
    \item{\code{map}:}{Object of class \code{"integer"} }
  }
}
\section{Methods}{
  \describe{
    \item{coords}{\code{signature(x = "IndexLookupVolume")}: ... }
    \item{indices}{\code{signature(x = "IndexLookupVolume")}: ... }
    \item{initialize}{\code{signature(.Object = "IndexLookupVolume")}: ... }
    \item{lookup}{\code{signature(x = "IndexLookupVolume", i = "numeric")}: ... }
    \item{space}{\code{signature(x = "IndexLookupVolume")}: ... }
	 }
}

\examples{
showClass("IndexLookupVolume")
}
\keyword{classes}
