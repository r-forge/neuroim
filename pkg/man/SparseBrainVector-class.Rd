\name{SparseBrainVector-class}
\Rdversion{1.1}
\docType{class}
\alias{SparseBrainVector-class}
\alias{[,SparseBrainVector,numeric,numeric-method}
\alias{typeof,SparseBrainVector-method}


\title{Class "SparseBrainVector" ~~~ }
\description{	 ~~ A concise (1-5 lines) description of what the class is.  ~~}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SparseBrainVector", ...)}.
	 ~~ describe objects here ~~ 
}
\section{Slots}{
	 \describe{
    \item{\code{mask}:}{Object of class \code{"BrainVolume"} ~~ }
    \item{\code{data}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{map}:}{Object of class \code{"IndexLookupVolume"} ~~ }
    \item{\code{space}:}{Object of class \code{"BrainSpace"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{BrainData}"}, directly.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "SparseBrainVector", i = "numeric", j = "numeric")}: ... }
    \item{concat}{\code{signature(x = "SparseBrainVector", y = "SparseBrainVector")}: ... }
    \item{coords}{\code{signature(x = "SparseBrainVector")}: ... }
    \item{eachSeries}{\code{signature(x = "SparseBrainVector", FUN = "function")}: ... }
    \item{indices}{\code{signature(x = "SparseBrainVector")}: ... }
    \item{lookup}{\code{signature(x = "SparseBrainVector", i = "numeric")}: ... }
    \item{series}{\code{signature(x = "SparseBrainVector", i = "matrix")}: ... }
    \item{series}{\code{signature(x = "SparseBrainVector", i = "numeric")}: ... }
    \item{takeVolume}{\code{signature(x = "SparseBrainVector", i = "numeric")}: ... }
    \item{typeof}{\code{signature(x = "SparseBrainVector")}: ... }
    \item{writeVector}{\code{signature(x = "SparseBrainVector", fileName = "character", format = "missing")}: ... }
	 }
}
\author{ bbuchsbaum@rotman-baycrest.on.ca }
\keyword{classes}
