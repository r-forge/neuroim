\name{BrainList-class}
\docType{class}
\alias{BrainList-class}

\title{Class "BrainList" }
\description{ a class for storing a named list of "BrainData" objects }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BrainList", ...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{.Data}:}{Object of class \code{"list"} : a list of
      "BrainData" objects, all of which having equivalent geometries }
    \item{\code{names}:}{ the names of the data objects }
    \item{\code{space}:}{Object of class \code{"BrainSpace"} : the
      canonical "BrainSpace" of the image list  }
  }
}
\section{Extends}{
Class \code{"\linkS4class{list}"}, from data part.
Class \code{"\linkS4class{BrainData}"}, directly.
Class \code{"\linkS4class{vector}"}, by class "list", distance 2.
}
\section{Methods}{
No methods defined with class "BrainList" in the signature.
}
\references{ ~put references to the literature/web site here ~ }
\author{  }
\examples{

}
\keyword{classes}
