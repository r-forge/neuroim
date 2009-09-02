\name{BrainRegion3D-class}
\Rdversion{1.1}
\docType{class}
\alias{BrainRegion3D-class}
\alias{[,BrainRegion3D,numeric,missing-method}
\alias{length,BrainRegion3D-method}

\title{Class "BrainRegion3D"}
\description{ class for to represent a set of 3d coordinates plus associated values that form a region interest }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BrainRegion3D", ...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{data}:}{Object of class \code{"numeric"}; the values associated with the coordinates }
    \item{\code{coords}:}{Object of class \code{"matrix"}; the x,y,z coordinates in voxel space }
    \item{\code{space}:}{Object of class \code{"BrainSpace"}; the associated geometry of image }
  }
}
\section{Extends}{
Class \code{"\linkS4class{BrainData}"}, directly.
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "BrainRegion3D", i = "numeric", j = "missing")}: ... }
    \item{coords}{\code{signature(x = "BrainRegion3D")}: extracts the x,y,z coordinates }
    \item{length}{\code{signature(x = "BrainRegion3D")}: returns the number of coordinates/values in the region }
 }
}

\author{ Bradley R. Buchsbaum, \email{bbuchsbaum@rotman-baycrest.on.ca}}

\examples{
showClass("BrainRegion3D")
}
\keyword{classes}
