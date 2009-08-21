\name{BrainData-class}
\docType{class}
\alias{BrainData-class}
\alias{dim,BrainData-method}
\alias{numdim,BrainData-method}
\alias{space,BrainData-method}
\alias{spacing,BrainData-method}
\title{Virtual Class "BrainData" }
\description{
  The \code{BrainData} class is a class contained by all actual
  classes in the \pkg{Neuroimage} package.  It is a \dQuote{virtual} class.

}

\section{Objects from the Class}{
  A virtual Class: No objects may be created from it.
}

\section{Slots}{
  Common to \emph{all} BrainData objects in the package:
  \describe{
    
    \item{\code{space}:}{Object of class \code{"BrainSpace"} -- a
      description of the data "space", e.g. voxel resolution and iamge
      dimensions.
    }
  }
}


\section{Methods}{
  \describe{
    \item{dim}{\code{signature(x = "BrainData")}: extract data dimensions
      \code{\link{dim}}.}
    \item{space}{\code{signature(x = "BrainData")}: extract
      \dQuote{space} slot.}
    \item{numdim}{\code{signature(x = "BrainData")}: return number of
      dimensions in space }
    \item{spacing}{\code{signature(x = "BrainData")}: returns cell
      spacing for image data  }
  }
}
  
\references{ }

\author{Bradley Buchsbaum \email{bbuchsbaum@berkeley.edu} }

\note{ No notes yet }

 

\seealso{

}

\examples{

slotNames("BrainData")

cl <- getClass("BrainData")
names(cl@subclasses) 

showClass("BrainData") # output with slots and all subclasses

}

\keyword{classes}



