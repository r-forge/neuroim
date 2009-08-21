\name{BrainSlice-class}
\docType{class}
\alias{BrainSlice-class}
\alias{indexToVoxel,BrainSlice,index-method}
\alias{numdim,BrainSlice-method}
\alias{voxelToIndex,BrainSlice,index,index-method}

\title{Class "BrainSlice" for two-dimensional image data }

\description{ This class is used to store two-dimensional image data
  representing a single cut or slice through a brain volume}

\section{Objects from the Class}{

  Objects can be created by calls of the form \code{ BrainSlice(data, space)}
 
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"array"} inherited data slot
      from "array" class. In this class, however, array data is always
      two-dimensional }
    
    \item{\code{space}:}{Object of class \code{"BrainSpace"} inherited
      slot from virtual class "BrainData" }
  }
}

\section{Extends}{
Class \code{"\linkS4class{array}"}, from data part.
Class \code{"\linkS4class{BrainData}"}, directly.
Class \code{"\linkS4class{structure}"}, by class "array", distance 2.
Class \code{"\linkS4class{matrix}"}, by class "array", distance 2, with explicit test and coerce.
Class \code{"\linkS4class{vector}"}, by class "array", distance 3, with explicit coerce.
Class \code{"\linkS4class{vector}"}, by class "array", distance 4, with explicit test and coerce.
Class \code{"\linkS4class{vector}"}, by class "array", distance 5, with explicit test and coerce.
}

\section{Methods}{
  \describe{
    \item{indexToVoxel}{\code{signature(x = "BrainSlice", idx =
	"index")}: -- maps from a one-dimensional index to a
      two-dimensional pixel/voxel coordinate }

    \item{voxelToIndex}{\code{signature(x = "BrainSlice", i = "index", j
	= "index")}: -- convert from a two-dimensional coordinate to
      one-dimensional index }
  }
}
       
\references{ }

\author{ Bradley Buchsbaum \email{bbuchsbaum@berkeley.edu} }

\note{ }

 
\seealso{
	
}
\examples{

#space <- BrainSpace(c(50,50))
#slice <- BrainSlice(rnorm(50*50), space)

}
\keyword{classes}

