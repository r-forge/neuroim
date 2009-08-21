\name{BrainSpace-class}
\docType{class}
\alias{BrainSpace-class}
\alias{bounds,BrainSpace-method}
\alias{invTrans,BrainSpace-method}
\alias{numdim,BrainSpace-method}
\alias{orientation,BrainSpace-method}
\alias{origin,BrainSpace-method}
\alias{spacing,BrainSpace-method}
\alias{trans<-,BrainSpace,matrix-method}
\alias{trans,BrainSpace-method}
\alias{dim,BrainSpace-method}
\alias{show,BrainSpace-method}
\alias{reptime,BrainSpace-method}
\title{Class "BrainSpace"}
\description{BrainSpace represents the topology of a dataset }



  
\section{Objects from the Class}{
Objects can be created by calls of the form \code{BrainSpace(Dim,
  dimension, spacing, origin) }.
	 
}
\section{Slots}{
  \describe{
    \item{\code{Dim}:}{Object of class \code{"integer"} --
      an integer array specifiying the spatial dimensions }

    \item{\code{origin}:}{Object of class \code{"numeric"} --
      an array representing the absolute location of the origin
      (coordinate at voxel [0, 0, 0]) }
    
    \item{\code{spacing}:}{Object of class \code{"numeric"} --
      an array representing the grid spacing for each dimension }      
   
    \item{\code{orientation}:}{Object of class \code{"character"} --
      character array describing axes orientation, e.g., "L" for
      "left-right", "P" for "posterior-anterior", "I" for
      "inferior-superior". This slot is currently not used and may
      contain invalid information.}
    
    \item{\code{trans}:}{Object of class \code{"matrix"} --
      This is a transformation matrix used for computing the mapping
      from index coordinates to real world oordiantes. Should be a 4X4
      matrix for volumetrix data.}
   
    \item{\code{invTrans}:}{Object of class \code{"matrix"} --
      This is the inverse transformation (real world to index) and is
      computed internally and therefore should not be changed externally }
    

    \item{\code{reptime}:}{Object of class \code{"numeric"} --
      The delay between volume acquisitions in seconds }
      
  }
}

\section{Methods}{
  \describe{
    \item{bounds}{\code{signature(x = "BrainSpace")}: -- returns the real
      world bounds as a matrix of (min, max) pairs }

    \item{invTrans}{\code{signature(x = "BrainSpace")}: -- returns the
      dimension of the data axes for the space }

    \item{numdim}{\code{signature(x = "BrainSpace")}: -- returns the
      number of dimensions in space }

    \item{orientation}{\code{signature(x = "BrainSpace")}: -- returns
      the (currently unsupported) orientation code  }

    \item{origin}{\code{signature(x = "BrainSpace")}: -- returns the
      coordinate origin }

    \item{spacing}{\code{signature(x = "BrainSpace")}: -- returns the voxel
      spacing }
    
    \item{trans<-}{\code{signature(x = "BrainSpace", value = "matrix")}:
      -- returns the index-to-word transformation matrix }

    \item{trans}{\code{signature(x = "BrainSpace")}: -- returns the
      inverse transformation matrix }
    
    \item{reptime}{\code{signature(x = "BrainSpace")}: -- returns the
      repetition time }
    
    \item{show}{\code{signature(x = "BrainSpace")}: -- print information
    about BrainSpace object }
    
  }

  
  }
  
\references{ no references }
\author{ Bradley Buchsbaum \email{bbuchsbaum@berkeley.edu}  }
\note{  }

\seealso{}
\examples{

space <- BrainSpace(Dim=c(64,64,18))
show(space)
trans(space)
origin(space)


}

\keyword{classes}
