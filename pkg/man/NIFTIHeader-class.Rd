\name{NIFTIHeader-class}
\docType{class}
\alias{NIFTIHeader-class}
\alias{show,NIFTIHeader-method}
\alias{spacing,NIFTIHeader-method}
\alias{versionNum,NIFTIHeader-method}
\alias{dataDim,NIFTIHeader-method}
\alias{createSpace,NIFTIHeader-method}

\title{Class "NIFTIHeader" }
\description{ class encapuslating a NIFTI formatted header }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("NIFTIHeader", ...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{headerFile}:}{Object of class \code{"character"} : see \link{BrainHeader-class} }
    \item{\code{dataFile}:}{Object of class \code{"character"} :  see \link{BrainHeader-class}}
    \item{\code{fileType}:}{Object of class \code{"character"} : see \link{BrainHeader-class}}
    \item{\code{versionNum}:}{Object of class \code{"character"} : see \link{BrainHeader-class}}
    \item{\code{encoding}:}{Object of class \code{"character"} : see \link{BrainHeader-class}}
    \item{\code{endian}:}{Object of class \code{"character"} : see \link{BrainHeader-class}}
    \item{\code{dataOffset}:}{Object of class \code{"numeric"} : see \link{BrainHeader-class}}
    \item{\code{dataType}:}{Object of class \code{"character"} : see \link{BrainHeader-class}}
    \item{\code{extraInfo}:}{Object of class \code{"list"} : see \link{BrainHeader-class}}
    
  }
}
\section{Extends}{
Class \code{"\linkS4class{BrainHeader}"}, directly.
}
\section{Methods}{
  \describe{
    \item{\code{headerFile}:}{Object of class \code{"character"} : the
      name of the header file  }
    \item{\code{dataDim}:}{Object of class \code{"character"} : the
      dimensions of the associated data file  }
    \item{\code{show}:}{Object of class \code{"character"} : prints a
      text representation of the header  }
    \item{\code{spacing}:}{Object of class \code{"character"} : returns
      the cell dimensions for the image grid  }
    \item{\code{versionNum}:}{Object of class \code{"character"} : returns
      the version number of the header format  }
    \item{\code{createSpace}:}{Object of class \code{"character"} :
      instantiates a "BrainSpace" from this \code{NIFTIHeader} object }
  }
  
}


\examples{

}
\keyword{classes}
