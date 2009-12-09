\name{ANALYZEFile-class}
\docType{class}
\alias{ANALYZEFile-class}
\alias{dataFileExt,ANALYZEFile-method}

\title{Class "ANALYZEFile" }

\description{a class that handles input/output of ANALYZE formatted image
data }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ANALYZEFile", ...)}.
	 
}
\section{Slots}{
  \describe{
    \item{\code{fileType}:}{Object of class \code{"character"} : the
      file type }
    \item{\code{path}:}{Object of class \code{"character"} : the file path }
    \item{\code{open}:}{Object of class \code{"character"} : whether to
      open file for reading "r" or writing "w" }
  }
}
\section{Extends}{
Class \code{"\linkS4class{BrainFile}"}, directly.
}
\section{Methods}{
 \describe{
    \item{dataFileExt}{\code{signature(x = "ANALYZEFile")}: returns the
      data file extension  }
    }
}

\keyword{classes}
