\name{AFNIFile-class}
\docType{class}
\alias{AFNIFile-class}

\title{Class "AFNIFile"}
\description{ a class that handles input/output of NIFTI formatted image
data }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("AFNIFile", ...)}
}
\section{Slots}{
	 \describe{
    \item{\code{path}:}{Object of class \code{"character"} the file path }
    \item{\code{open}:}{Object of class \code{"character"} whether to
      open file for reading "r" or writing "w"  }
    \item{\code{fileType}:}{Object of class \code{"character"} : the
      specific file type for this format }
  }
}

\section{Extends}{
Class \code{"\linkS4class{BrainFile}"}, directly.
}
\section{Methods}{
No methods defined with class "AFNIFile" in the signature.
}

\examples{

}
\keyword{classes}
