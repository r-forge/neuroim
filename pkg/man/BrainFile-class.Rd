\name{BrainFile-class}
\docType{class}
\alias{BrainFile-class}
\alias{show,BrainFile-method}
\alias{openFor,BrainFile-method}
\alias{path,BrainFile-method}
\alias{fileName,BrainFile-method}
\alias{fileType,BrainFile-method}

\title{ "BrainFile" virtual class }


\description{ A class that encapsulates function associated with brain image input/output }

\section{Objects from the Class}{A virtual Class: No objects may be created from it.}

\section{Slots}{
	 \describe{
    \item{\code{path}:}{Object of class \code{"character"} : the path
      to the image file   }
    \item{\code{open}:}{Object of class \code{"character"} : whether the
      file to be opened for read "r" or write  "w" }
    
  }
}
\section{Methods}{
  \describe{  
    \item{openFor}{\code{signature(x = "BrainFile")}: querues whether
      file is open for read or write }
    \item{path}{\code{signature(x = "BrainFile")}: the full path of the
      file  }
    \item{show}{\code{signature(x = "BrainFile")}: print a text
      representation of the \code{BrainFile} object }
    \item{fileName}{\code{signature(x = "BrainFile")}: return the file
      name of the current connection }
    \item{fileType}{\code{signature(x = "BrainFile")}: return the file
      type of the backing file (e.g. NIFTI) }
    
  }
}
  

\examples{

}
\keyword{classes}
