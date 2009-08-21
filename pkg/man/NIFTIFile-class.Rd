\name{NIFTIFile-class}
\docType{class}
\alias{NIFTIFile-class}
\alias{dataFileExt,NIFTIFile-method}
\alias{fileName,NIFTIFile-method}
\alias{fileType,NIFTIFile-method}
\alias{headerFileExt,NIFTIFile-method}
\alias{openFor,NIFTIFile-method}
\alias{path,NIFTIFile-method}
\alias{readData,NIFTIFile,index-method}
\alias{readData,NIFTIFile,missing-method}
\alias{readHeader,NIFTIFile-method}
\alias{writeHeader,NIFTIFile,BrainHeader-method}
\alias{writeData,NIFTIFile,NIFTIHeader,vector-method}
\alias{show,NIFTIFile-method}

\title{Class "NIFTIFile"  }
\description{ a class that handles input/output of NIFTI formatted image
data }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("NIFTIFile", ...)}.

}
\section{Slots}{
	 \describe{
    \item{\code{fileType}:}{Object of class \code{"character"} : the file
      type  }
    \item{\code{path}:}{Object of class \code{"character"} : the file
      path  }
    \item{\code{open}:}{Object of class \code{"character"} : whether to
      open file for reading "r" or writing "w" }
  }
}
\section{Extends}{
Class \code{"\linkS4class{BrainFile}"}, directly.
}
\section{Methods}{
  \describe{
    \item{dataFileExt}{\code{signature(x = "NIFTIFile")}: returns the
      data file extension  }
    
    \item{fileName}{\code{signature(x = "NIFTIFile")}: returns the file
      underlying file name  }
    
    \item{fileType}{\code{signature(x = "NIFTIFile")}: returns the file
      type  }
    
    \item{headerFileExt}{\code{signature(x = "NIFTIFile")}: returns the
      header extension }
    
    \item{openFor}{\code{signature(x = "NIFTIFile")}: querues whether
      file is open for read or write }
    
    \item{path}{\code{signature(x = "NIFTIFile")}: the full path of the
      file  }
    
    \item{readData}{\code{signature(x = "NIFTIFile", indices =
	"index")}: reads the data and returns a class deriving from
      "BrainData" }
    
    \item{readData}{\code{signature(x = "NIFTIFile", indices =
	"missing")}:  reads the data and returns a class deriving from
      "BrainData"}
    
    \item{readHeader}{\code{signature(x = "NIFTIFile")}: reads the
      header and returns a "NIFTIHeader" object  }
    
    \item{writeHeader}{\code{signature(x = "NIFTIFile",header = "BrainHeader")}: writes supplied header argument to disk as NIFTIHeader  }

    \item{writeData}{\code{signature(x = "NIFTIFile",header = "NIFTIHeader", data="vector")}: writes data vector to disk  }

    \item{show}{\code{signature(x = "NIFTIFile")}: print a text
      representation of the \code{NIFTIFile} object }
  }
  
}

\examples{

}
\keyword{classes}
