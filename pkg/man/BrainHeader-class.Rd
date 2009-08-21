\name{BrainHeader-class}
\docType{class}
\alias{BrainHeader-class}
\alias{dataFile,BrainHeader-method}
\alias{dataType,BrainHeader-method}
\alias{extraInfo,BrainHeader-method}
\alias{versionNum,BrainHeader-method}
\alias{encoding,BrainHeader-method}
\alias{fileType,BrainHeader-method}
\title{Class "BrainHeader"}

\description{ A virtual class representing basic information contained in
  brain image headers }
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots}{
	 \describe{
    \item{\code{headerFile}:}{Object of class \code{"character"} : the
      name of the header file  }
    \item{\code{dataFile}:}{Object of class \code{"character"} : the
      name of the data file  }
    \item{\code{fileType}:}{Object of class \code{"character"} : the
      file format of this header }
    \item{\code{versionNum}:}{Object of class \code{"character"} : the
      version number of the file format  }
    \item{\code{encoding}:}{Object of class \code{"character"} : the
      encoding type, which may be "binary" or "gz" }
    \item{\code{endian}:}{Object of class \code{"character"} : the
      endianess of the stored data  }
    \item{\code{dataOffset}:}{Object of class \code{"numeric"} : the
      offset in the data file where the first image value may be found  }
    \item{\code{dataType}:}{Object of class \code{"character"} : the
      data type of the stored image data }
    \item{\code{extraInfo}:}{Object of class \code{"list"} : auxilliary
      or format-specific information }
  }
}
\section{Methods}{
  \describe{
    \item{dataFile}{\code{signature(x = "BrainHeader")}: simple accessor
      method }
    \item{dataOffset}{\code{signature(x = "BrainHeader")}: simple
      accessor method }
    \item{dataType}{\code{signature(x = "BrainHeader")}: simple accessor
    method }
    \item{encoding}{\code{signature(x = "BrainHeader")}: simple accessor
    method}
    \item{endian}{\code{signature(x = "BrainHeader")}: simple accessor
      method  }
    \item{extraInfo}{\code{signature(x = "BrainHeader")}: simple accessor
    method}
    \item{fileType}{\code{signature(x = "BrainHeader")}: simple accessor method }
    \item{headerFile}{\code{signature(x = "BrainHeader")}: simple
      accessor method }
    \item{versionNum}{\code{signature(x = "BrainHeader")}: simple
      accessor method }
    
	 }
}

\examples{

}
\keyword{classes}
