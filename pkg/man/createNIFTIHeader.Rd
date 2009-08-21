\name{createNIFTIHeader}
\alias{createNIFTIHeader}

\title{ a function to create a NIFTIHeader object  }
\description{
  this function may be used to create a \code{NIFTIHeader} object given
  an existing brain data object
}
\usage{
createNIFTIHeader(data, fileName, dataType = "FLOAT")
}

\arguments{
  \item{data}{ an object deriving from the class \code{BrainData} }
  \item{fileName}{ the file name for the header (need not exist) }
  \item{dataType}{ the storage type of the data, e.g. "SHORT" }
}
\details{
  This argument is useful if one wants to create a NIFTIHeader object
  on the basis of an existing \code{BrainData} object
}
\value{
  an object of class \code{NIFTIHeader}
}

\author{ }
\examples{
}
\keyword{manip}

