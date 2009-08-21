\name{loadVector}
\alias{loadVector}
\title{ convenience function to create a "BrainVector" instance from a specified
  file  }
\description{
 convenience function to create a "BrainVector" instance from a
 specified file
}
\usage{
loadVector(filename, volRange = NULL, mask=NULL)
}

\arguments{
  \item{filename}{ name of the file to load }
  \item{volRange}{ the volume indices to load }
  \item{mask}{ a mask, which can be an \code{array} or \code{BrainData} instance with appropriate dimensions, indicating which voxels to load. If provided function returns a SparseBrainVector }
}

\value{
  an of object of class \code{BrainVector}
}

\author{ }
\examples{
}
\keyword{manip}

