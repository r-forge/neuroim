\name{BrainVolume}
\alias{BrainVolume}

\title{ creates a new "BrainVolume" object }
\description{
  function to create a new object of class "BrainVolume"
}
\usage{
BrainVolume(data, space, indices = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ a numeric vector containing image data }
  \item{space}{ object of class \code{BrainSpace} describing the
    geometry of the slice }
  \item{indices}{ optional argument specifiying 1-d indices if data
    vector is sparse }
}
\value{
  an object of class \code{BrainVolume}

}
\examples{
}
\keyword{manip}