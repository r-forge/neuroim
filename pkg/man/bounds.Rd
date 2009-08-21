\name{bounds}

\alias{bounds}


\title{ get the real world bounds of an image's data space }

\description{
  generic method for accessing the real world coordinates bounds of a
  \code{\link{BrainSpace}} object.
}

\arguments{
  \item{x}{an object from which to retrieve the bounds}
}


\usage{
bounds(x)
}

\examples{

space <- BrainSpace(c(256,256))
b <- bounds(space)

}

\keyword{}
