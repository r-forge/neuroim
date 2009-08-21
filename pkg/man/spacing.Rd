\name{spacing}
\alias{spacing}
\title{ returns cell dimensions for a voxel or pixel unit }
\description{
  generic function to access the voxel or pixel dimension for a brain image
}
\usage{
spacing(x)
}

\arguments{
  \item{x}{ object derived from "BrainSpace" or "BrainData" classes }
}
\value{
  a numeric vector of the cell sizes for each image axis
}

\note{
  this function does not deal with non-spatial dimensions, such as
  time.  Thus, a 4-D fMRI image return a vector of length 3, one for
  each spatail dimension.

}

\examples{
}

\keyword{ manip }

