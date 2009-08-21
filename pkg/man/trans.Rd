\name{trans}
\alias{trans}
\alias{trans<-}
\title{ function to access the forward transformation associated with a brain
  image }
\description{
  this function access the transformation matrix attached to a
  "BrainSpace" object
}
\usage{
trans(x)
}

\arguments{
  \item{x}{ an object derived from "BrainData" or "BrainSpace" classes }
}
\details{
  transformation matrices are 4X4 affine matrices for 3D images and 3X3
  matrices for 2D images
  }

\value{
the transformation matrix
}
\examples{
}
\keyword{ manip }

