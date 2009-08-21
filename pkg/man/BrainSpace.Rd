\name{BrainSpace}
\alias{BrainSpace}
\title{ creates a new "BrainSpace" object }
\description{
  function to create a new "BrainSpace" object from scratch
}
\usage{
BrainSpace(Dim, origin = NULL, spacing = NULL, orientation = NULL, trans
= NULL, reptime=1)
}

\arguments{
  \item{Dim}{ an integer vector supplying the dimensions of the image
    grid  }
  \item{origin}{ a numeric vector with \code{length} equal to \code{Dim}
  represting image origin in mm }
  \item{spacing}{ the cell (pixel or voxel) size for each dimension in mm }
  \item{orientation}{ a character vector representing anatomical
    orientation of the grid axes  }
  \item{trans}{ a transformation matrix used to convert grid positions
    to real world coordinates }
  \item{reptime}{ the delay in seconds between successive volume acquisitions }
}

\value{
  an object of class "BrainSpace"
}

\examples{
}

\keyword{ }
