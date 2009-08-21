\name{voxelToIndex}
\alias{voxelToIndex}
\title{ converts between grid and 1-d index coordinates  }
\description{
  generic function to convert between multidimensional grid coordinates and 1-d indices
}
\usage{
voxelToIndex(x, i, j, ...)
}


\arguments{
  \item{x}{ an object derived from the "BrainData" class  }
  \item{i}{ the grid coordinate of the first dimension }
  \item{j}{ the grid coordinate for the second dimension }
  \item{\dots}{ coordinates for additional dimensions (e.g. 3 or 4) }
}

\value{
  a integer representing the 1-d index
}
\examples{
}
\keyword{manip}

