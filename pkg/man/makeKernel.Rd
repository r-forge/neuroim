\name{makeKernel}
\Rdversion{1.1}
\alias{makeKernel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	constructs a "kernel" to be used for image operations
}
\description{
	a function to create an n-dimensional array of kernel weights using a supplied function that maps from distance to weight.
}
\usage{
makeKernel(kerndim, pdim, FUN = dnorm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{kerndim}{
	the dimensions (in grid units) of the kernel
}
  \item{pdim}{
	the dimensions of the grid units for the kernel	
}
  \item{FUN}{
	a function to map from distance to weight.
}
}
\details{
	this a convenience function for creating kernel functions that are wieghted by distance from the center coordinate.
}
\value{
	a list containing the kernel weights, the grid coordinates (relative to center voxel), and the real coordinates (relative to center coordinate)
}

\examples{
	kern <- makeKernel(c(3,3,3), c(1,1,1), exp)
}