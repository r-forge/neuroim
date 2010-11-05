\name{BrainVectorSource}
\alias{BrainVectorSource}
\title{Construct a \linkS4class{BrainVectorSource} object...}
\usage{BrainVectorSource(fileName, indices, mask)
}
\description{Construct a \code{\linkS4class{BrainVectorSource}} object}
\arguments{\item{fileName}{name of the 4-dimensional image file}
\item{indices}{the subset of volume indices to load -- if \code{NULL} then all volumes will be loaded}
\item{mask}{the subset of voxels that will be loaded}
}

