\name{loadVector}
\alias{loadVector}
\title{loadVector...}
\usage{loadVector(fileName, indices, mask)
}
\description{loadVector}
\details{load an image volume from a file}
\value{an \code{\linkS4class{BrainVector}} object}
\arguments{\item{fileName}{the name of the file to load}
\item{indices}{the indices of the sub-volumes to load (e.g. if the file is 4-dimensional)}
\item{mask}{a mask defining the spatial elements to load}
}

