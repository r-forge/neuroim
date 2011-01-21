\name{BrainVectorSource-class}
\alias{BrainVectorSource-class}
\alias{BrainVectorSource}
\title{A class that is used to produce a \linkS4class{BrainVector} instance...}
\description{A class that is used to produce a \code{\linkS4class{BrainVector}} instance}
\section{Slots}{\describe{\item{\code{indices}:}{(\code{\link{integer}}) the index vector of the volumes to be loaded}}}
\section{Extends}{\code{\linkS4class{BrainSource}}}
\section{Methods}{\describe{\item{\code{\link[=loadData,BrainVectorSource-method]{loadData}}}{\code{signature( = "BrainVectorSource")}: Load data from a \code{\linkS4class{BrainVectorSource}}}\item{\code{\link[=show,BrainVectorSource-method]{show}}}{}}}
\alias{BrainVectorSource}
\usage{BrainVectorSource(fileName, indices, mask)
}
\arguments{\item{fileName}{name of the 4-dimensional image file}
\item{indices}{the subset of volume indices to load -- if \code{NULL} then all volumes will be loaded}
\item{mask}{the subset of voxels that will be loaded}
}

