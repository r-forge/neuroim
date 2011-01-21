\name{SparseBrainVectorSource-class}
\alias{SparseBrainVectorSource-class}
\alias{SparseBrainVectorSource}
\title{A class that is used to produce a \linkS4class{SparseBrainVector} instance...}
\description{A class that is used to produce a \code{\linkS4class{SparseBrainVector}} instance}
\section{Slots}{\describe{\item{\code{mask}:}{(\code{\link{LogicalBrainVolume}}) the subset of voxels that will be stored in memory}}}
\section{Extends}{\code{\linkS4class{BrainVectorSource}}}
\section{Methods}{\describe{\item{\code{\link[=loadData,SparseBrainVectorSource-method]{loadData}}}{\code{signature( = "SparseBrainVectorSource")}: Load data from a \code{\linkS4class{SparseBrainVectorSource}}}}}
\alias{SparseBrainVectorSource}
\usage{SparseBrainVectorSource(metaInfo, indices, mask)
}
\details{\code{SparseBrainVectorSource}: constructs a SparseBrainVectorSource object

}
\arguments{\item{metaInfo}{an object of class \code{\linkS4class{BrainMetaInfo}}}
\item{indices}{a vector of 1D indices}
\item{mask}{a 3D \code{array} of type \code{logical}}
}

