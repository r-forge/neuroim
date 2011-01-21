\name{SparseBrainVector-class}
\alias{SparseBrainVector-class}
\alias{SparseBrainVector}
\title{a sparse four-dimensional brain image, backed by a matrix, where each column represents...}
\description{a sparse four-dimensional brain image, backed by a \code{matrix}, where each column represents 
a vector spanning the fourth dimension (e.g. time)}
\section{Slots}{\describe{\item{\code{mask}:}{(\code{\link{LogicalBrainVolume}}) the mask defining the sparse domain}\item{\code{data}:}{(\code{\link{matrix}}) the matrix of series, where rows span across voxel space and columns span the fourth dimensions}\item{\code{map}:}{(\code{\link{IndexLookupVolume}}) instance of class \code{\linkS4class{IndexLookupVolume}} is used to map between spatial and index/row coordinates}}}
\section{Extends}{\code{\linkS4class{BrainVector}}}
\section{Methods}{\describe{\item{\code{\link[=indices,SparseBrainVector-method]{indices}}}{}\item{\code{\link[=coords,SparseBrainVector-method]{coords}}}{}\item{\code{\link[=eachSeries,SparseBrainVector,function-method]{eachSeries}}}{}\item{\code{\link[=seriesIter,SparseBrainVector-method]{seriesIter}}}{}\item{\code{\link[=series,SparseBrainVector,matrix-method]{series}}}{}\item{\code{\link[=concat,SparseBrainVector,SparseBrainVector-method]{concat}}}{}\item{\code{\link[=concat,SparseBrainVector,SparseBrainVector-method]{concat}}}{}\item{\code{\link[=lookup,SparseBrainVector,numeric-method]{lookup}}}{}\item{\code{\link[=[,SparseBrainVector,numeric,numeric-method]{[}}}{}\item{\code{\link[=takeVolume,SparseBrainVector,numeric-method]{takeVolume}}}{}\item{\code{\link[=writeVector,SparseBrainVector,character-method]{writeVector}}}{}}}
\alias{SparseBrainVector}
\usage{SparseBrainVector(data, space, mask, source, label="")
}
\details{\code{SparseBrainVector}: constructs a SparseBrainVector object

}
\arguments{\item{data}{an array which can be a \code{matrix} or 4-D \code{array}}
\item{space}{a BrainSpace instance}
\item{mask}{a 3D \code{array} of type \code{logical}}
\item{source}{the data source -- an instance of class \code{\linkS4class{BrainSource}}}
\item{label}{associated sub-image labels}
}

