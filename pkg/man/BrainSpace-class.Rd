\name{BrainSpace-class}
\alias{BrainSpace-class}
\alias{BrainSpace}
\title{This class represents the geometry of a brain image...}
\description{This class represents the geometry of a brain image}
\section{Slots}{\describe{\item{\code{Dim}:}{(\code{\link{integer}}) the grid dimensions of the image}\item{\code{origin}:}{(\code{\link{numeric}}) the coordinates of the spatial origin}\item{\code{spacing}:}{(\code{\link{numeric}}) the dimensions (in mm) of the grid units (voxels)}\item{\code{axes}:}{(\code{\link{AxisSet}}) the set of named spatial axes}\item{\code{trans}:}{(\code{\link{matrix}}) an affine transformation matrix that moves from grid -> real world coordinates}\item{\code{inverseTrans}:}{(\code{\link{matrix}}) an inverse matrix that moves from real world -> grid coordinates}}}
\section{Methods}{\describe{\item{\code{\link[=show,BrainSpace-method]{show}}}{}\item{\code{\link[=addDim,BrainSpace,numeric-method]{addDim}}}{\code{signature(x = "BrainSpace", n = "numeric")}: add dimension to \code{\linkS4class{BrainSpace}}}\item{\code{\link[=dropDim,BrainSpace-method]{dropDim}}}{}\item{\code{\link[=dim,BrainSpace-method]{dim}}}{}\item{\code{\link[=ndim,BrainSpace-method]{ndim}}}{}\item{\code{\link[=spacing,BrainSpace-method]{spacing}}}{}\item{\code{\link[=bounds,BrainSpace-method]{bounds}}}{}\item{\code{\link[=origin,BrainSpace-method]{origin}}}{}\item{\code{\link[=axes,BrainSpace-method]{axes}}}{}\item{\code{\link[=trans,BrainSpace-method]{trans}}}{}\item{\code{\link[=inverseTrans,BrainSpace-method]{inverseTrans}}}{}\item{\code{\link[=indexToGrid,BrainSpace,index-method]{indexToGrid}}}{}}}
\alias{BrainSpace}
\usage{BrainSpace(Dim, origin, spacing, axes, trans)
}
\arguments{\item{Dim}{the dimensions}
\item{origin}{the coordinate origin of the image space}
\item{spacing}{the voxel dimensions}
\item{axes}{the images axes specification}
\item{trans}{the coordinate transformation associated with the image space}
}

