\name{BrainMetaInfo-class}
\alias{BrainMetaInfo-class}
\alias{BrainMetaInfo}
\title{This class contains meta information from an image...}
\description{This class contains meta information from an image}
\section{Slots}{\describe{\item{\code{dataType}:}{(\code{\link{character}}) the data type code, e.g. FLOAT}\item{\code{Dim}:}{(\code{\link{numeric}}) image dimensions}\item{\code{spatialAxes}:}{(\code{\link{AxisSet3D}}) image axes for spatial dimensions (x,y,z)}\item{\code{additionalAxes}:}{(\code{\link{AxisSet}}) axes for dimensions > 3 (e.g. time, color band, direction)}\item{\code{spacing}:}{(\code{\link{numeric}}) voxel dimensions}\item{\code{origin}:}{(\code{\link{numeric}}) coordinate origin}\item{\code{label}:}{(\code{\link{character}}) name(s) of images}}}
\section{Extends}{\code{\linkS4class{BaseMetaInfo}}}
\alias{BrainMetaInfo}
\usage{BrainMetaInfo(Dim, spacing, origin=rep(0, length(spacing)), dataType="FLOAT",
    label="", spatialAxes=OrientationList3D$AXIAL_LPI,
    additionalAxes=NullAxis)
}
\value{\code{BrainMetaInfo}: an instance of class \code{\linkS4class{BrainMetaInfo}}

}
\arguments{\item{Dim}{image dimensions}
\item{spacing}{voxel dimensions}
\item{origin}{coordinate origin}
\item{dataType}{the type of the data (e.g. " FLOAT")}
\item{label}{name(s) of images}
\item{spatialAxes}{image axes for spatial dimensions (x,y,z)}
\item{additionalAxes}{axes for dimensions > 3 (e.g. time, color band, direction)}
}

