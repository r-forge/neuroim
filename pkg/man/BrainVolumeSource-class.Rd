\name{BrainVolumeSource-class}
\alias{BrainVolumeSource-class}
\alias{BrainVolumeSource}
\title{A class is used to produce a \linkS4class{BrainVolume} instance...}
\description{A class is used to produce a \code{\linkS4class{BrainVolume}} instance}
\section{Slots}{\describe{\item{\code{index}:}{(\code{\link{integer}}) the index of the volume to be read -- must be of length 1.}}}
\section{Extends}{\code{\linkS4class{BrainSource}}}
\section{Methods}{\describe{\item{\code{\link[=loadData,BrainVolumeSource-method]{loadData}}}{\code{signature( = "BrainVolumeSource")}: load BrainVolume}}}
\alias{BrainVolumeSource}
\usage{BrainVolumeSource(input, index=1)
}
\arguments{\item{input}{the input file name}
\item{index}{the image subvolume index}
}

