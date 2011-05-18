\name{DenseBrainVolume-class}
\alias{DenseBrainVolume-class}
\alias{DenseBrainVolume}
\title{DenseBrainVolume...}
\description{DenseBrainVolume}
\details{\code{DenseBrainVolume-class}: Three-dimensional brain image, backed by an \code{array}

\code{DenseBrainVolume}: Construct a \code{\linkS4class{DenseBrainVolume}} instance


}
\section{Extends}{\code{\linkS4class{BrainVolume}}, \code{\linkS4class{array}}}
\section{Methods}{\describe{\item{\code{\link[=concat,DenseBrainVolume,DenseBrainVolume-method]{concat}}}{\code{signature(x = "DenseBrainVolume", y = "DenseBrainVolume")}: concatenate two BrainVolumes}\item{\code{\link[=concat,DenseBrainVolume,DenseBrainVolume-method]{concat}}}{\code{signature(x = "DenseBrainVolume", y = "DenseBrainVolume")}: concatenate two BrainVolumes}}}
\alias{DenseBrainVolume}
\usage{DenseBrainVolume(data, space, source, label="", indices)
}
\value{\code{DenseBrainVolume}: \code{\linkS4class{DenseBrainVolume}} instance

}
\arguments{\item{data}{a three-dimensional \code{array}}
\item{space}{an instance of class \code{\linkS4class{BrainSpace}}}
\item{source}{an instance of class \code{\linkS4class{BrainSource}}}
\item{label}{a \code{character} string}
\item{indices}{an optional 1-d index vector}
}

