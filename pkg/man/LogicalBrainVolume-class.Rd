\name{LogicalBrainVolume-class}
\alias{LogicalBrainVolume-class}
\alias{LogicalBrainVolume}
\title{Three-dimensional brain image where all values are either TRUE or FALSE...}
\description{Three-dimensional brain image where all values are either TRUE or FALSE}
\section{Extends}{\code{\linkS4class{DenseBrainVolume}}}
\alias{LogicalBrainVolume}
\usage{LogicalBrainVolume(data, space, source, label="", indices)
}
\details{\code{LogicalBrainVolume}: Construct a \code{\linkS4class{LogicalBrainVolume}} instance

}
\value{\code{LogicalBrainVolume}: \code{\linkS4class{LogicalBrainVolume}} instance

}
\arguments{\item{data}{a three-dimensional \code{array}}
\item{space}{an instance of class \code{\linkS4class{BrainSpace}}}
\item{source}{an instance of class \code{\linkS4class{BrainSource}}}
\item{label}{a \code{character} string}
\item{indices}{an optional 1-d index vector}
}

