\name{LogicalBrainVolume-class}
\alias{LogicalBrainVolume-class}
\alias{LogicalBrainVolume}
\title{LogicalBrainVolume...}
\description{LogicalBrainVolume}
\details{\code{LogicalBrainVolume-class}: Three-dimensional brain image where all values are either TRUE or FALSE

\code{LogicalBrainVolume}: Construct a \code{\linkS4class{LogicalBrainVolume}} instance


}
\section{Extends}{\code{\linkS4class{DenseBrainVolume}}}
\alias{LogicalBrainVolume}
\usage{LogicalBrainVolume(data, space, source, label="", indices)
}
\value{\code{LogicalBrainVolume}: \code{\linkS4class{LogicalBrainVolume}} instance

}
\arguments{\item{data}{a three-dimensional \code{array}}
\item{space}{an instance of class \code{\linkS4class{BrainSpace}}}
\item{source}{an instance of class \code{\linkS4class{BrainSource}}}
\item{label}{a \code{character} string}
\item{indices}{an optional 1-d index vector}
}

