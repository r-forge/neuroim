\name{BrainVolume-class}
\alias{BrainVolume-class}
\alias{BrainVolume}
\title{Three-dimensional brain image...}
\description{Three-dimensional brain image}
\section{Extends}{\code{\linkS4class{BrainData}}}
\section{Methods}{\describe{\item{\code{\link[=concat,BrainVector,BrainVolume-method]{concat}}}{}\item{\code{\link[=show,BrainVolume-method]{show}}}{}\item{\code{\link[=eachSlice,BrainVolume,function,missing-method]{eachSlice}}}{}\item{\code{\link[=eachSlice,BrainVolume,function,logical-method]{eachSlice}}}{}\item{\code{\link[=indexToGrid,BrainVolume,index-method]{indexToGrid}}}{}\item{\code{\link[=gridToIndex,BrainVolume,matrix-method]{gridToIndex}}}{}\item{\code{\link[=connComp,BrainVolume-method]{connComp}}}{\code{signature(x = "BrainVolume")}: find connected components in BrainVolume}\item{\code{\link[=writeVolume,BrainVolume,character,missing,missing-method]{writeVolume}}}{}\item{\code{\link[=writeVolume,BrainVolume,character,character,missing-method]{writeVolume}}}{}\item{\code{\link[=writeVolume,BrainVolume,character,missing,character-method]{writeVolume}}}{}\item{\code{\link[=Arith,BrainVolume,BrainVolume-method]{Arith}}}{}\item{\code{\link[=Arith,BrainVolume,BrainVolume-method]{Arith}}}{}\item{\code{\link[=Arith,BrainVector,BrainVolume-method]{Arith}}}{}}}
\alias{BrainVolume}
\usage{BrainVolume(data, space, source, label="", indices)
}
\details{\code{BrainVolume}: Construct a \code{\linkS4class{BrainVolume}} instance, using default (dense) implementation

}
\value{\code{BrainVolume}: \code{\linkS4class{DenseBrainVolume}} instance

}
\arguments{\item{data}{a three-dimensional \code{array}}
\item{space}{an instance of class \code{\linkS4class{BrainSpace}}}
\item{label}{a \code{character} string}
\item{source}{an instance of class \code{\linkS4class{BrainSource}}}
\item{indices}{an optional 1-d index vector}
}

