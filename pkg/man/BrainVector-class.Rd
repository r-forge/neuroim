\name{BrainVector-class}
\alias{BrainVector-class}
\alias{BrainVector}
\title{Four-dimensional brain image...}
\description{Four-dimensional brain image}
\section{Extends}{\code{\linkS4class{BrainData}}}
\section{Methods}{\describe{\item{\code{\link[=length,BrainVector-method]{length}}}{}\item{\code{\link[=show,BrainVector-method]{show}}}{}\item{\code{\link[=eachVolume,BrainVector,function,missing-method]{eachVolume}}}{}\item{\code{\link[=eachVolume,BrainVector,function,logical-method]{eachVolume}}}{}\item{\code{\link[=takeVolume,BrainVector,numeric-method]{takeVolume}}}{}\item{\code{\link[=eachSeries,BrainVector,function,missing-method]{eachSeries}}}{}\item{\code{\link[=concat,BrainVector,BrainVolume-method]{concat}}}{}\item{\code{\link[=concat,BrainVector,BrainVector-method]{concat}}}{}\item{\code{\link[=concat,BrainVector,BrainVector-method]{concat}}}{}\item{\code{\link[=seriesIter,BrainVector-method]{seriesIter}}}{}\item{\code{\link[=writeVector,BrainVector,character,missing,missing-method]{writeVector}}}{}\item{\code{\link[=writeVector,BrainVector,character,character,missing-method]{writeVector}}}{}\item{\code{\link[=writeVector,BrainVector,character,missing,character-method]{writeVector}}}{}\item{\code{\link[=indexToGrid,BrainVector,index-method]{indexToGrid}}}{}\item{\code{\link[=Arith,BrainVector,BrainVector-method]{Arith}}}{}\item{\code{\link[=Arith,BrainVector,BrainVector-method]{Arith}}}{}\item{\code{\link[=Arith,BrainVector,BrainVolume-method]{Arith}}}{}}}
\alias{BrainVector}
\usage{BrainVector(data, space, mask, source, label="")
}
\details{\code{BrainVector}: constructor function for virtual class \code{\linkS4class{BrainVector}}

}
\value{\code{BrainVector}: a concrete instance of \code{\linkS4class{BrainVector}} class

}
\arguments{\item{data}{the image data}
\item{space}{a \code{\linkS4class{BrainSpace}} object}
\item{mask}{an optional \code{array} of type \code{logical}}
\item{source}{an optional \code{\linkS4class{BrainSource}} object}
\item{label}{a label of type \code{character}}
}

