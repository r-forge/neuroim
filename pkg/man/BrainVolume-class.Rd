\name{BrainVolume-class}
\alias{BrainVolume}
\alias{BrainVolume-class}
\title{Three-dimensional brain image}
\usage{
  BrainVolume(data, space, source = NULL, label = "",
    indices = NULL)
}
\arguments{
  \item{data}{a three-dimensional \code{array}}

  \item{space}{an instance of class
  \code{\linkS4class{BrainSpace}}}

  \item{label}{a \code{character} string}

  \item{source}{an instance of class
  \code{\linkS4class{BrainSource}}}

  \item{indices}{an optional 1-d index vector}
}
\value{
  \code{\linkS4class{DenseBrainVolume}} instance
}
\description{
  Three-dimensional brain image

  Construct a \code{\linkS4class{BrainVolume}} instance,
  using default (dense) implementation
}

