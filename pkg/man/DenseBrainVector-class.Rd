\name{DenseBrainVector-class}
\alias{DenseBrainVector}
\alias{DenseBrainVector-class}
\title{DenseBrainVector}
\usage{
  DenseBrainVector(data, space, source = NULL, label = "")
}
\arguments{
  \item{data}{a 4-dimensonal \code{array}}

  \item{space}{a \code{\linkS4class{BrainSpace}} object}

  \item{source}{an optional
  \code{\linkS4class{BrainSource}} object}

  \item{label}{a label of type \code{character}}
}
\value{
  \code{\linkS4class{DenseBrainVector}} instance
}
\description{
  Four-dimensional brain image, backed by an array

  constructor function for class
  \code{\linkS4class{DenseBrainVector}}
}

