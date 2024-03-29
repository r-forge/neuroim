\name{SparseBrainVector-class}
\alias{SparseBrainVector}
\alias{SparseBrainVector-class}
\title{SparseBrainVector}
\usage{
  SparseBrainVector(data, space, mask, source = NULL,
    label = "")
}
\arguments{
  \item{data}{an array which can be a \code{matrix} or 4-D
  \code{array}}

  \item{space}{a BrainSpace instance}

  \item{mask}{a 3D \code{array} of type \code{logical}}

  \item{source}{the data source -- an instance of class
  \code{\linkS4class{BrainSource}}}

  \item{label}{associated sub-image labels}
}
\description{
  a sparse four-dimensional brain image, backed by a
  \code{matrix}, where each column represents a vector
  spanning the fourth dimension (e.g. time)

  constructs a SparseBrainVector object
}

