\name{eachSeries}
\alias{eachSeries}
\title{Generic functions to apply a function to each series of a 4D image
That is, if the 4th dimension is 'time' each series is a 1D time series.}
\arguments{
  \item{x}{a four dimensional image}

  \item{FUN}{a \code{function} taking one or two arguments
  (depending on the value of \code{withIndex}}

  \item{withIndex}{whether the index of the series is
  supplied as the second argument to the function}
}
\description{
  Generic functions to apply a function to each series of a
  4D image That is, if the 4th dimension is 'time' each
  series is a 1D time series.

  eachSeries

  eachSeries
}

