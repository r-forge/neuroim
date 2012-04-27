\name{BrainBucketSource}
\alias{BrainBucketSource}
\title{BrainBucketSource}
\usage{
  BrainBucketSource(fileName, pattern = NULL,
    indices = NULL)
}
\arguments{
  \item{fileName}{the name of the bucket file}

  \item{pattern}{optional regular expression used to filter
  the sub-volumes using associated labels}

  \item{indices}{optional set of sub-volume indices to
  load}
}
\description{
  Constructor function for
  \code{\linkS4class{BrainBucketSource}} class
}

