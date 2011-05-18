\name{BrainBucketSource-class}
\alias{BrainBucketSource-class}
\alias{BrainBucketSource}
\title{BrainBucketSource...}
\description{BrainBucketSource}
\details{\code{BrainBucketSource-class}: A class that is used to produce a \code{\linkS4class{BrainBucket}} instance

\code{BrainBucketSource}: Constructor function for \code{\linkS4class{BrainBucketSource}} class


}
\section{Slots}{\describe{\item{\code{sourceList}:}{(\code{\link{list}}) a list of sources for the bucket sub-volumes}\item{\code{cache}:}{(\code{\link{environment}}) a cache used to store data in memory}}}
\section{Extends}{\code{\linkS4class{BrainVectorSource}}}
\section{Methods}{\describe{\item{\code{\link[=names,BrainBucketSource-method]{names}}}{}\item{\code{\link[=loadData,BrainBucketSource-method]{loadData}}}{\code{signature( = "BrainBucketSource")}: Load data from a \code{\linkS4class{BrainBucketSource}}}}}
\alias{BrainBucketSource}
\usage{BrainBucketSource(fileName, pattern, indices)
}
\arguments{\item{fileName}{the name of the bucket file}
\item{pattern}{optional regular expression used to filter the sub-volumes using associated labels}
\item{indices}{optional set of sub-volume indices to load}
}

