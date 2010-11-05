\name{BrainBucketSource-class}
\alias{BrainBucketSource-class}
\alias{BrainBucketSource}
\title{A class that is used to produce a \linkS4class{BrainBucket} instance...}
\description{A class that is used to produce a \code{\linkS4class{BrainBucket}} instance}
\section{Slots}{\describe{\item{\code{sourceList}:}{(\code{\link{list}}) a list of sources for the bucket sub-volumes}\item{\code{cache}:}{(\code{\link{environment}}) a cache used to store data in memory}}}
\section{Extends}{\code{\linkS4class{BrainVectorSource}}}
\section{Methods}{\describe{\item{\code{\link[=names,BrainBucketSource-method]{names}}}{\code{signature( = “BrainBucketSource”)}: get bucket names}\item{\code{\link[=loadData,BrainBucketSource-method]{loadData}}}{\code{signature( = “BrainBucketSource”)}: Load data from a \code{\linkS4class{BrainBucketSource}}}}}

