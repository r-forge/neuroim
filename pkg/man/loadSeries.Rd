\name{loadSeries}
\Rdversion{1.1}
\alias{loadSeries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	load a set of time series from a 4-dimensional image.
}
\description{
	this function takes a set of volume-indices and loads the corresponding time series (in matrix form) froma 4-dimensional file. 
	
}
\usage{
loadSeries(filename, indices, volidx = NULL, reduce = T, demean = F, verbose = F, bulk.thresh = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{ the name of the image file.}
  \item{indices}{ the set of 1d indices (in volumetric space) }
  \item{volidx}{ the set of volumes to include }
  \item{reduce}{ apply a function across the set of time series to reduce it to a single column }
  \item{demean}{ remove the mean from each time series }
  \item{verbose}{ output io progress }
  \item{bulk.thresh}{ if number of indices > bulk.thresh, loadSeries will load in one volume at a time and then select the time series in memory. }
}

\value{
	a matrix of time series or a vector if reduce=TRUE
}

\author{
	Bradley Buchsbaum
}
