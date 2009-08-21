\name{loadBrainList}
\alias{loadBrainList}
\title{ a function to create a "BrainList" object from a simple data.frame descriptor }
\description{
  a convenience function to create a "BrainList" object from a text file description
}

\usage{
loadBrainList(tableSpec, path = NULL)
}

\arguments{
  \item{tableSpec}{ a two-column data.frame containing the labels and
    image file names }
  \item{path}{ optional path to image files }
}
\details{
  the supplied data.frame must have two columns named "label" (for the
  image identifiers) and "file" for the image file names.  These are
  then used as key-value paris in the constructed \code{BrainList}
  object. It is assumed that each opf the imege files are
  three-dimensional and this can be loaded with the \code{loadVolume} function.
  
}
\value{
  an object of class \code{BrainList}
}


\keyword{manip}