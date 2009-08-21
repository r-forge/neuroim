\name{BrainList}
\alias{BrainList}
\title{ creates a new "BrainList" object }
\description{
  function to create a new object of class "BrainList"
}
\usage{
BrainList(volList, labels = NULL, files = NULL)
}

\arguments{
  \item{volList}{ the list of \code{BrainData} objects }
  \item{labels}{ the names of the objects, optional }
  \item{files}{ the file names of the images, optional }
}
\details{
  This class is used for storing related collections of brain images. It
  is simply a class extending \code{list} and \code{BrainData}.
  For instance, one may represent a set of MRIs across a group of
  subjects, each having the same geometry, as a "BrainList". Unlike,
  \code{BrainVector} instances, which are stored as arrays and
  suitable for representing multi-dimensional images such as FMRI
  time-series, \code{BrainList} objects are better suited for loose,
  possibly unordered collections of related data sets. Thus, "BrainList"
  objects are closely related to "bucket" data sets in AFNI.
}
\value{
  an object of class "BrainList"
}
\examples{
}
\keyword{manip}