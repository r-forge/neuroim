\name{loadVolumeList}
\alias{loadVolumeList}
\title{loadVolList}
\usage{
  loadVolumeList(fileNames, mask = NULL)
}
\arguments{
  \item{fileNames}{a list of files to load}

  \item{mask}{an optional mask indicating subset of voxels
  to load}
}
\value{
  an instance of class \code{\linkS4class{BrainVector}}
}
\description{
  load a list of image volumes and return a
  \code{\linkS4class{BrainVector}} instance
}

