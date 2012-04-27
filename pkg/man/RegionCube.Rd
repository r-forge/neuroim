\name{RegionCube}
\alias{RegionCube}
\title{Create A Spherical Region of interest}
\usage{
  RegionCube(bvol, centroid, surround, mask = NULL)
}
\arguments{
  \item{bvol}{an image volume}

  \item{centroid}{the center of the cube in voxel space}

  \item{surround}{the number of voxels on either side of
  the central voxel}

  \item{mask}{an optional mask that wil be intersected with
  the ROI cube.}
}
\description{
  Create A Spherical Region of interest
}

