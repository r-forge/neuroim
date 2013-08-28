\name{RegionSphere}
\alias{RegionSphere}
\title{Create A Spherical Region of interest}
\usage{
  RegionSphere(bvol, centroid, radius, fill = NULL,
    nonzero = TRUE)
}
\arguments{
  \item{bvol}{an image volume}

  \item{centroid}{the center of the sphere in voxel space}

  \item{radius}{the radius of the spherical ROI}

  \item{fill}{optional value to assign to data slot}

  \item{nonzero}{keep only nonzero elements from 'bvol'
  mask}
}
\description{
  Create A Spherical Region of interest
}

