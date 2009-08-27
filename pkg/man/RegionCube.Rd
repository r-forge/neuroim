\name{RegionCube}
\Rdversion{1.1}
\alias{RegionCube}
\docType{data}
\title{
 creates a BrainRegion3D instance that is a cube
}
\description{
 creation fucntion for a BrainRegion3D class that is a cube.
}

\usage{ RegionCube(bvol, centroid, surround, mask) }

\arguments{
        \item{bvol}{ the \code{BrainVolume} that on which the geometry of the region is based }
        \item{centroid}{ the center coordinates (in grid space) of the region }
        \item{surround}{ the number of surrounding voxels on each side of the center voxel }
		\item{mask}{a mask supplied as a 3d array the same shape as \code{bvol} that will be used to exlucde voxels in the cube that fall outside the mask}
		
}


