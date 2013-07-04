#' @include AllClass.R
{}
#' @include AllGeneric.R
{}


#' Create an instance of class ROIVolume
#' @param vspace the volume space
#' @param coords matrix of coordinates
#' @param data the data values
#' @export
ROIVolume <- function(vspace, coords, data=rep(length(indices),1)) {
  new("ROIVolume", space=vspace, data=data, coords=coords)
}
  

#' Create A Cuboid Region of interest
#' @param bvol an image volume
#' @param centroid the center of the cube in voxel space
#' @param surround the number of voxels on either side of the central voxel
#' @param mask an optional mask that wil be intersected with the ROI cube.
#' @export
RegionCube <- function(bvol, centroid, surround, mask=NULL) {
  bspace <- space(bvol)

  vspacing <- spacing(bvol)
  vdim <- dim(bvol)
  
  #mcentroid <- (centroid * vspacing)

  x <- round(seq(centroid[1]-surround, centroid[1]+surround))
  y <- round(seq(centroid[2]-surround, centroid[2]+surround))
  z <- round(seq(centroid[3]-surround, centroid[3]+surround))

  x <- x[x > 0 & x <= vdim[1]]
  y <- y[y > 0 & y <= vdim[2]]
  z <- z[z > 0 & z <= vdim[3]]

  
  if (all(c(length(x), length(y), length(z)) == 0)) {
    stop(paste("invalid sphere for centroid", centroid, " with surround", surround))
  }
  
  grid <- as.matrix(expand.grid(x=x,y=y,z=z))

  if (!is.null(mask)) {
    idx <- which(mask[grid] == 0)
    if (length(idx) > 0) {
      grid <- grid[-idx,]
    }
  }
   
  vals <- bvol[grid]
  new("ROIVolume", space=space(bvol), data=vals, coords=grid)
  
}


#' Create A Spherical Region of interest
#' @param bvol an image volume
#' @param centroid the center of the sphere in voxel space
#' @param radius the radius of the spherical ROI
#' @param fill optional value to assign to data slot
#' @param keep only nonzero elements from 'bvol' mask
#' @export
RegionSphere <- function (bvol, centroid, radius, fill=NULL, nonzero=TRUE) {
    ### TODO centroid doesn't work with matrix of one row
    bspace <- space(bvol)
    vspacing <- spacing(bvol)
    vdim <- dim(bvol)
  
    mcentroid <- ((centroid-1) * vspacing + vspacing/2)
    cubedim <- ceiling(radius/vspacing)

    nsamples <- max(cubedim) * 2 + 1
    vmat <- apply(cbind(cubedim, centroid), 1, function(cdim) {
      round(seq(cdim[2] - cdim[1], cdim[2] + cdim[1], length.out=nsamples))
    })

    vlist <- lapply(1:NCOL(vmat), function(i) {
      v <- vmat[,i]
      unique(v[v >= 1 & v <= vdim[i]])
    })
    
     
    if (all(sapply(vlist, length) == 0)) {
        stop(paste("invalid sphere for centroid", centroid, " with radius",
            radius))
    }
    
    grid <- as.matrix(expand.grid(x = vlist[[1]], y = vlist[[2]], z = vlist[[3]]))
    dvals <- apply(grid, 1, function(gvals) {
		coord <- (gvals-1) * vspacing + vspacing/2
        sqrt(sum((coord - mcentroid)^2))
    })
    
    idx <- which(dvals <= radius)
    ## coercion to numeric is  a hack and needs to be fixed. subsetting of BrainVolume is broken?
    
    vals <- if (!is.null(fill)) {
      rep(fill, length(idx))
    } else {
      as.numeric(bvol[grid[idx,]])
    }   
    
    keep <- if (nonzero) {
      which(vals != 0)    
    } else {
      seq_along(vals)
    }
    
    new("ROIVolume", space = space(bvol), data = vals[keep], coords = grid[idx[keep], ])
}

.resample <- function(x, ...) x[sample.int(length(x), ...)]

#' Create an Random Searchlight iterator
#' @param mask an image volume containing valid central voxels for roving searchlight
#' @param the radius in mm of spherical searchlight
#' @export
RandomSearchlight <- function(mask, radius) {
  done <- array(FALSE, dim(mask))
  mask.idx <- which(mask != 0)
  grid <- indexToGrid(mask, mask.idx)
  
  nextEl <- function() {
    if (!all(done[mask.idx])) {
      center <- .resample(which(!done[mask.idx]), 1)
      done[center] <<- TRUE
      search <- RegionSphere(mask, grid[center,], radius, nonzero=TRUE) 
      vox <- coords(search)
      vox <- vox[!done[vox],,drop=FALSE]
      done[vox] <- TRUE
      vox
      
    } else {
      stop('StopIteration')
    }
  }
  obj <- list(nextElem=nextEl)
  class(obj) <- c("RandomSearchLight", 'abstractiter', 'iter')
  obj
}

#' Create an exhaustive searchlight iterator
#' @param mask an image volume containing valid central voxels for roving searchlight
#' @param the radius in mm of spherical searchlight
#' @export
Searchlight <- function(mask, radius) {
	grid <- indexToGrid(mask, which(mask != 0))
	index <- 0
  
	nextEl <- function() {
		if (index < nrow(grid)) { 
			 index <<- index + 1
		 	 search <- RegionSphere(mask, grid[index,], radius, nonzero=TRUE) 
       search@coords
		} else {
			stop('StopIteration')
		}
	}
	
	obj <- list(nextElem=nextEl)
  class(obj) <- c("SearchLight", 'abstractiter', 'iter')
	obj
			
}

#' @nord
# conversion from ROIVolume to DenseBrainVolume
# @rdname as-methods
setAs(from="ROIVolume", to="DenseBrainVolume", def=function(from) {
  dat <- array(0, dim(from@space))
  dat[from@coords] <- from@data
  ovol <- DenseBrainVolume(dat, from@space, from@source)
})

#' indices
#' @param x an ROIVolume
#' 
#' @export 
setMethod("indices", signature(x="ROIVolume"),
          function(x) {
			  gridToIndex(x@space, x@coords)
		  })
            

#' coords
#' 
#' @param x an ROIVolume object
#' @export
#' @rdname coords-methods
setMethod(f="coords", signature=signature(x="ROIVolume"),
          function(x) {
            x@coords
          })

#' length
#' @export 
#' @nord
setMethod(f="length", signature=signature(x="ROIVolume"),
          function(x) {
            length(x@data)
		})

#' @export 
#' @nord
setMethod(f="[", signature=signature(x = "ROIVolume", i = "numeric", j = "missing", drop = "ANY"),
          function (x, i, j, drop) {
            x@data[i]
          })
  
#' @nord
 setMethod(f="show", signature=signature(object = "ROIVolume"),
		  function (object) {
			  cat("\n\n\tROIVolume", "\n")
			  cat("\tsize: ", length(object), "\n")
			  cat("\tparent dim:", dim(object), "\n")
			  cat("\tvoxel center of mass: ", colMeans(coords(object)), "\n")
		  })
  
#' @nord          
.distance <- function(p1, p2) {
  diffs = (p1 - p2)
  sqrt(sum(diffs*diffs))
}


#' Create a Kernel object
#' @param kerndim the dimensions in voxels of the kernel
#' @param vdim the dimensions of the voxels in real units
#' @param FUN the kernel function taking as its first argument representing the distance from the center of the kernel
#' @param ... additional parameters to the kernel FUN
#' @export
Kernel <- function(kerndim, vdim, FUN=dnorm, ...) {
  if (length(kerndim) < 2) {
    stop("kernel dim length must be greater than 1")
  }
  
  #kern <- array(0, kerndim)
  
  ## the half-width for each dimensions
  hwidth <- sapply(kerndim, function(d) ceiling(d/2 -1))
  
  ## note, if a kernel dim is even, this will force it to be odd numbered
  grid.vec <- lapply(hwidth, function(sv) seq(-sv, sv))

  # compute relative voxel locations (i.e. centered at 0,0,0)
  voxel.ind <- as.matrix(do.call("expand.grid", grid.vec))
  
  # fractional voxel locations so that the location of a voxel coordinate is centered within the voxel
  cvoxel.ind <- t(apply(voxel.ind, 1, function(vals) sign(vals)* ifelse(vals == 0, 0, abs(vals)-.5)))
  
  ## the coordinates ofthe voxels (i.e. after multiplying by pixel dims)
  coords <- t(apply(cvoxel.ind, 1, function(v) (v * vdim)))
  
  ## distance of coordinate from kernel center
  coord.dist <- apply(coords, 1, .distance, c(0,0,0))
  
  wts <- FUN(coord.dist, ...)
  wts <- wts/sum(wts)

  
  kern.weights <- wts
  
  new("Kernel", width=kerndim, weights=kern.weights, voxels=voxel.ind, coords=coords)

}


#' extract voxels from a \code{Kernel} object
#' @param kerndim the dimensions in voxels of the kernel
#' @param vdim the dimensions of the voxels in real units
#' @param FUN the kernel function taking as its first argument representing the distance from the center of the kernel
#' @param ... additional parameters to the kernel FUN
#' @export
setMethod(f="voxels", signature=signature(x="Kernel"),
          function(x, centerVoxel=NULL) {
            if (is.null(centerVoxel)) {
              x@voxels
            } else {
              sweep(x@voxels, 2, centerVoxel, "+")
            }
          })


  

  
