


RegionCube <- function(bvol, centroid, surround, mask=NULL) {
  bspace <- space(bvol)

  vspacing <- spacing(bvol)
  vdim <- dim(bvol)
  
  mcentroid <- (centroid * vspacing)

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

  new("BrainRegion3D", space=space(bvol), data=vals, coords=grid)
  
}

RegionSphere <- function (bvol, centroid, radius) {
    bspace <- space(bvol)
    vspacing <- spacing(bvol)
    vdim <- dim(bvol)
  
    mcentroid <- ((centroid-1) * vspacing + vspacing/2)
    cubedim <- radius/vspacing

    nsamples <- max(cubedim)* 2 + 1
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
        sqrt(sum((gvals * vspacing - mcentroid)^2))
    })
    
    idx <- which(dvals <= radius)
    gsphere <- grid[idx, ]

    ## coercion to numeric is  ahack and needs to be fixed. subsetting of BrainVolume is broken?
    vals <- as.numeric(bvol[gsphere])
    volspace <- space(bvol)
    new("BrainRegion3D", space = space(bvol), data = vals, coords = gsphere)
}

#setMethod("indices", signature(x="BrainRegion3D"),
#          function(x) {
#            


#' @param x a BrainRegion3D object
#' @rdname coords-methods
setMethod(f="coords", signature=signature(x="BrainRegion3D"),
          function(x) {
            x@coords
          })

 
#' @param x a BrainRegion3D object
#' @rdname length-methods  
setMethod(f="length", signature=signature(x="BrainRegion3D"),
          function(x) {
            length(x@data)
          })

setMethod(f="[", signature=signature(x = "BrainRegion3D", i = "numeric", j = "missing", drop = "ANY"),
          function (x, i, j, drop) {
            x@data[i]
          })
          
.distance <- function(p1, p2) {
  diffs = (p1 - p2)
  return(sqrt(sum(diffs*diffs)))
}


makeKernel <- function(kerndim, pdim, FUN=dnorm) {
  if (length(kerndim) < 2) {
    stop("kernel dim length must be greater than 1")
  }
  
  kern <- array(0, kerndim)
  svec <- sapply(kerndim, function(d) ceiling(d/2 -1))
  dlist <- lapply(svec, function(sv) seq(-sv, sv))

  indmat <- do.call("expand.grid", dlist)
  cmat <- t(apply(indmat, 1, function(vals) sign(vals)* ifelse(vals == 0, 0, abs(vals)-.5)))
  cmat2 <- t(apply(cmat, 1, function(v) (v * pdim)))
  
  dvals <- apply(cmat2, 1, .distance, c(0,0,0))
  wts <- FUN(dvals)
  wts <- wts/sum(wts)

  wt.arr <- array(wts, kerndim)
  ret <- list(kern=kern, wts=wt.arr, indmat=indmat, coordmat=cmat2)


}
  

  
