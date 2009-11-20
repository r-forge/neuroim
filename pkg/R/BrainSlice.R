
BrainSlice <- function(data, space, indices=NULL) {
  if (numdim(space) != 2) {
    stop("incorrect dimension for BrainSlice")
  }
  
  if (is.null(indices)) {
    if (length(dim(data)) != 2) {
      data <- matrix(data, dim(space)[1], dim(space)[2])
    }
    new("BrainSlice", .Data=data, space=space)
  } else {
    mdat <- matrix(0, dim(space))
    mdat[indices] <- data
    new("BrainSlice", .Data=mdat, space=space)
  }
}


setMethod("gridToIndex", signature(x = "BrainSlice", coords="matrix"),
          function(x, coords) {            
            dx <- dim(x)
            nsize <- prod(dx)
            apply(coords, 1, function(vox) {
              (vox[2]-1)*dx[1] + vox[1]
            })
          })
          

setMethod("indexToGrid", signature(x = "BrainSlice", idx="index"),
	  function(x, idx) {            
            adim <- dim(x)
            idx <- as.integer(idx)          
            rank <- 2
            
            wh1 <- idx-1
            wh <-1 + wh1 %% adim[1]
    
            denom <- adim[1]                      
            nextd1 <- wh1%/%denom
            wh2 <- 1 + nextd1%%adim[2]
            
            ret <- cbind(wh,wh2)[,,drop=T]
            names(ret) <- c("i", "j")

            ret

          })
          


