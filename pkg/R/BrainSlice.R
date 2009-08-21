
BrainSlice <- function(data, space, indices=NULL) {
  if (numdim(space) != 2) {
    stop("incorrect dimension for BrainSlice")
  }
  
  if (is.null(indices)) {
    if (length(dim(data)) != 2) {
      data <- matrix(data, dim(space)[1], dim(space)[2])
    }
    return(new("BrainSlice", .Data=data, space=space))
  } else {
    mdat <- matrix(0, dim(space))
    mdat[indices] <- data
    return(new("BrainSlice", .Data=mdat, space=space))
  }
}


setMethod("gridToIndex", signature(x = "BrainSlice", coords="matrix"),
          function(x, coords) {
            
            dx <- dim(x)
            nsize <- prod(dx)
            idx <- apply(coords, 1, function(vox) {
              (vox[2]-1)*dx[1] + vox[1]
            })
            
            return(idx)
          })
          

setMethod("indexToGrid", signature(x = "BrainSlice", idx="index"),
	  function(x, idx) {            
            adim <- dim(x)
            idx <- as.integer(idx)          
            rank = 2
            
            wh1 <- idx-1
            wh <-1 + wh1 %% adim[1]
    
            denom = adim[1]                      
            nextd1 = wh1%/%denom
            wh2 = 1 + nextd1%%adim[2]
            
            ret = cbind(wh,wh2)[,,drop=T]
            names(ret) <- c("i", "j")

            return(ret)

          })
          

## "x[]":
#setMethod("[", signature(x = "BrainSlice",
#                         i = "missing", j = "missing", drop = "ANY"),
#          function (x, i, j, drop) x)


#setMethod("[", signature(x = "BrainSlice", i = "index", j = "missing",
#                         drop = "logical"),
#          function (x, i, drop) {
#              ret = x@data[i=i, , drop=drop]
#              
#          })

#setMethod("[", signature(x = "BrainSlice", i = "missing", j = "index",
#                         drop = "logical"),
#          function (x, j, drop) {
#              ret = x@data[, j=j, drop=drop]
#              
#          })

#setMethod("[", signature(x = "BrainSlice", i = "index", j = "index",
#                         drop = "logical"),
#          function (x, i, j, drop) {
#              ret = x@data[i=i, j=j, drop=drop]
#              
#          })

## missing 'drop' --> 'drop = TRUE'
##                     -----------
## select rows
#
#setMethod("[", signature(x = "BrainSlice", i = "index", j = "missing",
#                         drop = "missing"),
#          function(x,i,j, drop) callGeneric(x, i=i, drop= TRUE))
#
## select columns

#setMethod("[", signature(x = "BrainSlice", i = "missing", j = "index",
#                         drop = "missing"),
#          function(x,i,j, drop) callGeneric(x, j=j, drop= TRUE))

#setMethod("[", signature(x = "BrainSlice", i = "index", j = "index",
#                         drop = "missing"),
#          function(x,i,j, drop) callGeneric(x, i=i, j=j, drop= TRUE))

## bail out if any of (i,j,drop) is "non-sense"
#setMethod("[", signature(x = "BrainSlice", i = "ANY", j = "ANY", drop = "ANY"),
#          function(x,i,j, drop)
#          stop("invalid or not-yet-implemented 'BrainSlice' subsetting"))

