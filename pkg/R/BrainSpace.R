
BrainSpace <- function(Dim, origin=NULL, spacing=NULL, orientation=NULL, trans=NULL, reptime=1) {
  

    if (is.null(spacing)) {
      spacing <- rep(1, length(Dim))
    }    

    if (is.null(origin)) {
      origin <- rep(0, length(Dim))
    }

    if (is.null(orientation)) {
      orientation <- c("L", "P", "I")[1:length(Dim)]
    }
    
    if (is.null(trans)) {
      trans <- diag(c(spacing,1))
      trans[1:length(Dim),length(Dim)+1] <- origin
    }
    
    new("BrainSpace", Dim=as.integer(Dim),
        origin=origin,
        spacing=spacing,
        orientation=orientation,
        trans=trans,
        invTrans=solve(trans),
        reptime=reptime)
}
      
                                                                                              
setMethod("show", "BrainSpace",
    function(object) {
        cat("BrainSpace\n")
        cat("  Type           :", class(object), "\n")
        cat("  Dimension      :", object@Dim, "\n")
        cat("  Spacing        :", paste(paste(object@spacing[1:(length(object@spacing)-1)], " X ", collapse=" "), 
        object@spacing[length(object@spacing)], "\n"))
        cat("  Origin         :", paste(paste(object@origin[1:(length(object@origin)-1)], " X ", collapse=" "), 
        object@origin[length(object@origin)], "\n"))
        cat("  Index To World :", object@trans, "\n")
        
    }
)



setMethod("dim", signature(x = "BrainSpace"),
	  function(x) x@Dim, valueClass = "integer")


setMethod("numdim", signature(x = "BrainSpace"),
    function(x) length(x@Dim))
 

setMethod("spacing", signature(x = "BrainSpace"),
    function(x) x@spacing)


setMethod("bounds", signature(x = "BrainSpace"),
    function(x) {
        mat <- cbind(x@origin, x@origin+(x@spacing*x@Dim))
        row.names(mat) <- x@orientation
        return(mat)
    }
)

setMethod("origin", signature(x = "BrainSpace"),
    function(x) x@origin)

setMethod("orientation", signature(x = "BrainSpace"),
    function(x) x@orientation)

setMethod("trans", signature(x = "BrainSpace"),
    function(x) x@trans
)

setMethod("reptime", signature(x = "BrainSpace"),
    function(x) x@reptime
)

setMethod("invTrans", signature(x = "BrainSpace"),
  function(x) x@invTrans)


setReplaceMethod("trans", signature(x = "BrainSpace", value="matrix"),
    function(x, value) {
      if (any(dim(value) != c(numdim(x)+1,numdim(x)+1))) {
        stop("Incorrect dimension: BrainSpace@trans must be a 3X3 or 4X4 matrix")
      }
      
      x@trans <- value
      x@origin <- drop(value %*% c(0,0,0,1))
      x@invTrans <- solve(x@trans)
      x
    }
)

