#' @include AllClass.R
roxygen()
#' @include Axis.R
roxygen()

BrainSpace <- function(Dim, origin=NULL, spacing=NULL, axes=NULL, trans=NULL) {
  

    if (is.null(spacing)) {
      spacing <- rep(1, min(length(Dim), 3))
    }    

    if (is.null(origin)) {
      origin <- rep(0, min(length(Dim), 3))
    }

    if (is.null(axes)) {
      axes <- OrientationList3D$AXIAL_LPI
    }
    
    if (is.null(trans)) {
		D <- min(length(Dim), 3)
      	trans <- diag(c(spacing,1))
      	trans[1:D,D+1] <- origin
    }
    
    new("BrainSpace", Dim=as.integer(Dim),
        origin=origin,
        spacing=spacing,
        axes=axes,
        trans=trans,
        inverseTrans=solve(trans))
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
		cat("  Axes           :", print(object@axes), "\n")
        cat("  Coordinate Transform :", object@trans, "\n")
		
        
    }
)



setMethod("dim", signature(x = "BrainSpace"),
	  function(x) x@Dim, valueClass = "integer")


setMethod("ndim", signature(x = "BrainSpace"),
    function(x) length(x@Dim))
 

setMethod("spacing", signature(x = "BrainSpace"),
    function(x) x@spacing)


setMethod("bounds", signature(x = "BrainSpace"),
    function(x) {
        mat <- cbind(origin(x), origin(x)+(spacing(x)*dim(x)))
        return(mat)
    }
)

setMethod("origin", signature(x = "BrainSpace"),
    function(x) x@origin)

setMethod("axes", signature(x = "BrainSpace"),
    function(x) x@axes)

setMethod("trans", signature(x = "BrainSpace"),
    function(x) x@trans)

setMethod("inverseTrans", signature(x = "BrainSpace"),
  function(x) x@inverseTrans)



