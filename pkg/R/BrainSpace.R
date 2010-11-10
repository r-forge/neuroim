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


setMethod(f="show", signature=signature("BrainSpace"),
		def=function(object) {
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

#' add dimension to \code{\linkS4class{BrainSpace}}
#' 
#' function is used to add a dimension to a \code{\linkS4class{BrainSpace}} object
#' @param x an \code{\linkS4class{BrainSpace}} object
#' @param n the size of the dimension to be added
#' @rdname addDim-methods
setMethod(f="addDim", signature=signature(x = "BrainSpace", n="numeric"),
		def=function(x, n) {
			BrainSpace(c(dim(x), n), origin(x), spacing(x), axes(x), trans(x))
		})

#' @rdname addDim-methods
setMethod(f="dropDim", signature=signature(x = "BrainSpace"),
		def=function(x) {			
			D <- dim(x)		
			stopifnot(length(D) > 2)
			Dind <- 1:(length(D)-1)		
			
			### doesn't drop dimension in transformation matrix...
			BrainSpace(D[Dind], origin(x)[Dind], spacing(x)[Dind], axes(x), trans(x))
		})

setMethod(f="dim", signature=signature(x = "BrainSpace"),
		def=function(x) x@Dim)


setMethod(f="ndim", signature=signature(x = "BrainSpace"),
		def=function(x) length(x@Dim))


setMethod(f="spacing", signature=signature(x = "BrainSpace"),
		def=function(x) x@spacing)


setMethod(f="bounds", signature=signature(x = "BrainSpace"),
		def=function(x) {
			mat <- cbind(origin(x), origin(x)+(spacing(x)*dim(x)))
			return(mat)
		}
)


setMethod(f="origin", signature=signature(x = "BrainSpace"),
		def=function(x) x@origin)

setMethod(f="axes", signature=signature(x = "BrainSpace"),
		def=function(x) x@axes)

setMethod(f="trans", signature=signature(x = "BrainSpace"),
		def=function(x) x@trans)

setMethod(f="inverseTrans", signature=signature(x = "BrainSpace"),
		def=function(x) x@inverseTrans)


## delegate methods


setMethod(f="bounds", signature=signature(x = "BrainData"),
		def=function(x) {
			bounds(space(x))
		})

setMethod(f="axes", signature=signature(x = "BrainData"),
		def=function(x) {
			axes(space(x))
		})

setMethod(f="origin", signature=signature(x = "BrainData"),
		def=function(x) {
			origin(space(x))
		})


setMethod(f="trans", signature=signature(x = "BrainData"),
		def=function(x) trans(space(x)))

setMethod(f="inverseTrans", signature=signature(x = "BrainData"),
		def=function(x) inverseTrans(space(x)))




