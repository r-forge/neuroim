#' @include AllClass.R
{}
#' @include Axis.R
{}

#' Constructor function for \code{\linkS4class{BrainSpace}} class
#' 
#' @param Dim a vector describing the dimensions of the spatial grid
#' @param origin the coordinate origin of the image space
#' @param spacing the real-valued voxel dimensions (usually in millimeters)
#' @param axes the image axes ordering (default is based on the NIFTI standard, Left-Posterior-Inferior)
#' @param trans a matrix representing the coordinate transformation associated with the image space (default is based on the NIFTI standard, Left-Posterior-Inferior)
#' @return an instance of class \code{\linkS4class{BrainSpace}}
#' @note one should rarely need to create a new \code{BrainSpace} instance, as it will almost always be created automatically using information stored in an image header.
#' Also, If one already has an existing image object, it's \code{BrainSpace} instance can be easily extracted with the \code{space} method.
#' @export
#' @rdname BrainSpace
#' @examples
#' bspace <- BrainSpace(c(64,64,64), origin=c(0,0,0), spacing=c(2,2,2))
#' print(bspace)
#' origin(bspace)
#' axes(bspace)
#' trans(bspace)
#' 
BrainSpace <- function(Dim, origin=NULL, spacing=NULL, axes=NULL, trans=NULL) {
	
	if (is.null(spacing)) {
		spacing <- rep(1, min(length(Dim), 3))
	}    
	
	if (is.null(origin)) {
		origin <- rep(0, min(length(Dim), 3))
	}
	
	if (is.null(trans)) {
		D <- min(length(Dim), 3)
		trans <- diag(c(spacing,1))
		trans[1:D,D+1] <- origin
	}
  
	if (is.null(axes)) {
	  axes <- .nearestAnatomy(trans)
	}
	
	new("BrainSpace", Dim=as.integer(Dim),
			origin=origin,
			spacing=spacing,
			axes=axes,
			trans=trans,
			inverseTrans=solve(trans))
}


#' @nord
setMethod(f="show", signature=signature("BrainSpace"),
		def=function(object) {
			cat("BrainSpace\n")
			cat("  Type           :", class(object), "\n")
			cat("  Dimension      :", object@Dim, "\n")
			cat("  Spacing        :", paste(paste(object@spacing[1:(length(object@spacing)-1)], " X ", collapse=" "), 
							object@spacing[length(object@spacing)], "\n"))
			cat("  Origin         :", paste(paste(object@origin[1:(length(object@origin)-1)], " X ", collapse=" "), 
							object@origin[length(object@origin)], "\n"))
			cat("  Axes           :", paste(object@axes@i@axis, object@axes@j@axis, object@axes@k@axis, "\n"))
			cat("  Coordinate Transform :", object@trans, "\n")
			
			
		}
)

#' add dimension to \code{\linkS4class{BrainSpace}}
#' 
#' function is used to add a dimension to a \code{\linkS4class{BrainSpace}} object
#' @name addDim
#' @docType methods
#' @param x an \code{\linkS4class{BrainSpace}} object
#' @param n the size of the dimension to be added
#' @export
#' @rdname addDim-methods
setMethod(f="addDim", signature=signature(x = "BrainSpace", n="numeric"),
		def=function(x, n) {
			BrainSpace(c(dim(x), n), origin(x), spacing(x), axes(x), trans(x))
		})


#' dropDim
#' @export
#' @rdname dropDim-methods
setMethod(f="dropDim", signature=signature(x = "BrainSpace", dimnum="missing"),
		def=function(x) {			
			D <- dim(x)		
			stopifnot(length(D) > 2)
			Dind <- 1:(length(D)-1)		
			
			### doesn't drop dimension in transformation matrix...
			BrainSpace(D[Dind], origin(x)[Dind], spacing(x)[Dind], dropDim(axes(x)), trans(x))
		})

#' dim
#' 
#' @export
#' @rdname dim-methods
#' @aliases dim,BrainSpace,ANY-method
setMethod(f="dim", signature=signature(x = "BrainSpace"),
		def=function(x) x@Dim)

#' ndim
#' 
#' @export
#' @rdname ndim-methods
setMethod(f="ndim", signature=signature(x = "BrainSpace"),
		def=function(x) length(x@Dim))

#' spacing
#' 
#' @export
#' @rdname spacing-methods
setMethod(f="spacing", signature=signature(x = "BrainSpace"),
		def=function(x) x@spacing)

#' bounds
#' 
#' @export
#' @rdname bounds-methods
setMethod(f="bounds", signature=signature(x = "BrainSpace"),
		def=function(x) {
      direc <- diag(trans(x))
      direc <- direc[1:(length(direc)-1)]
			mat <- cbind(origin(x), origin(x)+(spacing(x)*dim(x)*direc))
			return(mat)
		}
)

#' indexToGrid
#' 
#' @export indexToGrid
#' @rdname indexToGrid-methods
setMethod(f="indexToGrid", signature=signature(x="BrainSpace", idx="index"),
          def=function(x, idx) {
            array.dim <- dim(x)          
            t(sapply(idx, .indexToGrid, array.dim))            
          })

#' indexToCoord
#' 
#' @export 
#' @rdname indexToCoord-methods
setMethod(f="indexToCoord", signature=signature(x="BrainSpace", idx="index"),
          def=function(x, idx) {
            grid <- indexToGrid(x, idx) - .5
            res <- trans(x) %*% t(cbind(grid, rep(1,nrow(grid))))
            t(res[1:3,])
          })

#' coordToIndex
#' 
#' @export 
#' @rdname coordToIndex-methods
setMethod(f="coordToIndex", signature=signature(x="BrainSpace", coords="matrix"),
          def=function(x, coords) {
            grid = t(inverseTrans(x) %*% t(cbind(coords, rep(1, nrow(coords)))))
            gridToIndex(x, grid[,1:3] + 1)
          })

#' axisToIndex
#' 
#' @export 
#' @rdname axisToIndex-methods
setMethod(f="axisToIndex", signature=signature(x="BrainSpace", real="numeric", dimNum="numeric"),
          def=function(x, real, dimNum) {
            # todo check tat real is within bounds
            bds <- bounds(x)[dimNum,]           
            floor(abs(real - bds[1])/(spacing(x)[dimNum]) + 1)
            
          })

#' coordToGrid
#' 
#' @export 
#' @rdname coordToGrid-methods
setMethod(f="coordToGrid", signature=signature(x="BrainSpace", coords="matrix"),
          def=function(x, coords) {
            grid = t(inverseTrans(x) %*% t(cbind(coords, rep(1, nrow(coords)))))
            grid[,1:3]+ 1
          })

#' gridToIndex
#' 
#' @export 
#' @rdname gridToIndex-methods
setMethod(f="gridToIndex", signature=signature(x="BrainSpace", coords="matrix"),
		def=function(x, coords) {
			array.dim <- dim(x)
			.gridToIndex3D(dim(x), coords)
		})

#' gridToIndex
#' 
#' @export 
#' @rdname gridToIndex-methods
setMethod(f="gridToIndex", signature=signature(x="BrainSpace", coords="numeric"),
		def=function(x, coords) {
			array.dim <- dim(x)
			.gridToIndex3D(dim(x), matrix(coords, nrow=1, byrow=TRUE))
		})



#' origin
#' 
#' @export
#' @rdname origin-methods
setMethod(f="origin", signature=signature(x = "BrainSpace"),
		def=function(x) x@origin)


#' axes
#' 
#' @export
#' @rdname axes-methods
setMethod(f="axes", signature=signature(x = "BrainSpace"),
		def=function(x) x@axes)

#' trans
#' 
#' @export
#' @rdname trans-methods
setMethod(f="trans", signature=signature(x = "BrainSpace"),
		def=function(x) x@trans)

#' inverseTrans
#' 
#' @export
#' @rdname inverseTrans-methods
setMethod(f="inverseTrans", signature=signature(x = "BrainSpace"),
		def=function(x) x@inverseTrans)


## delegate methods

#' bounds
#' 
#' @export
#' @rdname bounds-methods
setMethod(f="bounds", signature=signature(x = "BrainData"),
		def=function(x) {
			bounds(space(x))
		})
#' axes
#' 
#' @export
#' @rdname axes-methods
setMethod(f="axes", signature=signature(x = "BrainData"),
		def=function(x) {
			axes(space(x))
		})

#' origin
#' 
#' @export
#' @rdname origin-methods
setMethod(f="origin", signature=signature(x = "BrainData"),
		def=function(x) {
			origin(space(x))
		})


#' trans
#' 
#' @export
#' @rdname trans-methods
setMethod(f="trans", signature=signature(x = "BrainData"),
		def=function(x) trans(space(x)))


#' inverseTrans
#' 
#' @export
#' @rdname inverseTrans-methods
setMethod(f="inverseTrans", signature=signature(x = "BrainData"),
		def=function(x) inverseTrans(space(x)))




