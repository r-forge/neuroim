#' @include AllClass.R
{}
#' @include AllGeneric.R
{}
#' @include common.R
{}
#' @include SparseBrainVector.R
{}



#' @nord 
.BrainVectorFromMatrix <- function(data, space) {
	nvols <- dim(space)[4]
	nelements <-  prod(dim(space)[1:3])
	
	if ( (dim(data)[1] == nvols) && (dim(data)[2] == nelements) ) {
		#fourth dimension is rows
		DenseBrainVector(t(data), space)        
	} else if ((dim(data)[2] == nvols) && (dim(data)[1] == nelements )) {
		#fourth dimension is columns
		DenseBrainVector(data, space=space)
	} else {
		stop(paste("illegal matrix dimension ", dim(data)))
	}
}




#' DenseBrainVector
#' 
#' constructor function for class \code{\linkS4class{DenseBrainVector}}
#' 
#' @param data a 4-dimensonal \code{array}
#' @param space a \code{\linkS4class{BrainSpace}} object
#' @param source an optional \code{\linkS4class{BrainSource}} object
#' @param label a label of type \code{character} 
#' @return \code{\linkS4class{DenseBrainVector}} instance 
#' @export 
#' @rdname DenseBrainVector-class
DenseBrainVector <- function(data, space, source=NULL, label="") {
	if (ndim(space) != 4) {
		stop("DenseBrainVector: data array must be 4-dimensional")
	} 
	
	if (is.null(source)) {
		meta <- BrainMetaInfo(dim(data), spacing(space), origin(space), "FLOAT", label)
		source <- new("BrainSource", metaInfo=meta)	
	}
	
	new("DenseBrainVector", .Data=data, source=source, space=space)
	
}


#' Load data from a \code{\linkS4class{BrainVectorSource}}
#' @param x an instance of class \code{\linkS4class{BrainVectorSource}}
#' @return an instance of class \code{\linkS4class{BrainVector}} 
#' @rdname loadData-methods
setMethod(f="loadData", signature=c("BrainVectorSource"), 
		def=function(x) {		
			meta <- x@metaInfo
			stopifnot(length(meta@Dim) == 4)
			
			meta <- x@metaInfo
			nels <- prod(meta@Dim[1:3]) 
			
			datlist <- list()
			ind <- x@indices
			
			for (i in 1:length(ind)) {
				offset <- prod(nels * (ind[i]-1)) * meta@bytesPerElement
				reader <- dataReader(meta, offset)		
				datlist[[i]] <- array(readElements(reader, nels), meta@Dim[1:3])
				close(reader)				
			}
			
			arr <- abind(datlist, along=4)			
			bspace <- BrainSpace(meta@Dim, meta@origin, meta@spacing, meta@spatialAxes)
			DenseBrainVector(arr, bspace, x)
			
		})




#' Construct a \code{\linkS4class{BrainVectorSource}} object
#' @param fileName name of the 4-dimensional image file
#' @param indices the subset of volume indices to load -- if \code{NULL} then all volumes will be loaded
#' @param mask the subset of voxels that will be loaded
#' @rdname BrainVectorSource-class
#' @export BrainVectorSource
BrainVectorSource <- function(fileName, indices=NULL, mask=NULL) {
	stopifnot(is.character(fileName))
	stopifnot(file.exists(fileName))
	
	
	metaInfo <- readHeader(fileName)
	
	if ( length(metaInfo@Dim) != 4) {
		stop(paste("file must be 4-dimensional, and it's not: ", paste(metaInfo@Dim, collapse= ",")))
	}
	
	if (is.null(indices)) {
		indices=seq(1, metaInfo@Dim[4])
	}
	
	if (is.null(mask)) {
		new("BrainVectorSource", metaInfo=metaInfo, indices=indices)		
	} else {
		SparseBrainVectorSource(metaInfo, indices, as(mask, "LogicalBrainVolume"))		
	}
	
}


#' @nord
setMethod("names", signature=c("BrainBucketSource"),
		def=function(x) {
			x@metaInfo@label[x@indices]
		})

#' @nord
setMethod("names", signature=c("BrainBucket"),
		def=function(x) {
			x@labels
		})

#' @nord
setMethod("length", signature=c("BrainVector"),
		def=function(x) {
			dim(x)[4]
		})

#' Load data from a \code{\linkS4class{BrainBucketSource}}
#' @param x an instance of class \code{\linkS4class{BrainBucketSource}}
#' @return an instance of class \code{\linkS4class{BrainVolume}} 
#' @docType methods
#' @rdname loadData-methods
setMethod(f="loadData", signature=signature("BrainBucketSource"), 
		def=function(x, key) {

			if (is.numeric(key)) {
				labs <- names(x)
				if (any(key < 1) || any(key > length(labs))) {
					stop(paste("illegal index: ", key))
				}
				
				key <- labs[key]							
			}
			
			ret <- lapply(key, function(k) {				
				haskey <- exists(k, envir=x@cache, inherits=FALSE)
				if (!haskey) {
					idx <- which(names(x) == k) 
					vol <- loadData(x@sourceList[[idx]])
					assign(k, vol, envir=x@cache)
				} else {
					#print(paste("found cached volume for label ", k))
					vol <- get(k, envir=x@cache, inherits=FALSE)
				}		
				attr(vol, "label") <- k
				vol
			})
	
			if (length(ret) == 1) {
				ret[[1]]
			} else {
				ret
			}
			
		})
#' BrainBucketSource
#' 
#' Constructor function for \code{\linkS4class{BrainBucketSource}} class
#' 
#' @param fileName the name of the bucket file
#' @param pattern optional regular expression used to filter the sub-volumes using associated labels
#' @param indices optional set of sub-volume indices to load
#' @export BrainBucketSource
#' @rdname BrainBucketSource-class
BrainBucketSource <- function(fileName, pattern=NULL, indices=NULL) {
	stopifnot(is.character(fileName))
	stopifnot(file.exists(fileName))
	
	
	metaInfo <- readHeader(fileName)
	
	labels <- metaInfo@label
	nvols <- length(labels)	
	
	if (is.null(indices)) {
		indices <- seq_along(labels)
	} else {
		stopifnot(all(indices >0 & indices < nvols))
	}
	
	if (!is.null(pattern)) {
		idx <- grep(pattern, labels)
		
		if (length(idx) < 1) {
			stop(paste("pattern: ", pattern, "does not match any volume labels"))
		}
		
		indices <- intersect(idx, indices)
	}
			
	
	sourceList <- lapply(indices, function(i) { new("BrainVolumeSource", metaInfo=metaInfo, index=as.integer(i)) })	
	new("BrainBucketSource", metaInfo=metaInfo, indices=indices, sourceList=sourceList, cache=new.env(hash=TRUE))	
}

#' loadBucket
#' 
#' load a BrainBucket object from file
#' 
#' @param fileName the name of the file to load
#' @param pattern optional regular expression used to filter the sub-volumes using associated labels
#' @param indices optional set of sub-volume indices to load
#' @export loadBucket
loadBucket <- function(fileName, pattern=NULL, indices=NULL) {
	bsource <- BrainBucketSource(fileName, pattern, indices)
	
	meta <- bsource@metaInfo	
	labels <- meta@label
	idx <- bsource@indices
	
	D <- c(meta@Dim[1:3], length(idx))
	bspace <- BrainSpace(D, meta@origin, meta@spacing, meta@spatialAxes)
	buck <- new("BrainBucket", source=bsource, space=bspace, labels=labels[idx])
}


#' @nord
setMethod(f="[[", signature=signature(x="BrainBucket", i = "character", j = "missing"),
		def=function(x, i) {
			loadData(x@source, i)
		})

#' @nord
setMethod(f="[[", signature=signature(x="BrainBucket", i = "numeric", j = "missing"),
		def=function(x, i) {
			loadData(x@source, i)
		})



setAs("DenseBrainVector", "array", function(from) from@.Data)
setAs("BrainVector", "array", function(from) from[,,,])


#' @nord
setMethod(f="show",
		signature=signature(object="BrainVectorSource"),
		def=function(object) {
			cat("an instance of class",  class(object), "\n\n")
			cat("   indices: ", object@indices, "\n\n")
			cat("   metaInfo: \n")
			show(object@metaInfo)
			cat("\n\n")
			
		})


#' @nord
setMethod("show",
		signature=signature(object="BrainVector"),
		def=function(object) {
			cat("an instance of class",  class(object), "\n\n")
			cat("   dimensions: ", dim(object), "\n")
			cat("   voxel spacing: ", spacing(object))
			cat("\n\n")
			
		})



#' @rdname eachVolume-methods
setMethod(f="eachVolume", signature=signature(x="BrainVector", FUN="function", withIndex="missing"),
		def=function(x, FUN, ...) {
			lapply(1:(dim(x)[4]), function(tt) FUN(x[,,,tt]))				
		})


#' @rdname eachVolume-methods
setMethod(f="eachVolume", signature=signature(x="BrainBucket", FUN="function", withIndex="missing"),
		def=function(x, FUN, ...) {
			lapply(1:(dim(x)[4]), function(tt) FUN(x[[tt]]))				
		})

#' @rdname eachVolume-methods
setMethod("eachVolume", signature=signature(x="BrainBucket", FUN="function", withIndex="logical"),
		def=function(x, FUN, withIndex, ...) {
			lapply(1:(dim(x)[4]), function(tt) {					
						vol <- x[[tt]]
						if (withIndex) FUN(vol,tt) else FUN(vol)
					})
		})


#' @rdname eachVolume-methods
setMethod("eachVolume", signature=signature(x="BrainVector", FUN="function", withIndex="logical"),
		def=function(x, FUN, withIndex, ...) {
			lapply(1:(dim(x)[4]), function(tt) {					
						vol <- x[,,,tt]
						if (withIndex) FUN(vol,tt) else FUN(vol)
					})
		})



#' @rdname takeVolume-methods
setMethod(f="takeVolume", signature=signature(x="BrainVector", i="numeric"),
		def=function(x, i, merge=FALSE) {
			
			xs <- space(x)
			bspace <- BrainSpace(dim(x)[1:3], origin=origin(xs), spacing=spacing(xs), axes(xs), trans(xs))
			
			makevol <- function(i) {
				bv <- BrainVolume(x@.Data[,,,i], bspace)
			}
			
			res <- lapply(i, makevol)
			
			if (length(res) > 1 && merge) {
				res <- do.call("concat", res)				
			}
			
			if (length(res) == 1) {
				res[[1]]
			} else {
				res
			}											
		})


#' @rdname eachSeries-methods
setMethod(f="eachSeries", signature=signature(x="BrainVector", FUN="function", withIndex="missing"),
		def=function(x, FUN, withIndex=FALSE, ...) {
			
			NX <- dim(x)[1]
			NY <- dim(x)[2]
			NZ <- dim(x)[3]
			ret <- vector("list", prod(NX, NY, NZ))
			
			for (i in 1:NZ) {
				for (j in 1:NY) {
					for (k in 1:NX) {
						ret[[index]] <- FUN(x[k,j,i,])
						index <- index+1
					}
				}
			}
			
			ret
			
		})

#loadSeries <- function(filenames, indices, volidx=NULL, reduce=T, demean=F, verbose=F, bulk.thresh=100 ) {
	#stopifnot(all(sapply(filenames, .isNIFTI)))
	
	#ret <- lapply(filenames, function(filename) {
	#			bvec <- loadVector(filename, indices)
	#			
	#			if (demean) {
	#				cmeans <- colMeans(retmat)
	#				retmat <- sweep(retmat, 2, cmeans)
	#			}
	#			
	#			if (reduce) {
	#				retmat <- rowMeans(retmat)
	#			}
	#			
	#			if (is.matrix(retmat) && NCOL(retmat) == 1) {
	#				retmat <- retmat[,1]
	#			}
	#			
	#			close(conn)
	#			
	#			retmat
	#		})
#	
#	
#	if (length(ret) == 1) {
#		ret[[1]]
#	} else {
#		ret
#	}
#}



#.loadSparseVector <- function(filename, mask) {
#	if (!.isNIFTI(filename)) {
#		stop("only support NIFTI files at present")
#	}
#	
#	nfile <- NIFTIFile(filename)
#	header <- readHeader(nfile)
#	ddim <- dataDim(header)
#	
#	if (length(ddim) != 4) {
#		stop("Error: file does not have 4 dimensions, which is required for a BrainVector object")
#	}
#	
#	
#	if (inherits(mask, "array") || inherits(mask, "BrainData")) {
#		# should check that dimensions are equal
#		ind <- which(mask > 0)
#	} else {
#		stop("mask must be of type array or BrainData")
#	}
#	
#	mat <- readData(nfile, ind)
#	
#	space <- createSpace(header)
#	SparseBrainVector(mat, space, indices=ind)
#}


#' loadVector
#' 
#' load an image volume from a file
#' 
#' @param fileName the name of the file to load
#' @param indices the indices of the sub-volumes to load (e.g. if the file is 4-dimensional)
#' @param mask a mask defining the spatial elements to load 
#' @return an \code{\linkS4class{BrainVector}} object
#' @export loadVector
loadVector  <- function(fileName, indices=NULL, mask=NULL) {
	src <- BrainVectorSource(fileName, indices, mask)
	loadData(src)
}




#setMethod("sliceMeans", signature(x="BrainVector"),
#          function(x) {
#             t(colMeans(x, dims=2))
#           })



#' @rdname concat-methods
setMethod(f="concat", signature=signature(x="BrainVector", y="BrainVolume"),
		def=function(x,y, ...) {
			.concat4D(x,y,...)			
		})


#' @rdname concat-methods
setMethod(f="concat", signature=signature(x="BrainVector", y="BrainVector"),
		def=function(x,y,...) {
			.concat4D(x,y,...)
		})


#' @rdname series-methods
setMethod("series", signature(x="BrainVector", i="matrix"),
		def=function(x,i) {
			if (!is.matrix(i) && length(i) == 3) {
				i <- matrix(i, 1, 3)
			}
			
			stopifnot(ncol(i) == 3)
			apply(i, 1, function(i) x[i[1], i[2], i[3],])
	
		})

#' @rdname series-methods
setMethod("series", signature(x="BrainVector", i="numeric"),
		def=function(x,i, j, k) {	
			if (missing(j) && missing(k)) {
				vdim <- dim(x)[1:3]
				mat <- arrayInd(i, vdim)
				apply(mat, 1, function(i) x[i[1], i[2], i[3],])			
			} else {
				x[i,j,k,]	
			}
		})

#' @rdname seriesIter-methods
setMethod(f="seriesIter", signature=signature(x="BrainVector"), 
		def=function(x) {
			len <- prod(dim(x)[1:3])
			vdim <- dim(x)[1:3]
			i <- 1
			nextEl <- function() {
				if (i <= len) {
					vox <- .indexToGrid(i, vdim)
					i <<- i + 1
					x[vox[1], vox[2], vox[3],]
					
				} else {
					stop("StopIteration") 
				}		
			}
			
			hasNx <- function() {
				i <= len
			}
			
			obj <- list(nextElem = nextEl, hasNext=hasNx) 
			class(obj) <- c("seriesIter", "abstractiter", "iter") 
			obj
			
			
		})


#' @nord
setAs(from="DenseBrainVector", to="matrix",
		function(from) {
			data <- from@.Data
			dm <- dim(data)
			d123 <- prod(dm[1:3])
			d4 <- dm[4]
			
			dim(data) <- c(d123,d4)
			return(data)
			
		})

#' @rdname as.matrix-methods
setMethod(f="as.matrix", signature=signature(x = "DenseBrainVector"), def=function(x) {
			as(x, "matrix")						
		})


#' @rdname as.sparse-methods
setMethod(f="as.sparse", signature=signature(x="DenseBrainVector", mask="numeric"),
		def=function(x, mask) {
			vdim <- dim(x)[1:3]
			m <- array(0, vdim)
			m[mask] <- TRUE
			
			logivol <- LogicalBrainVolume(m, dropDim(space(x)))
			
			dat <- as.matrix(x)[mask,]
			
			bvec <- SparseBrainVector(dat, space(x), logivol)
			
		})

# @exportMethod gridToIndex
# @rdname gridToIndex-methods
# setMethod(f="gridToIndex", signature=signature(x="BrainVector", coords="matrix"),
#		def=function(x, coords) {
#			stopifnot(ncol(coords) == 4)
#			array.dim <- dim(x)
#			ind3d <- .gridToIndex(dim(x), coords[,1:3])
#		})

#setMethod(f="takeSeries", signature=signature(x="BrainVector", indices="numeric"),
#		def=function(x, indices) {
#			
#			D <- dim(x)[1:3]
#			if (all.equal(dim(indices), D)) {
#				indices <- which(indices>0)
#			}
			
#			vox <- t(sapply(indices, .indexToGrid, D)) 
			
#			apply(vox, 1, function(v) {
#				x[v[1], v[2], v[3],]
#			})
			
#		})

#setMethod(f="takeSeries", signature=signature(x="BrainVector", indices="BrainVolume"),
#		def=function(x, indices) {				
#			D <- dim(x)[1:3]
#			stopifnot(all.equal(dim(indices), D))
#			
#			idx <- which(indices > 0)
#			callGeneric(x, idx)			
#			
#		})          


#' @param x an object of class \code{\linkS4class{BrainVector}}
#' @param fileName the output file name
#' @rdname writeVector-methods
setMethod(f="writeVector",signature=signature(x="BrainVector", fileName="character"),
		def=function(x, fileName) {
			stop()
			
		})





