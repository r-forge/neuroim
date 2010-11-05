#' @include AllClass.R
roxygen()
#' @include common.R
roxygen()
#' @include SparseBrainVector.R
roxygen()

#' @rdname private
.BrainVectorFromIndices <- function(data, space, indices) {
	nvols <- dim(space)[4]
	nelements <- prod(dim(space)[1:3])
	
	if (nvols == dim(data)[1]) {
		mat <- matrix(0, nelements, nvols)
		for (i in 1:nvols) {
			mat[indices,i] <- data[i,]
		}
		
	} else if (nvols == dim(data)[2]) {
		mat <- matrix(0, nelements, nvols)
		for (i in 1:nvols) {
			mat[indices,i] <- data[,i]
		}
	} else {
		stop(paste("illegal matrix dimension ", dim(data)))
	}
	
	
	dim(mat) <- dim(space)
	
	new("BrainVector", .Data=mat, space=space)
}

#' @rdname private
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

#' @rdname private
.createVectorSpaceFromData <- function(data, space, indices=NULL) {
	if (numdim(space) == 4) {
		return(space)
	}
	
	if (numdim(space) < 3) {
		stop(paste("incorrect dimensions : ", dim(space)))
	}
	
		
	if (numdim(space) == 3 && is.matrix(data)) {
		if (is.null(indices)) {
			nels <- prod(dim(space)[1:3])
		} else {
			nels <- length(indices)
		}
		
		if (nels == NROW(data)) {
			#cols is 4th dimension
			nvols <- NCOL(data)
			d <- c(dim(space), nvols)
			space <- BrainSpace(c(dim(space), nvols), origin=origin(space), spacing=spacing(space),
					orientation=orientation(space), trans=trans(space),
					reptime=nvols)
		} else if (nels == NCOL(data)) {
			#rows is 4th dimension
			nvols <- NROW(data)
			d <- c(dim(space), nvols)
			space <- BrainSpace(c(dim(space), nvols), origin=origin(space), spacing=spacing(space),
					orientation=orientation(space), trans=trans(space),
					reptime=nvols)
		}
	} else {
		stop(paste("space argument has incorrect dimensions : ", dim(space)))
	}
}



#' constructor function for class \code{\linkS4class{BrainVector}}
BrainVector <- function(data, space, indices=NULL, mask=NULL) {
	
	space <- .createVectorSpaceFromData(data, space, indices)
	
	bvec <- NULL
	
	if (is.null(indices)) {    
		if (is.matrix(data)) {
			bvec <- .BrainVectorFromMatrix(data, space)
		} else if ( all(dim(space) == dim(data)) ) {
			bvec <- new("BrainVector", .Data=data, space=space)
		} else {
			data <- array(data, c(dim(space)[1], dim(space)[2], dim(space)[3], dim(space)[4]))
			bvec <- new("BrainVector", .Data=data, space=space)
		}      
	} else {  
		bvec <- .BrainVectorFromIndices(data, space, indices)
	}
	
	bvec
}

#' constructor function for class \code{\linkS4class{DenseBrainVector}}
DenseBrainVector <- function(data, space, source=NULL, label="") {
	if (ndim(space) != 4) {
		stop("DenseBrainVector: data array must be 4-dimensional")
	} 
	
	if (is.null(source)) {
		meta <- BrainMetaInfo(dim(data), spacing(space), origin(space), "FLOAT", label)
		source <- new("BrainSource", metaInfo=meta)	
	}
	
	new("DenseBrainVector", source=source, .Data=data, space=space)
	
}


#' Load data from a \code{\linkS4class{BrainVectorSource}}
#' @param x an instance of class \code{\linkS4class{BrainVectorSource}}
#' @return an instance of class \code{\linkS4class{BrainVector}} 
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

#' get bucket names
setMethod("names", signature=c("BrainBucketSource"),
		def=function(x) {
			x@metaInfo@label[x@indices]
		})

#' get bucket names
setMethod("names", signature=c("BrainBucket"),
		def=function(x) {
			x@labels
		})

#' length of 4th dimension
setMethod("length", signature=c("BrainVector"),
		def=function(x) {
			dim(x)[4]
		})

#' Load data from a \code{\linkS4class{BrainBucketSource}}
#' @param x an instance of class \code{\linkS4class{BrainBucketSource}}
#' @return an instance of class \code{\linkS4class{BrainVolume}} 
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

#' Constructor function for BrainBucketSource
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

#' load brain bucket from file
loadBucket <- function(fileName, pattern=NULL, indices=NULL) {
	bsource <- BrainBucketSource(fileName, pattern, indices)
	
	meta <- bsource@metaInfo	
	labels <- meta@label
	idx <- bsource@indices
	
	D <- c(meta@Dim[1:3], length(idx))
	bspace <- BrainSpace(D, meta@origin, meta@spacing, meta@spatialAxes)
	buck <- new("BrainBucket", source=bsource, space=bspace, labels=labels[idx])
}

setMethod(f="[[", signature=signature(x="BrainBucket", i = "character", j = "missing"),
		def=function(x, i) {
			loadData(x@source, i)
		})

setMethod(f="[[", signature=signature(x="BrainBucket", i = "numeric", j = "missing"),
		def=function(x, i) {
			loadData(x@source, i)
		})



setAs("DenseBrainVector", "array", function(from) from@.Data)
setAs("BrainVector", "array", function(from) from[,,,])


setMethod(f="show",
		signature=signature(object="BrainVectorSource"),
		def=function(object) {
			cat("an instance of class",  class(object), "\n\n")
			cat("   indices: ", object@indices, "\n\n")
			cat("   metaInfo: \n")
			show(object@metaInfo)
			cat("\n\n")
			
		})

setMethod("show",
		signature=signature(object="BrainVector"),
		def=function(object) {
			cat("an instance of class",  class(object), "\n\n")
			cat("   dimensions: ", dim(object), "\n")
			cat("   voxel spacing: ", spacing(object))
			cat("\n\n")
			
		})


#' apply function to each volume in a BrainVector object
setMethod(f="eachVolume", signature=signature(x="BrainVector", FUN="function", withIndex="missing"),
		def=function(x, FUN, ...) {
			lapply(1:(dim(x)[4]), function(tt) FUN(x[,,,tt]))				
		})

#' apply function to each volume in a BrainBucket object
setMethod(f="eachVolume", signature=signature(x="BrainBucket", FUN="function", withIndex="missing"),
		def=function(x, FUN, ...) {
			lapply(1:(dim(x)[4]), function(tt) FUN(x[[tt]]))				
		})
#' apply function to each volume in a BrainVector object
setMethod("eachVolume", signature=signature(x="BrainBucket", FUN="function", withIndex="logical"),
		def=function(x, FUN, withIndex, ...) {
			lapply(1:(dim(x)[4]), function(tt) {					
						vol <- x[[tt]]
						if (withIndex) FUN(vol,tt) else FUN(vol)
					})
		})

#' apply function to each volume in a BrainVector object
setMethod("eachVolume", signature=signature(x="BrainVector", FUN="function", withIndex="logical"),
		def=function(x, FUN, withIndex, ...) {
			lapply(1:(dim(x)[4]), function(tt) {					
						vol <- x[,,,tt]
						if (withIndex) FUN(vol,tt) else FUN(vol)
					})
		})

#' extract one or more volumes from a  BrainVector object
setMethod(f="takeVolume", signature=signature(x="BrainVector", i="numeric"),
		def=function(x, i, merge=FALSE) {
			makevol <- function(i) {
				xs <- space(x)
				bspace <- BrainSpace(dim(x)[1:3], origin=origin(xs), spacing=spacing(xs), axes(xs), trans(xs))
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

loadSeries <- function(filenames, indices, volidx=NULL, reduce=T, demean=F, verbose=F, bulk.thresh=100 ) {
	stopifnot(all(sapply(filenames, .isNIFTI)))
	
	
	ret <- lapply(filenames, function(filename) {
				
				nfile <- NIFTIFile(filename)
				header <- readHeader(nfile)
				ddim <- dataDim(header)
				NT <- ddim[4]
				if (is.null(volidx)) {
					volidx <- 1:NT
				}
				
				if (any(volidx > ddim[4])) {
					stop("invalid volidx : index exceeds data dimenion")
				}
				
				# check for invalid volidx (i.e. < 0 or duplicates)
				if (length(ddim) < 4) {
					stop("loadSeries requires a four dimensional input file")
				}
				
				conn <- .openRead(nfile)
				rstorage <- .getRStorage(dataType(header))
				dsize <- .getDataSize(dataType(header))
				offset <- dataOffset(header)  
				seek(conn, where=offset, origin="start")
				indices <- sort(indices)
				
				
				VOLSIZE <- prod(ddim[1:3])
				ENDIAN <- endian(header)
				retmat <- matrix(0, length(volidx), length(indices))
				
				for (i in 1:length(volidx)) {
					vol <- volidx[i]
					VOLSTART <- VOLSIZE*(vol-1)*dsize + offset
					if (verbose) {
						print(paste("reading data from volume : ", vol))
					}
					
					
					if (length(indices) < bulk.thresh) {
						retmat[i,] <- sapply(indices, function(idx) {
									seek(conn, VOLSTART + ((idx-1)*dsize), origin="start")
									readBin(conn, what=rstorage, n=1,size=dsize, endian=ENDIAN)
								})
					} else {
						seek(conn, VOLSTART, origin="start")
						vals <- readBin(conn, what=rstorage, n=VOLSIZE,size=dsize, endian=ENDIAN)
						retmat[i,] <- vals[indices]
					}
				}
				
				if (demean) {
					cmeans <- colMeans(retmat)
					retmat <- sweep(retmat, 2, cmeans)
				}
				
				if (reduce) {
					retmat <- rowMeans(retmat)
				}
				
				if (is.matrix(retmat) && NCOL(retmat) == 1) {
					retmat <- retmat[,1]
				}
				
				close(conn)
				
				retmat
			})
	
	
	if (length(ret) == 1) {
		ret[[1]]
	} else {
		ret
	}
}



.loadSparseVector <- function(filename, mask) {
	if (!.isNIFTI(filename)) {
		stop("only support NIFTI files at present")
	}
	
	nfile <- NIFTIFile(filename)
	header <- readHeader(nfile)
	ddim <- dataDim(header)
	
	if (length(ddim) != 4) {
		stop("Error: file does not have 4 dimensions, which is required for a BrainVector object")
	}
	
	
	if (inherits(mask, "array") || inherits(mask, "BrainData")) {
		# should check that dimensions are equal
		ind <- which(mask > 0)
	} else {
		stop("mask must be of type array or BrainData")
	}
	
	mat <- readData(nfile, ind)
	
	space <- createSpace(header)
	SparseBrainVector(mat, space, indices=ind)
	
}

#' load an image volume from a file
#' @param fileName the name of the file to load
#' @param indices the indices of the sub-volumes to load (e.g. if the file is 4-dimensional)
#' @param mask a mask defining the spatial elements to load 
#' @export loadVector
loadVector  <- function(fileName, indices=NULL, mask=NULL) {
	src <- BrainVectorSource(fileName, indices, mask)
	loadData(src)
}




#setMethod("sliceMeans", signature(x="BrainVector"),
#          function(x) {
#             t(colMeans(x, dims=2))
#           })



setMethod(f="concat", signature=signature(x="BrainVector", y="BrainVolume"),
		def=function(x,y, ...) {
			.concat4D(x,y,...)			
		})


setMethod(f="concat", signature=signature(x="BrainVector", y="BrainVector"),
		def=function(x,y,...) {
			.concat4D(x,y,...)
		})

setMethod("series", signature(x="BrainVector", i="matrix"),
		def=function(x,i) {
			if (!is.matrix(i) && length(i) == 3) {
				i <- matrix(i, 1, 3)
			}
			
			stopifnot(ncol(mat) == 3)
			apply(mat, 1, function(i) x[i[1], i[2], i[3],])
	
		})

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

setMethod("as.sparse", signature(x="DenseBrainVector", mask="numeric"),
		def=function(x, mask) {
			vdim <- dim(x)[1:3]
			m <- array(0, vdim)
			m[mask] <- TRUE
			
			logivol <- LogicalBrainVolume(m, dropDim(space(x)))
			
			dat <- as.matrix(x)[mask,]
			
			bvec <- SparseBrainVector(dat, space(x), logivol)
			
		})

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


setMethod(f="writeVector",signature=signature(x="BrainVector", fileName="character"),
		def=function(x, fileName) {
			stop()
			
		})


setAs(from="BrainVector", to="matrix",
		def=function(from) {
			data <- from@.Data
			dm <- dim(data)
			d123 <- prod(dm[1:3])
			d4 <- dm[4]
			
			dim(data) <- c(d123,d4)
			data
			
		})


