#' @include AllClass.R
{}
#' @include BrainVector.R
{}
#' @include common.R
{}
#' @include BrainMetaInfo.R
{}
#' @include NIFTI_IO.R
{}


#' BrainVolume
#' 
#' Construct a \code{\linkS4class{BrainVolume}} instance, using default (dense) implementation
#' @param data a three-dimensional \code{array}
#' @param space an instance of class \code{\linkS4class{BrainSpace}}
#' @param label a \code{character} string
#' @param source an instance of class \code{\linkS4class{BrainSource}}
#' @param indices an optional 1-d index vector
#' @return \code{\linkS4class{DenseBrainVolume}} instance 
#' @export BrainVolume
#' @rdname BrainVolume-class
BrainVolume <- function(data, space, source=NULL, label="", indices=NULL) {
	DenseBrainVolume(data,space,source=source, label=label, indices=indices)	
}

#' DenseBrainVolume
#' 
#' Construct a \code{\linkS4class{DenseBrainVolume}} instance
#' @param data a three-dimensional \code{array}
#' @param space an instance of class \code{\linkS4class{BrainSpace}}
#' @param source an instance of class \code{\linkS4class{BrainSource}}
#' @param label a \code{character} string
#' @param indices an optional 1-d index vector
#' @return \code{\linkS4class{DenseBrainVolume}} instance 
#' @export DenseBrainVolume
#' @rdname DenseBrainVolume-class
DenseBrainVolume <- function(data, space, source=NULL, label="", indices=NULL) {
	if (length(dim(data)) != 3) {
		stop("DenseBrainVolume: data argument must have three dimensions")
	} 
	
	if (ndim(space) != 3) {
		stop("DenseBrainVolume: space argument must have three dimensions")
	} 
	
	if (is.null(source)) {
		meta <- BrainMetaInfo(dim(data), spacing(space), origin(space), "FLOAT", label)
		source <- new("BrainSource", metaInfo=meta)
	}
	
	if (!is.null(indices)) {
		newdat <- array(0, dim(space))
		newdat[indices] <- data
		data <- newdat
	}
	
			
	new("DenseBrainVolume", .Data=data, source=source, space=space)

}

#' LogicalBrainVolume
#' 
#' Construct a \code{\linkS4class{LogicalBrainVolume}} instance
#' @param data a three-dimensional \code{array}
#' @param space an instance of class \code{\linkS4class{BrainSpace}}
#' @param source an instance of class \code{\linkS4class{BrainSource}}
#' @param label a \code{character} string
#' @param indices an optional 1-d index vector
#' @return \code{\linkS4class{LogicalBrainVolume}} instance 
#' @export LogicalBrainVolume
#' @rdname LogicalBrainVolume-class
LogicalBrainVolume <- function(data, space, source=NULL, label="", indices=NULL) {
	
	if (is.null(dim(data)) && length(data) == prod(dim(space))) {
		data <- array(data, dim(space))
	}
	
	if (length(dim(data)) != 3) {
		stop("LogicalBrainVolume: data argument must have three dimensions")
	} 
	
	if (ndim(space) != 3) {
		stop("LogicalVolume: space argument must have three dimensions")
	} 
	
	if (!is.null(indices)) {
		newdat <- array(FALSE, dim(space))
		newdat[indices] <- data
		data <- newdat
	}
	
	
	if (!is.logical(data)) {
		D <- dim(data)
		data <- as.logical(data)
		dim(data) <- D
	}
	
	if (is.null(source)) {
		meta <- BrainMetaInfo(dim(data), spacing(space), origin(space), "BINARY", label)
		source <- new("BrainSource", metaInfo=meta)
	}
	
	new("LogicalBrainVolume", source=source, .Data=data, space=space)
	
}

#' conversion from DenseBrainVolume to array
setAs(from="DenseBrainVolume", to="array", def=function(from) from@.Data)

#' conversion from BrainVolume to LogicalBrainVolume
setAs(from="BrainVolume", to="LogicalBrainVolume", def=function(from) {
	LogicalBrainVolume(as.array(from), space(from), from@source)
})

#' conversion from DenseBrainVolume to LogicalBrainVolume
setAs(from="DenseBrainVolume", to="LogicalBrainVolume", def=function(from) {
	LogicalBrainVolume(as.array(from), space(from), from@source)
})

#' conversion from BrainVolume to array
setAs(from="BrainVolume", to="array", def=function(from) from[,,])

#' @nord
setMethod(f="show",
		signature=signature(object="BrainVolume"),
			def=function(object) {
				
				cat("an instance of class",  class(object), "\n\n")
				cat("   dimensions: ",       dim(object), "\n")
				cat("   voxel spacing: ",    spacing(object), "\n")
				if (!is.null(attr(object, "label"))) {
					cat("   label: ", attr(object, "label"))
				}
				cat("\n\n")
			
		})

#' load BrainVolume
#' @exportMethod loadData
#' @rdname loadData-methods
setMethod(f="loadData", signature=c("BrainVolumeSource"), 
		def=function(x) {
			
			meta <- x@metaInfo
			nels <- prod(meta@Dim[1:3]) 
			offset <- prod(nels * (x@index-1)) * meta@bytesPerElement
			
			reader <- dataReader(meta, offset)
			dat <- readElements(reader, nels)
			close(reader)
			arr <- array(dat, meta@Dim[1:3])
			
			bspace <- BrainSpace(meta@Dim[1:3], meta@origin, meta@spacing, meta@spatialAxes)
			DenseBrainVolume(arr, bspace, x)
					
		})

#' Constructor for BrainVolumeSource
#' @param input the input file name
#' @param index the image subvolume index
#' @export BrainVolumeSource
#' @rdname BrainVolumeSource-class
BrainVolumeSource <- function(input, index=1) {
	stopifnot(index >= 1)
	stopifnot(is.character(input))
	
	if (!file.exists(input)) {
		candidates <- Sys.glob(paste(input, "*", sep=""))
		if (length(candidates) > 0) {
			input <- candidates[1]
		}
	}
	
	stopifnot(file.exists(input))
		
	metaInfo <- readHeader(input)
	
	if ( length(metaInfo@Dim) < 4 && index > 1) {
		stop("index cannot be greater than 1 for a image with dimensions < 4")
	}
	
	new("BrainVolumeSource", metaInfo=metaInfo, index=as.integer(index))								
	
}

#' load an image volume from a file
#' @param fileName the name of the file to load
#' @param index the index of the volume (e.g. if the file is 4-dimensional)
#' @export loadVolume
loadVolume  <- function(fileName, index=1) {
	src <- BrainVolumeSource(fileName, index)
	loadData(src)
}


#' concatenate two BrainVolumes
#' @param x the first volume
#' @param y the second volume
#' @param ... extra arguments of class BrainVolume or other concatenable objects
#' @note dimensions of x and y must be equal
#' @export concat
#' @rdname concat-methods
setMethod(f="concat", signature=signature(x="DenseBrainVolume", y="DenseBrainVolume"),
		def=function(x,y,...) {
			.concat4D(x,y,...)			
		})

#' @exportMethod eachSlice
#' @rdname eachSlice-methods
setMethod(f="eachSlice", signature=signature(x="BrainVolume", FUN="function", withIndex="missing"),
		def=function(x, FUN) {
			lapply(1:(dim(x)[3]), function(z) FUN(x[,,z]))				
		})

#' @exportMethod eachSlice
#' @rdname eachSlice-methods
setMethod(f="eachSlice", signature=signature(x="BrainVolume", FUN="function", withIndex="logical"),
		def=function(x, FUN, withIndex) {
			lapply(1:(dim(x)[3]), function(z) {					
				slice <- x[,,z]
				if (withIndex) FUN(slice,z) else FUN(slice)
			})
		})

#' @exportMethod indexToGrid
#' @rdname indexToGrid-methods
setMethod(f="indexToGrid", signature=signature(x="BrainSpace", idx="index"),
          def=function(x, idx) {
            array.dim <- dim(x)          
            t(sapply(idx, .indexToGrid, array.dim))            
          })
  
#' @exportMethod indexToGrid
#' @rdname indexToGrid-methods
setMethod(f="indexToGrid", signature=signature(x="BrainVector", idx="index"),
		  def=function(x, idx) {
			  callGeneric(space(x), idx)
		  })

#' @exportMethod indexToGrid
#' @rdname indexToGrid-methods
setMethod(f="indexToGrid", signature=signature(x="BrainVolume", idx="index"),
		  def=function(x, idx) {
			  callGeneric(space(x), idx)
		  })

#' @exportMethod gridToIndex
#' @rdname gridToIndex-methods
setMethod(f="gridToIndex", signature=signature(x="BrainVolume", coords="matrix"),
          def=function(x, coords) {
            array.dim <- dim(x)
            .gridToIndex(dim(x), coords)
          })

#' @nord
.pruneCoords <- function(coord.set,  vals,  mindist=10) {

	if (NROW(coord.set) == 1) {
		1
	}
	
	.prune <- function(keepIndices) {
		if (length(keepIndices) == 1) {
			keepIndices
		} else {
			ret <- yaImpute::ann(coord.set[keepIndices,], coord.set[keepIndices,], verbose=F,  k=2)$knn
			ind <- ret[, 2]
			ds <- sqrt(ret[, 4])
			v <- vals[keepIndices] 
			ovals <- v[ind]		
			pruneSet <- ifelse(ds < mindist & ovals > v,  TRUE, FALSE)
			
			if (any(pruneSet)) {
				Recall(keepIndices[!pruneSet])
			} else {
				keepIndices
			}
		}
		
		
	}
		
	.prune(1:NROW(coord.set))
	
	
	  
}

#' find connected components in BrainVolume
#' @exportMethod connComp
#' @rdname connComp-methods
setMethod(f="connComp", signature=signature(x="BrainVolume"), 
	def=function(x, threshold=0, coords=TRUE, clusterTable=TRUE, localMaxima=TRUE, localMaximaDistance=15) {
		mask <- (x > threshold)
		stopifnot(any(mask))
	
		comps <- connComp3D(mask)
		
		
	
		grid <- as.data.frame(indexToGrid(mask, which(mask>0)))
		colnames(grid) <- c("x", "y", "z")
		locations <- split(grid, comps$index[comps$index>0])
		
		ret <- list(size=BrainVolume(comps$size, space(x)), index=BrainVolume(comps$index, space(x)), voxels=locations)
		
				
		
		if (clusterTable) {
			maxima <- do.call(rbind, lapply(locations, function(loc) {
				if (nrow(loc) == 1) {
					loc
				} else {
					vals <- x[as.matrix(loc)]
					loc[which.max(vals),]
				}			
			}))
			N <- comps$size[as.matrix(maxima)]
			Area <- N * prod(spacing(x))
			maxvals <- x[as.matrix(maxima)]
			ret$clusterTable <- data.frame(index=1:NROW(maxima), x=maxima[,1], y=maxima[,2], z=maxima[,3], N=N, Area=Area, value=maxvals)			
		}
		
		if (localMaxima) {	
			if (all(sapply(locations, NROW) == 1)) {
				
			}	
			coord.sets <- lapply(locations, function(loc) {
				sweep(as.matrix(loc), 2, spacing(x), "*")
			})
			
	
		    
			loc.max <- do.call(rbind, mapply(function(cset, i) {	
				idx <- .pruneCoords(as.matrix(cset), x[as.matrix(locations[[i]])], mindist=localMaximaDistance)
				maxvox <- as.matrix(locations[[i]])[idx,,drop=F]
				cbind(i, maxvox)
			}, coord.sets, 1:length(coord.sets), SIMPLIFY=FALSE))
			
			
			loc.max <- cbind(loc.max, x[loc.max[, 2:4, drop=F]])
			
			row.names(loc.max) <- 1:NROW(loc.max)
			colnames(loc.max) <- c("index", "x", "y", "z", "value")
			ret$localMaxima <- loc.max
		}
		
		ret
		
		
	})
	
	
	
    



#' @exportMethod writeVolume
#' @rdname writeVolume-methods
setMethod(f="writeVolume",signature=signature(x="BrainVolume", fileName="character", format="missing", dataType="missing"),
		def=function(x, fileName) {
			write.nifti.volume(x, fileName)           
		})

#' @exportMethod writeVolume
#' @rdname writeVolume-methods
setMethod(f="writeVolume",signature=signature(x="BrainVolume", fileName="character", format="character", dataType="missing"),
		def=function(x, fileName, format) {
			if (toupper(format) == "NIFTI" || toupper(format) == "NIFTI1" || toupper(format) == "NIFTI-1") {
				callGeneric(x, fileName)
			} else {
				stop(paste("sorry, cannot write format: ", format))
			}      
		})

#' @exportMethod writeVolume
#' @rdname writeVolume-methods
setMethod(f="writeVolume",signature=signature(x="BrainVolume", fileName="character", format="missing", dataType="character"),
		def=function(x, fileName, dataType) {
			write.nifti.volume(x, fileName, dataType)   
			
		})





            
            
