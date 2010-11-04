


#TiledBrainVector <- function(fname, mask, ntiles=5, capacity=.5) {
#  stopifnot(capacity <= 1 && capacity > 0)
#  
#  indices <- which(mask > 0)
  
#  indexList <- split(indices, cut(1:length(indices),ntiles))
#  names(indexList) <- seq(1,length(indexList))
#  new("TiledBrainVector", filename=fname, indexList=indexList, mask=mask, capacity=capacity) 

#}

#setMethod("initialize", "TiledBrainVector", function(.Object, filename, mask, indexList, capacity) {
#  .Object@filename <- filename
#  .Object@cache <- vector("list", length(indexList))
#  .Object@indexList <- indexList
#  .Object@mask <- mask
#  .Object@capacity <- capacity
#  .Object
#})
  
  
SparseBrainVectorSource <- function(metaInfo, indices, mask) {
	stopifnot(length(dim(metaInfo)) == 4)
	stopifnot(all(indices >= 1 & indices <= dim(metaInfo)[4]))
	
	
	D <- dim(metaInfo)[1:3]
	if (all.equals(dim(mask), D)) {
		mask <- as.array(mask)
	} else if (is.vector(mask) && length(mask) == prod(D)) {
		mask <- array(mask, D)
	} else {
		stop("mask must have same number of elements as input file: ", D)
	}
	
	new("SparseBrainVectorSource", metaInfo=metaInfo, indices=indices, mask=mask)				
}

	
SparseBrainVector <- function(data, space, mask, source=NULL, label="") {
	stopifnot(inherits(space, "BrainSpace"))
	
	if (is.logical(mask) && !inherits(mask, "LogicalBrainVolume")) {
		mspace <- BrainSpace(dim(space)[1:3], origin(space), axes(space), trans(space))
		mask <- LogicalBrainVolume(mask, mspace)
	}
	
	stopifnot(inherits(mask, "LogicalBrainVolume"))

	
	D4 <- 0
	if (is.matrix(data)) {
		Nind <- sum(mask == TRUE)
		if (nrow(data) == Nind) {
			D4 <- ncol(data)	
		} else if (ncol(data) == Nind) {
			D4 <- nrow(data)
		} else {
			stop(paste("matrix with dim:", dim(data), " does not match mask cardinality: ", Nind))
		}
	} else if (length(dim(data)) == 4) {
		D4 <- dim(data)[4]
		mat <- apply(data, 4, function(vals) vals)
		data <- mat[mask==TRUE,]
	}
	
	if (ndim(space) == 3) {
		space <- addDim(space, nrow(data))
	}
		
		
  	stopifnot(ndim(space) == 4)
	
	if (is.null(source)) {
		meta <- BrainMetaInfo(dim(space), spacing(space), origin(space), "FLOAT", label)
		source <- new("BrainSource", metaInfo=meta)	
	}
	
	
  
  	new("SparseBrainVector", source=source, space=space, mask=mask, data=data, map=IndexLookupVolume(space(mask), as.integer(which(mask))))
  
}

#' Load data from a \code{\linkS4class{SparseBrainVectorSource}}
#' @param x an instance of class \code{\linkS4class{SparseBrainVectorSource}} 
#' @return an instance of class \code{\linkS4class{SparseBrainVector}} 
setMethod(f="loadData", signature=c("SparseBrainVectorSource"), 
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
				datlist[[i]] <- readElements(reader, nels)[mask]
				close(reader)				
			}
			
			arr <- do.call(rbind, datlist)		
			bspace <- BrainSpace(meta@Dim, meta@origin, meta@spacing, meta@spatialAxes)
			SparseBrainVector(arr, bspace, x)
			
		})


          
setMethod(f="indices", signature=signature(x="SparseBrainVector"),
          def=function(x) {
            indices(x@map)
          })
          
setMethod(f="indices", signature=signature(x="TiledBrainVector"),
          def=function(x) {
            ret <- unlist(x@indexList)
            names(ret) <- NULL
            ret
          })
  

setMethod(f="coords", signature=signature(x="TiledBrainVector"),
          def=function(x,i) {
            if (missing(i)) {
              return(indexToGrid(space(mask), indices(x)))
            }
            indexToGrid(space(mask), i)
          })
            
            
            
setMethod(f="coords", signature=signature(x="SparseBrainVector"),
          def=function(x,i) {           
            if (missing(i)) {
              return(coords(x@map, indices(x@map)))
            }
            coords(x@map, i)            
          })            

setMethod(f="eachSeries", signature=signature(x="SparseBrainVector", FUN="function"),
          def=function(x, FUN, withIndex=FALSE) {
            ret <- list()
            if (withIndex) {
              idx <- indices(x)
              for (i in 1:NCOL(x@data)) {
                ret[[i]] <- FUN(x@data[,i], idx[i])
              }
            } else {
              for (i in 1:NCOL(x@data)) {
                ret[[i]] <- FUN(x@data[,i])
              }
            }

            ret
          })


setMethod(f="seriesIter", signature=signature(x="SparseBrainVector"), 
	def=function(x) {
		len <- NCOL(x@data)
		i <- 0
		nextEl <- function() {
			i <<- i+1
			if (i <= len) {
				x@data[,i]
			} else {
				stop("StopIteration") 
			}		
		}

		hasNx <- function() {
			i < len
		}

		obj <- list(nextElem = nextEl, hasNext=hasNx) 
		class(obj) <- c("seriesIter", "abstractiter", "iter") 
		obj


	})


            
setMethod(f="series", signature=signature(x="TiledBrainVector", i="numeric"),
	def=function(x,i, j, k) {
		stop()         
       })
              
            
setMethod(f="series", signature=signature(x="TiledBrainVector", i="matrix"),
         def=function(x,i) {
           idx <- gridToIndex(x@mask, i)
           callGeneric(x,idx)
         })
  

setMethod(f="series", signature=signature(x="SparseBrainVector", i="matrix"),
         def=function(x,i) {
           idx <- gridToIndex(x@mask, i)
           callGeneric(x,idx)
         })
           

setMethod(f="series", signature=signature(x="SparseBrainVector", i="numeric"),
         def=function(x,i, j, k) {
           if (missing(j) && missing(k)) {
             if (length(i) == 3) {
               return(x[i[1], i[2], i[3]])
             }
             
             idx <- lookup(x, as.integer(i))
             idx <- idx[idx!=0]

             if (length(idx) == 0) {
               rep(0, dim(x)[4])
             } else {
               mat <- matrix(0, dim(x)[4], length(idx))
               mat[,idx != 0] <- x@data[,idx]               
             }
           } else if (missing(k)) {
             stop("must supply either linearized index (i) or grid location (i,j,k)")
           } else {
             
             xdim <- dim(x)
             slicedim <- xdim[1] * xdim[2]
             idx <- slicedim*(k-1) + (j-1)*xdim[1] + i
             x@data[,idx]
           }
         })


setMethod(f="concat", signature=signature(x="SparseBrainVector", y="SparseBrainVector"),
          def=function(x,y,...) {
            if (!all(indices(x) == indices(y))) {
              stop("cannot concatenate arguments with different index maps")
            }

            ndat <- rbind(x@data, y@data)
            d1 <- dim(x)
            d2 <- dim(y)
            
            ndim <- c(d1[1:3], d1[4] + d2[4])
            nspace <- BrainSpace(ndim, origin(x@space), spacing(x@space),  orientation(x@space), trans(x@space), reptime=1)
  
            
            ret <- SparseBrainVector(ndat, nspace, mask=x@mask)
            rest <- list(...)
            
            if (length(rest) >= 1) {
              ret <- Reduce("concat", c(ret, rest))
            }

            return(ret)
            
          })
          
          
          
setMethod(f="lookup", signature=signature(x="SparseBrainVector", i="numeric"),
         def=function(x,i) {
            lookup(x@map, i)
          })
                      

setMethod(f="[", signature=signature(x = "SparseBrainVector", i = "numeric", j = "numeric"),
          def=function (x, i, j, k, m, ..., drop) {
            if (missing(k)) k = 1:dim(x)[3]
            if (missing(m)) m = 1:dim(x)[4]
			
           
            
            ######
            ## cache grid coordinates?
          })

setMethod(f="takeVolume", signature=signature(x="SparseBrainVector", i="numeric"),
          def=function(x,i) {
            
            if (length(i) > 1) {
              stop("can only take one volume at a time (for now).")
            }

            xs <- space(x)
            bspace <- BrainSpace(dim(x)[1:3], origin=origin(xs), spacing=spacing(xs), orientation(xs), trans(xs))
            vals <- array(0, (dim(x)[1:3]))
            vals[indices(x)] <- x@data[i,]
            bv <- BrainVolume(vals, bspace)
            return(bv)
          })



setMethod(f="writeVector",signature=signature(x="SparseBrainVector", fileName="character"),
          def=function(x, fileName) {
            if (typeof(x) == "double") {
              dataType = "FLOAT"
            } else if (typeof(x) == "integer") {
              dataType = "SHORT"
            } else {
              stop(paste("unrecognized storage stype : ", typeof(x)))
            }

            
            brainFile <- NIFTIFile(fileName, "w")
            hdr <- createNIFTIHeader(x, fileName, dataType)
            
            writeHeader(brainFile, hdr)

            NVOLS <- dim(x)[4]
            
            for (i in 1:NVOLS) {
              vol <- takeVolume(x,i)
              print(paste("writing volume ", i))
              writeData(brainFile, hdr, as.vector(vol))
            }
              
          })

#		setAs(from="BrainVector", to="matrix",
#		      function(from) {
#		        data <- from@.Data
#		        dm <- dim(data)
#		        d123 <- prod(dm[1:3])
#		        d4 <- dm[4]
#
#		        dim(data) <- c(d123,d4)
#		        data
#
#		      })
            

          
          
