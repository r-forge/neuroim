


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

.BrainVectorFromMatrix <- function(data, space) {
   nvols <- dim(space)[4]
   nelements <-  prod(dim(space)[1:3])

   if ( (dim(data)[1] == nvols) && (dim(data)[2] == nelements) ) {
     #fourth dimension is rows
     new("BrainVector", .Data=t(data), space=space)        
   } else if ((dim(data)[2] == nvols) && (dim(data)[1] == nelements )) {
     #fourth dimension is columns
     new("BrainVector", .Data=data, space=space)
   } else {
     stop(paste("illegal matrix dimension ", dim(data)))
   }
 }
  
  
.createVectorSpaceFromData <- function(data, space, indices=NULL) {

  
  
  if (numdim(space) < 3) {
    stop(paste("incorrect dimensions : ", dim(space)))
  }

  if (numdim(space) == 4) {
    return(space)
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
      
    
  
                                                   
BrainVector <- function(data, space, indices=NULL) {
  
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

loadSeries <- function(filename, indices, volidx=NULL, reduce=T, demean=F, verbose=F, bulk.thresh=100 ) {
  if (!.isNIFTI(filename)) {
    stop("only support NIFTI files at present")
  }

  nfile <- NIFTIFile(filename)
  header <- readHeader(nfile)
  ddim <- dataDim(header)

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

  NT <- ddim[4]

  if (is.null(volidx)) {
    volidx <- 1:NT
  }
  
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
  

  

  
loadVector  <- function(filename, volRange=NULL, mask=NULL) {

  if (!.isNIFTI(filename)) {
    stop("only support NIFTI files at present")
  }

  if (!is.null(mask)) {
    return(.loadSparseVector(filename, mask))
  }

    
  nfile <- NIFTIFile(filename)
  header <- readHeader(nfile)
  ddim <- dataDim(header)

  if (length(ddim) != 4) {
    stop("Error: file does not have 4 dimensions, which is required for a BrainVector object")
  }
  
  if (is.null(volRange)) {
    volRange <- 1:ddim[4]
  }

    
  data <- readData(nfile)
  data <- array(data, ddim)
  space <- createSpace(header)
  new("BrainVector", .Data=data, space=space)
  
}

setMethod("sliceMeans", signature(x="BrainVector"),
          function(x) {
             t(colMeans(x, dims=2))
           })

setMethod("takeVolume", signature(x="BrainVector", i="numeric"),
  function(x,i) {
    
    xs <- space(x)
    makevol <- function(i) {
      xs <- space(x)
      bspace <- BrainSpace(dim(x)[1:3], origin=origin(xs), spacing=spacing(xs), orientation(xs), trans(xs))
      bv <- BrainVolume(x@.Data[,,,i], bspace)
    }

    if (length(i) > 1) {
      lapply(i, makevol)
    } else {
      makevol(i)
    }
  })

setMethod("concat", signature(x="BrainVector", y="BrainVolume"),
          function(x,y) {
            if (!all(dim(x)[1:3] == dim(y)[1:3])) {
              stop("cannot concatenate arguments with different spatial dimensions")
            }
            
            ndat <- abind(x@.Data, y@.Data, along=4)
            d1 <- dim(x)
            d2 <- dim(y)
            ndim <- c(d1[1:3], dim(x)[4] + 1)
            nspace <- BrainSpace(ndim, origin(x@space), spacing(x@space),
                                 orientation(x@space), trans(x@space), reptime=1)
            BrainVector(ndat, nspace)
          })
            
setMethod("concat", signature(x="BrainVolume", y="BrainVolume"),
          function(x,y,...) {
            if (!all(dim(x)[1:3] == dim(y)[1:3])) {
              stop("cannot concatenate arguments with different spatial dimensions")
            }

            

            ndat <- abind(x@.Data, y@.Data, along=4)
            d1 <- dim(x)
            d2 <- dim(y)

            ndim <- c(d1[1:3], 2)
            nspace <- BrainSpace(ndim, origin(x@space), spacing(x@space),
                                 orientation(x@space), trans(x@space), reptime=1)
            ret <- BrainVector(ndat, nspace)
            rest <- list(...)

            if (length(rest) >= 1) {
              for (i in 1:length(rest)) {
                ret <- concat(ret, rest[[i]])
              }
            }

            ret
            
          })

 ### lot of code duplication ..           
 setMethod("concat", signature(x="BrainVector", y="BrainVector"),
          function(x,y,...) {

            if (!all(dim(x)[1:3] == dim(y)[1:3])) {
              stop("cannot concatenate arguments with different spatial dimensions")
            }

            ndat <- abind(x@.Data, y@.Data)
            d1 <- dim(x)
            d2 <- dim(y)

            ndim <- c(d1[1:3], d1[4] + d2[4])
            nspace <- BrainSpace(ndim, origin(x@space), spacing(x@space),
                                 orientation(x@space), trans(x@space), reptime=1)


            ret <- BrainVector(ndat, nspace)
            rest <- list(...)
            
            if (length(rest) >= 1) {
              for (i in 1:length(rest)) {
                ret <- concat(ret, rest[[i]])
              }
            }
            

            ret

          })



setMethod("eachSeries", signature(x="BrainVector", FUN="function"),
          function(x, FUN, withIndex=FALSE) {
            
            NX <- dim(x)[1]
            NY <- dim(x)[2]
            NZ <- dim(x)[3]
            ret <- vector("list", prod(NX, NY, NZ))
            #browser()
            if (withIndex) {
              index = 1
              for (i in 1:NZ) {
                for (j in 1:NY) {
                  for (k in 1:NX) {
                    ret[[index]] <- FUN(x[k,j,i,], index)
                    index <- index+1
                  }
                }
              }
            } else {
              for (i in 1:NZ) {
                for (j in 1:NY) {
                  for (k in 1:NX) {
                    ret[[index]] <- FUN(x[k,j,i,])
                    index <- index+1
                  }
                }
              }
            }

            ret
           
          })

setMethod("seriesIter", signature(x="BrainVector"), 
	function(x) {
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
	


setMethod("pick", signature(x="BrainVector", mask="vector"),
          function(x, mask, reduce=F, FUN=mean) {
            if ( (length(dim(mask)) == 3) & (length(mask) == prod(dim(x)[1:3])) ) {
              ## 3d mask
              #mask <- as.logical(mask)
              NVOL <- dim(x)[4]
              
              args <- lapply(1:NVOL, function(i) mask) 
              imask <- do.call("abind", c(args, along=4))
              
              mat <- x[imask]
              dim(mat) <- c(sum(mask), NVOL)

              #mat <- matrix(0, c(sum(mask), NVOL))
              #for (i in 1:NVOL) {
              #  vol <- x[,,,i]
              #  mat[i,] <- vol[mask]
              #}

              if (reduce) {
                apply(mat, 2, FUN)
              } else {
                mat
              }
            }
          })
              
              
            

setMethod("writeVector",signature(x="BrainVector", fileName="character"),
          function(x, fileName) {
            

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
            writeData(brainFile, hdr, as.vector(x))
            
          })
            
            
setAs(from="BrainVector", to="matrix",
      function(from) {
        data <- from@.Data
        dm <- dim(data)
        d123 <- prod(dm[1:3])
        d4 <- dm[4]
        
        dim(data) <- c(d123,d4)
        data
          
      })


