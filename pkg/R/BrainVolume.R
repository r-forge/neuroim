

BrainVolume <- function(data, space, indices=NULL) {
  if (numdim(space) != 3) {
    stop("supplied data argument has incorrect dimensions for BrainVolume: ")
  }

  if (is.null(indices)) {
    if (length(dim(data)) != 3) {
      data <- array(data, c(dim(space)[1], dim(space)[2], dim(space)[3]))
    }
    return(new("BrainVolume", .Data=data, space=space))
  } else {
    mdat <- array(0, dim(space))
    mdat[indices] <- data
    new("BrainVolume", .Data=mdat, space=space)
  }
}

loadVolume  <- function(file, volNum=1) {

  if (!.isNIFTI(file)) {
    stop("only support NIFTI files at present")
  }

  
  nfile <- NIFTIFile(file)
  header <- readHeader(nfile)
  ddim <- dataDim(header)

  if (length(ddim) < 3) {
    stop("Error: file has less than 3 dimensions, not a volumetric data set")
  }

  if (volNum > 1) {
	stop("offset volume reading is not supported yet.")
	
    if (length(ddim) < 4) {
      stop(paste("Error: file has less than 4 dimensions, cannot read a volume number ", volNum))
    }

    
  }

  
 
  data <- readData(nfile)
  data <- array(data, ddim[1:3])
  space <- createSpace(header)
  new("BrainVolume", .Data=data, space=space)
 
}


.gridToIndex <- function(dimensions, vmat) {
  slicedim = dimensions[1]*dimensions[2]
  idx <- apply(vmat, 1, function(vox) {
    (slicedim*(vox[3]-1)) + (vox[2]-1)*dimensions[1] + vox[1]   
  })

  return(idx)
}

.indexToGrid <- function(idx, array.dim) {
  rank = length(array.dim)
  wh1 = idx-1
  wh = 1 + wh1 %% array.dim[1]
  wh = rep(wh, rank)
  if (rank >=2) {
    denom = 1
    for (i in 2:rank) {
      denom = denom * array.dim[i-1]
      nextd1 = wh1%/%denom
      wh[i] = 1 + nextd1%%array.dim[i]
    }
  }
  wh
  
}

setMethod("indexToGrid", signature(x="BrainSpace", idx="index"),
          function(x, idx) {
            array.dim <- dim(x)          
            t(sapply(idx, .indexToGrid, array.dim))            
          })

setMethod("indexToGrid", signature(x="BrainVector", idx="index"),
	function(x, idx) {
		callGeneric(space(x), idx)
	})

setMethod("indexToGrid", signature(x="BrainVolume", idx="index"),
          function(x, idx) {
            callGeneric(space(x), idx)
          })


setMethod("gridToIndex", signature(x="BrainVolume", coords="matrix"),
          function(x, coords) {
            array.dim <- dim(x)
            .gridToIndex(dim(x), coords)
          })



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


setMethod("connComp", signature(x="BrainVolume"), 
	function(x, threshold=0, coords=TRUE, clusterTable=TRUE, localMaxima=TRUE, localMaximaDistance=15) {
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
    
write.nifti.volume <- function(vol, fileName) {
	brainFile <- NIFTIFile(fileName, "w")

    dataType <- if (typeof(vol) == "double") {
      "FLOAT"
    } else if (typeof(vol) == "integer") {
      "SHORT"
    } else {
      stop(paste("unrecognized storage stype : ", typeof(vol)))
    }
   
    hdr <- createNIFTIHeader(vol, fileName, dataType)
    writeHeader(brainFile, hdr)
    writeData(brainFile, hdr, as.vector(vol))
	
}  


setMethod("writeVolume",signature(x="BrainVolume", fileName="character", format="missing"),
	function(x, fileName) {
		write.nifti.volume(x, fileName)           
    })

setMethod("writeVolume",signature(x="BrainVolume", fileName="character", format="character"),
	function(x, fileName, format) {
		type <- pmatch(toupper(format), c(toupper("analyze7.5"), toupper("nifti1")))
		if (is.na(type)) {
			stop(paste("unrecognized output format", format))
		}        
		if (type == 1) {
			stop(paste("unsupported format: ", type))
		} else if (type == 2) {
			write.nifti.volume(x, fileName)
		} else {
			stop(paste("unsupported format: ", type))
		}
	})



            
            
