

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

  if (length(ddim) < 3) {
    stop("Error: file has less than 3 dimensions, not a volumetric data set")
  }

  
  nfile <- NIFTIFile(file)
  header <- readHeader(nfile)
  ddim <- dataDim(header)

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
            if (length(idx) == 1) {
              .indexToGrid(idx, array.dim)
            } else {
              t(sapply(idx, .indexToGrid, array.dim))
            }
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
    
write.nifti.volume <- function(vol, fileName) {
	brainFile <- NIFTIFile(fileName, "w")

    dataType <- if (typeof(x) == "double") {
      "FLOAT"
    } else if (typeof(x) == "integer") {
      "SHORT"
    } else {
      stop(paste("unrecognized storage stype : ", typeof(x)))
    }
   
    hdr <- createNIFTIHeader(x, fileName, dataType)
    writeHeader(brainFile, hdr)
    writeData(brainFile, hdr, as.vector(x))
	
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



            
            
