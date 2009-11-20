
NIFTIFile <- function(path, open="r", type=NULL) {
  if ( !(any(c("r", "w") == open)) ) {
    stop("open argument must either be \"r\" or \"w\"")
  }

  if (is.null(type) && .isExtension(path, ".nii")) {
    type <- "nifti-single"
  } else if (is.null(type) && .isExtension(path, ".nii.gz")) {
    type <- "nifti-gz"
  } else {
    path <- paste(path, ".nii", sep="")
    type <- "nifti-single"
  }
  
  if ( !(any(c("nifti-single", "nifti-pair", "nifti-gz") == type) ) ) {
    stop("type argument must be one of \"nifti-single\" or \"nifti-pair\" or \"nifti-gz\"")
  }

  return(new("NIFTIFile", path=path, open=open, fileType=type))
}


setMethod("show", "NIFTIFile",
    function(object) {
        cat("NIFTI file\n")
        cat(" Path            :", object@path, "\n")
        cat(" Type           :", object@fileType, "\n")
        cat(" Open           :", object@open, "\n")     
    }
)




.openRead <- function(x) {

  filedir <- dirname(path(x))
  dataName <- paste(filedir, "/", fileName(x), ".", dataFileExt(x), sep="")
  
  if (fileType(x) == "nifti-gz") {
    return(gzfile(dataName, open="rb"))
  } else {
    return(file(dataName, open="rb"))
  }
}

setMethod("headerFileExt", signature(x= "NIFTIFile"),
          function(x) {
            if (x@fileType == "nifti-single") {
              return("nii")
            } else if (x@fileType == "nifti-pair") {
              return("hdr")
            } else if (x@fileType == "nifti-gz") {
              return("nii.gz")
            } else {
              stop(paste("invalid file type: ", x@fileType))
            }
          })

setMethod("dataFileExt", signature(x= "NIFTIFile"),
          function(x) {
            if (fileType(x) == "nifti-single") {
              return("nii")
            } else if (fileType(x) == "nifti-pair") {
              return("img")
            } else if (fileType(x) == "nifti-gz") {
              return("nii.gz")
            } else {
              stop(paste("invalid file type: ", fileType(x)))
            }
          })
          



setMethod("writeData", signature(x="NIFTIFile", header="NIFTIHeader", data="vector"),
          function(x, header, data) {
            
            if (!openFor(x) == "w") {
              stop("this file is not open for writing")
            }
            
            filedir <- dirname(path(x))
            dataName <- paste(filedir, "/", fileName(x), ".", dataFileExt(x), sep="")
            conn <- file(dataName, open="ab")
            gc()
            .writeNIFTIData(conn, data, header)
            close(conn)
          })
            
            
setMethod("writeHeader", signature(x="NIFTIFile", header="BrainHeader"),
          function(x, header) {
            filedir <- dirname(path(x))
            headerName <- paste(filedir, "/", fileName(x), ".", headerFileExt(x), sep="")
            conn <- file(headerName, open="wb")

            if (class(header) == "NIFTIHeader") {
              info <- extraInfo(header)
              ## might have to change file name here
            } else {
              info <- createNIFTIInfo(headerName, dataType(header), header, brainData=NULL, encoding="binary", endian=.Platform$endian)
            }            
            .writeNIFTIHeader(info, conn, headerFile=headerName)
          })
          

setMethod("readData", signature(x="NIFTIFile", indices="missing"), 
  function(x) {
    header <- readHeader(x)

    filedir <- dirname(path(x))   
    dataName <- paste(filedir, "/", fileName(x), ".", dataFileExt(x), sep="")

    conn <- .openRead(x)
    
    dsize <- .getDataSize(dataType(header))
    rstorage <- .getRStorage(dataType(header))

    

    offset <- dataOffset(header)
    fsize <- file.info(dataName)$size
    seek(conn, where=offset, origin="start")

    numElements <- prod(dataDim(header)) 
    data <- readBin(conn, what=rstorage, n=numElements,
                  size=dsize, endian=endian(header))

    sfactor <- extraInfo(header)$sclSlope
    
    if (sfactor > 0) {
      data <- data * sfactor
    }
    
    close(conn)
    
    return(data)
  })

setMethod("readData", signature(x="NIFTIFile", indices="index"), 
  function(x, indices) {
    header <- readHeader(x)
    filedir <- dirname(path(x))   
    dataName <- paste(filedir, "/", fileName(x), ".", dataFileExt(x), sep="")

    conn <- .openRead(x)
    
    dsize <- .getDataSize(dataType(header))
    rstorage <- .getRStorage(dataType(header))
    offset <- dataOffset(header)

    ddim <- dataDim(header)
    NVOLS <- ifelse(length(ddim) == 4, ddim[4], 1)

    mat <- matrix(0, length(indices), NVOLS)
    numElements <- prod(ddim[1:3])
    seek(conn, where=offset, origin="start")
    
    for (i in 1:NVOLS) {
      
      data <- readBin(conn, what=rstorage, n=numElements,
                  size=dsize, endian=endian(header))
      mat[,i] <- data[indices]
    }

    sfactor <- extraInfo(header)$sclSlope
    
    if (sfactor > 0) {
      mat <- mat * sfactor
    }

    close(conn)
    return(mat)
  })


setMethod("readHeader", signature(x="NIFTIFile"),
          function(x) {
	
            if (openFor(x) != "r") {
              stop("NIFTIFile is open for writing, not reading")
            }

            fname <- fileName(x)
            filedir <- dirname(path(x))

            
            ftype <- fileType(x)

            ext <- .niftiExt(ftype)

            headerName <- paste(paste(filedir, .Platform$file.sep, sep=""),
                                paste(fname, ext$header, sep="."), sep="")

            header <- readNIFTIHeader(headerName)

            
          }
        )


.checkDimensions <- function(dimvec) {
  if (any(dimvec < 0)) {
    stop(paste("nifti(checkDimensons): illegal dimension vector in header: ", dimvec))
  }
}



.checkConnection <- function(conn, headerFile, niftiInfo) {

  if (is.null(conn)) {
    if (!is.null(headerFile)) {  #check if name already exists
      if (file.exists(headerFile)) {
        stop(paste("file ", headerFile, " already exists. aborting"))
      } else {
        conn <- file(headerFile, open="wb") # open file connection
      }
    } else {
        headerFile <- niftiInfo$headerFile  # take header name from info list
        if (file.exists(headerFile)) {      
          stop(paste("file ", headerFile, " already exists. aborting"))
        } 
        conn <- file(headerFile, open="wb")
      }
    
  }
  return(conn)
}

.writeNIFTIData <- function(conn, data, hdr) {
 
  info <- extraInfo(hdr)
  
  offset <- dataOffset(hdr)
  
  seek(conn, offset)
  if (info$dataStorage == "BINARY") {
    writeBin(as.logical(data), conn, 1, endian=info$endian)
  } else if (info$dataStorage == "UBYTE") {
    writeBin(as.integer(data), conn, 1, endian=info$endian)
  } else if (info$dataStorage == "SHORT") {
    writeBin(as.integer(data), conn, 2, endian=info$endian)
  } else if (info$dataStorage == "INT") {
    writeBin(as.integer(data), conn, 4, endian=info$endian)
  } else if (info$dataStorage == "FLOAT") {
    writeBin(as.double(data), conn, 4, endian=info$endian)
  } else if (info$dataStorage == "DOUBLE") {
    writeBin(as.double(data), conn, 8, endian=info$endian)
  } else {
    stop(paste("unsupported output data type: ", info$dataStorage))
  }
  
}


            
.writeNIFTIHeader <- function(niftiInfo, conn=NULL, endian=NULL, headerFile=NULL) {

  conn <- .checkConnection(conn, headerFile, niftiInfo)

  if (is.null(endian)) {
    endian <- niftiInfo$endian
  }
  

  writeBin(as.integer(348), conn, 4, endian) 
  writeBin(integer(34),conn,1,endian)
  writeChar("r", conn,1,eos=NULL)
  writeBin(as.integer(niftiInfo$diminfo), conn, size=1, endian) #diminfo, not supported currently -- write zero  
  writeBin(as.integer(niftiInfo$ndim), conn, 2, endian)         #num dimensions 
  writeBin(as.integer(niftiInfo$dimensions), conn, 2, endian)   #dimension vector 
  writeBin(as.double(niftiInfo$intent1), conn, 4, endian)       #intent1
  writeBin(as.double(niftiInfo$intent2), conn, 4, endian)       #intent2
  writeBin(as.double(niftiInfo$intent3), conn, 4, endian)       #intent3
  writeBin(as.integer(niftiInfo$intentCode), conn, 2, endian)   #intent code
  writeBin(as.integer(niftiInfo$datatype),conn, 2, endian)      #datatype 
  writeBin(as.integer(niftiInfo$bitpix),conn, 2, endian)        #bits per pixel 
  writeBin(as.integer(niftiInfo$sliceStart),conn, 2, endian)    #slice start
  writeBin(as.double(niftiInfo$pixdim), conn, 4, endian)        #pix dim
  writeBin(as.double(niftiInfo$voxOffset), conn, 4, endian)     #voxel offset
  writeBin(as.double(niftiInfo$sclSlope), conn, 4, endian)      #slope
  writeBin(as.double(niftiInfo$sclIntercept), conn, 4, endian)  #intercept
  writeBin(as.integer(niftiInfo$sliceEnd), conn, 2, endian)     #slice end
  writeBin(as.integer(niftiInfo$sliceCode), conn, 1, endian)    #slice code
  writeBin(as.integer(niftiInfo$xyztUnits), conn, 1, endian)    #xyzt units
  writeBin(as.double(niftiInfo$calMax), conn, 4, endian)        #cal max
  writeBin(as.double(niftiInfo$calMin), conn, 4, endian)        #cal min
  writeBin(as.double(niftiInfo$sliceDuration), conn, 4, endian) #slice duration
  writeBin(as.double(niftiInfo$toffset), conn, 4, endian)       #t offset
  writeBin(as.integer(niftiInfo$glmax), conn, 4, endian)        #glmax
  writeBin(as.integer(niftiInfo$glmin), conn, 4, endian)        #glmin
  writeBin(as.integer(niftiInfo$description), conn, 1, endian)  #description
  writeBin(as.integer(niftiInfo$auxfile), conn, 1, endian)      #aux_file
  writeBin(as.integer(niftiInfo$qformCode), conn, 2, endian)    #qform code
  writeBin(as.integer(niftiInfo$sformCode), conn, 2, endian)    #sform code
  writeBin(as.double(niftiInfo$quaternion), conn, 4, endian)    #quaternion
  writeBin(as.double(niftiInfo$qoffset), conn, 4, endian)       #qoffset
  writeBin(as.double(t(niftiInfo$sform[1:3,])), conn, 4, endian) #sform
  writeBin(as.integer(niftiInfo$intentName), conn, 1, endian)    #intentName
  writeChar(niftiInfo$magic, conn)                               #magic

  loc <- seek(conn)
  offset <- niftiInfo$voxOffset

  nbytes <- offset-loc

  if (nbytes > 0) {
    writeBin(integer(nbytes), conn, size=1, endian)
  }
  
  close(conn)
}

createNIFTIInfo <- function(fname,  dataType="FLOAT", header=NULL, brainData=NULL, encoding="binary", endian=.Platform$endian) {

  if (is.null(header) && is.null(brainData)) {
    stop("header and brainData arguments cannot both be null.")
  }

  if(!.isExtension(fname,".nii")) {
    fname = paste(fname, ".nii", sep="")
  }

 
  info <- list()

  if (!is.null(header)) {
    if (missing(dataType)) {
      info$datatype <- .getDataCode(dataType(header))
    }
    info$dataStorage <- .getDataStorage(info$datatype)
    info$bitpix <- .getDataSize(dataType(header))*8
    info$quaternion <- c(0,0,0)

    ### dicey indeed
    info$qfac <- 1
    ### 
  } else {
    if (missing(dataType)) {
      dataType = "FLOAT"
    } 
    info$datatype <- .getDataCode(dataType)
    info$dataStorage <- .getDataStorage(info$datatype)
    info$bitpix <- .getDataSize(info$datatype)*8
    res <- .matrixToQuatern(trans(space(brainData)))
    info$quaternion <- res$quaternion
    info$qfac <- res$qfac
    
    #info$qform <- .quaternToMatrix(info$quaternion, header$qoffset, header$pixdim[2:4], header$qfac)
  }

    
    
  if (is.null(brainData)) {
    brainSpace <- createSpace(header)
  } else {
    brainSpace <- space(brainData)
  }
      
  ndim <- numdim(brainSpace)
  dims <- dim(brainSpace)
      
  
  info$version <- "1"
  info$headerFile <- fname
  info$dataFile <- fname
  info$endian <- endian
  info$encoding <- encoding
  
  info$diminfo <- 0

  odims <- rep(1,8)
  odims[2:(length(dims)+1)] = dims
  
  odims[1] <- ndim
  info$dimensions <- odims
  info$numDimensions <- ndim

  info$intent1 <- 0
  info$intent2 <- 0
  info$intent3 <- 0

  info$intentCode <- 0
  
  
  info$sliceStart <- 0
  info$pixdim <- c(1, spacing(brainSpace))
  info$pixdim = c(info$pixdim, rep(1, (8-length(info$pixdim))))
  info$pixdim[1] <- info$qfac

  ######################################
  ## assumes 4th dimension is time ####
  info$pixdim[5] <- reptime(brainSpace)
  ######################################
  
  #info$qfac <- 1
  info$voxOffset <- 352 # assumes single .nii file
  info$sclSlope <- 0
  info$sclIntercept <- 0
  info$sliceEnd <- dims[3]-1
  info$sliceCode <- 0
  info$xyztUnits <- 2  # assumes MM units
  info$calMax <- 0
  info$calMin <- 0
  info$sliceDuration <- 0
  info$toffset <- 0
  info$glmax <- 0
  info$glmin <- 0
  info$description <- integer(80)
  info$auxfile <- integer(24)

  info$qformCode <- 1 # scanner-based anatomy as default
  info$sformCode <- 1
  #info$quaternion <- 
  info$qoffset <- origin(brainSpace)
  
  info$sform <- .quaternToMatrix(info$quaternion, info$qoffset, info$pixdim[2:4], info$qfac)
  info$sform <- info$sform[1:3,] # trim last row

  info$qform <- .quaternToMatrix(info$quaternion, info$qoffset, info$pixdim[2:4], info$qfac)
  info$qform <- info$qform[1:3,] # trim last row

  
  info$intentName <- integer(16)

  info$onefile <- T
  info$version <- "1.1"
  info$magic <- "n+1"
  
  return(info)
}
         

readNIFTIHeader <- function(fname) {
  if (! ((.isExtension(fname, ".nii") || .isExtension(fname, ".hdr") || .isExtension(fname, ".nii.gz") )) ) {
    stop(paste("readNIFTIHeader: supplied filename:", fname, "is not an nifti header"))
  }

  header <- list()

  if (.isExtension(fname, ".hdr")) {
    header$headerFile <- fname
    header$dataFile <- paste(substr(fname, 1, length(fname)-4), ".img", sep="")
  } else {
     header$headerFile <- fname
     header$dataFile <- fname
   }
   

  header$fileType <- "NIFTI"
  header$encoding <- "binary"
  header$version <- "1"
  
  conn <- NULL

  if (.isExtension(fname, ".nii") || .isExtension(fname, ".hdr")) {
    conn <- file(fname, open="rb")
  } else if (.isExtension(fname, ".nii.gz")) {
    conn <- gzfile(fname, open="rb")
    header$encoding <- "gzip"
  } else {
    stop(paste("illegal NIFTI header name", fname))
  }


  endian <- .getEndian(conn)
  header$endian <- endian

  readBin(conn, what=integer(), n=10+18+4+2+1, size=1)

  header$diminfo <- readBin(conn, what=integer(), n=1, size=1)
  header$dimensions <- readBin(conn, integer(), n=8, size=2, endian=endian)

  header$dimensions[header$dimensions == 0] <- 1
  #which.zero <- min(which(header$dimensions == 0))
  #header$dimensions <- header$dimensions[2:(which.zero-1)]
  
  .checkDimensions(header$dimensions)
  header$numDimensions <- header$dimensions[1]

  header$intent1 <-  readBin(conn, double(), n=1, size=4, endian=endian)
  header$intent2 <-  readBin(conn, double(), n=1, size=4, endian=endian)
  header$intent3 <-  readBin(conn, double(), n=1, size=4, endian=endian)

 
  header$intentCode <-  readBin(conn, integer(), n=1, size=2, endian=endian)
  header$datatype <- readBin(conn, integer(), n=1, size=2, endian=endian)
  header$dataStorage <- .getDataStorage(header$datatype)
  header$bitpix <- readBin(conn, integer(), n=1, size=2, endian=endian)
  header$sliceStart <- readBin(conn, integer(), n=1, size=2, endian=endian)
  header$pixdim <-  readBin(conn, double(), n=8, size=4, endian=endian)

  ### qfac
  header$qfac = header$pixdim[1]

  if (header$qfac == 0) {
    header$qfac = 1
  }

  header$voxOffset <- readBin(conn, double(), n=1, size=4, endian=endian)
  header$sclSlope <- readBin(conn, double(), n=1, size=4, endian=endian)
  header$sclIntercept <- readBin(conn, double(), n=1, size=4, endian=endian)
  header$sliceEnd <- readBin(conn, integer(), n=1, size=2, endian=endian)
  header$sliceCode <-  readBin(conn, integer(), n=1, size=1, endian=endian)
  

  header$xyztUnits <- readBin(conn, integer(), n=1, size=1, endian=endian)
  header$calMax <- readBin(conn, double(), n=1, size=4, endian=endian)
  header$calMin <- readBin(conn, double(), n=1, size=4, endian=endian)

  header$sliceDuration <- readBin(conn, double(), n=1, size=4, endian=endian)
  header$toffset <- readBin(conn, double(), n=1, size=4, endian=endian)

  header$glmax <- readBin(conn, integer(), n=1, size=4, endian=endian) # unused glmax, glmin
  header$glmin <- readBin(conn, integer(), n=1, size=4, endian=endian) # unused glmax, glmin
  
  header$description <- readBin(conn, integer(), n=80, size=1, endian=endian)
  header$auxfile <- readBin(conn, integer(), n=24, size=1, endian=endian)

  header$qformCode <- readBin(conn, integer(), n=1, size=2, endian=endian)
  header$sformCode <- readBin(conn, integer(), n=1, size=2, endian=endian)
  header$quaternion <- readBin(conn, double(), n=3, size=4, endian=endian)
  
  header$qoffset <- readBin(conn, double(), n=3, size=4, endian=endian)
  header$qform <- .quaternToMatrix(header$quaternion, header$qoffset, header$pixdim[2:4], header$qfac)

  sform  <- readBin(conn, double(), n=12, size=4, endian=endian)
  header$sform <- rbind(matrix(sform,3,4, byrow=T), c(0,0,0,1))
  header$intentName <- readBin(conn, character(), n=16, size=1, endian=endian)
  header$magic <- readChar(conn, nchars=4)

  header$onefile <- F
  if (substr(header$magic,2,2) == "+") {
    header$onefile <- T
  }

  header$version <- substr(header$magic,3,3)
  
  close(conn)
 
  hdr = NIFTIHeader(headerFile=header$headerFile, dataFile=header$dataFile,
    extraInfo=header, fileType=header$fileType, versionNum=header$version, encoding=header$encoding,
    endian=header$endian, dataOffset=header$voxOffset,
    dataType=header$dataStorage)
  
  return(hdr)
    
}


    
