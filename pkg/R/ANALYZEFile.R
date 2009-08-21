ANALYZEFile <- function(path, open="r", type=NULL) {
  if ( !(any(c("r", "w") == open)) ) {
    stop("open argument must either be \"r\" or \"w\"")
  }

  if (is.null(type)) {
    type <- "analyze-standard"
  }
  
  if ( !(any(c("analyze-standard", "analyze-gz") == type) ) ) {
    stop("type argument must be one of \"analyze-standard\" or \"analyze-gz\"")
  }

  return(new("ANALYZEFile", path=path, open=open, fileType=type))
}

setMethod("headerFileExt", signature(x= "NIFTIFile"),
          function(x) {
            if (x@fileType == "analyze75") {
              return("hdr")
            } else if (x@fileType == "analyze75-gz") {
              return("hdr")
            } else {
              stop(paste("invalid file type: ", x@fileType))
            }
          })


setMethod("dataFileExt", signature(x="ANALYZEFile"),
          function(x) {
            if (fileType(x) == "analyze75") {
              return("img")
            } else if (fileType(x) == "analyze75-gz") {
              return("img.gz")
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
		.writeANALYZEData(conn, data, header)
	    close(conn)
})

readANALYZEHeader <- function(headerName) {	

  hfile <- file(headerName, open="rb")
  endian <- "little"

  size.of.hdr <- readBin(hfile, integer(), 1, endian=endian)
    
  if (size.of.hdr != 348) {
    endian <- "big"
    seek(hfile,0)
    size.of.hdr <- readBin(hfile, integer(), 1, endian=endian)
    print(size.of.hdr)
    if (size.of.hdr != 348) {
      stop("invalid header size: must equal 348")
    }
  }
   
  info <- list()
  info$filetype <- "ANALYZE7.5"
  info$encoding <- "binary"
  info$headerFile <- headerName
  info$dataFile <- paste(substr(headerName, 1, nchar(headerName)-4), ".img", sep="")
  info$endian <- endian

  tmp <- readBin(hfile, integer(), n=10, size=1, endian=endian)
  seek(hfile, 18, origin="current") # db_name
  seek(hfile, 4, origin="current")  # extents
  seek(hfile, 2, origin="current")  # session_error
  seek(hfile, 1, origin="current")  # regular
  seek(hfile, 1, origin="current")  # hkey_un0

  
  dimensions <- readBin(hfile, integer(), n=8, size=2, endian=endian)
  info$dimensions <- dimensions[2:5]
  info$numDimensions <- sum(dimensions[2:length(dimensions)] > 1)
  
  
  seek(hfile, 12, origin="current") # 4 vox_units, 8 cal_units
  seek(hfile, 2, origin="current")  # unused_short

  info$dataCode <- readBin(hfile, integer(), n=1, size=2, endian=endian)

  info$dataStorage <- 
  	if (info$dataCode == 1) { 
		"BOOLEAN" 
  	} else if (info$dataCode == 2) { 
		"BYTE" 
  	} else if (info$dataCode == 4) { 
		"SHORT" 
  	} else if (info$dataCode == 8) { 	
		"INTEGER" 
  	} else if (info$dataCode == 16) { 
		"FLOAT" 
  	} else if (info$dataCode == 32) { 
		"DOUBLE" 
  	} else { 
		stop(paste("unsupported data type code: ", info$dataCode)) 
  	}

  #info$dataType <- data.type

  info$bits.per.pixel <- readBin(hfile, integer(), n=1, size=2, endian=endian) 
  seek(hfile, 2, origin="current")  # unused_short

  info$pixdim <- abs(readBin(hfile, double(), n=8, size=4, endian=endian)[2:4])
  info$voxOffset <- readBin(hfile, double(), n=1, size=4, endian=endian)
  info$scaleFactor <- readBin(hfile, double(), n=1, size=4, endian=endian)
  
  info$max=0
  info$min=0

  
  close(hfile)

  ANALYZEHeader(headerFile=info$headerFile, dataFile=info$dataFile,
    extraInfo=info, encoding=info$encoding,
    endian=info$endian, dataOffset=info$voxOffset,
    dataType=info$dataStorage)

}

.writeANALYZEData <- function(conn, data, hdr) {
 
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


.writeANALYZEHeader <- function(analyzeInfo, conn=NULL, endian=NULL, headerFile=NULL) {

  conn <- .createWritableConnection(conn, headerFile)

  if (is.null(endian)) {
    endian <- analyzeInfo$endian
  }

  writeBin(as.integer(348), conn, 4, endian)

  writeBin(character(34),conn,1,endian)
  writeBin("r", conn,1,endian)
  writeBin(character(), conn, 1, endian)

  if (analyzeInfo$dimensions[4] > 1)
    writeBin(as.integer(4), conn, 2, endian)
  else
    writeBin(as.integer(3), conn, 2, endian)

  writeBin(as.integer(analyzeInfo$dimensions[1]), conn, 2, endian)
  writeBin(as.integer(analyzeInfo$dimensions[2]), conn, 2, endian)
  writeBin(as.integer(analyzeInfo$dimensions[3]), conn, 2, endian)
  writeBin(as.integer(analyzeInfo$dimensions[4]), conn, 2, endian)


  writeBin(as.integer(1), conn, 2, endian)
  writeBin(as.integer(1), conn, 2, endian)
  writeBin(as.integer(1), conn, 2, endian)

  writeBin(character(14), conn,1,endian)

  writeBin(as.integer(analyzeInfo$type.code), conn,2,endian)
  writeBin(as.integer(analyzeInfo$bits.per.pixel), conn,2,endian)


  writeBin(as.integer(0), conn, 2, endian=endian)
  writeBin(as.double(0), conn, 4, endian)
  writeBin(as.double(analyzeInfo$pixel.dimensions[1]), conn,4,endian)
  writeBin(as.double(analyzeInfo$pixel.dimensions[2]), conn,4,endian)
  writeBin(as.double(analyzeInfo$pixel.dimensions[3]), conn,4,endian)

  seek(conn, 140)
  writeBin(as.integer(analyzeInfo$max), conn, 4, endian)
  writeBin(as.integer(analyzeInfo$min), conn, 4, endian)

  curloc = seek(conn)
  remaining = 348-curloc

  writeBin(character(remaining), conn, 1, endian)
  close(analyzeInfo)

}



createAnalyzeInfo <- function(fname, dataType=NULL, header=NULL, brainData=NULL, encoding="binary", endian=.Platform$endian) {
	if (is.null(header) && is.null(brainData)) {
	    stop("header and brainData arguments cannot both be null.")
	} else if (!is.null(header) && !is.null(brainData)) {
		stop("must provide either BrainHeader or BrainData but not both")
	}
	
	if(!.isExtension(fname,".hdr")) {
		fname <- paste(fname, ".hdr", sep="")
	}
	
	info <- list()
	
	
	if (!is.null(header)) {
	    if (is.null(dataType)) {
		  dataType <- dataType(header)
	      info$datatype <- .getDataCode(dataType(header))
	    } else {
		  info$datatype <- .getDataCode(dataType)		
		}
	
	    info$dataStorage <- .getDataStorage(info$datatype)
	    info$bitpix <- .getDataSize(dataType(header))*8
	  
	  } else {
		if (is.null(dataType)) {
	      dataType <- "FLOAT"		
	      info$datatype <- .getDataCode(dataType)
	    } else {
		  info$datatype <- .getDataCode(dataType)		
		}
	
	    info$dataStorage <- .getDataStorage(info$datatype)
	    info$bitpix <- .getDataSize(info$dataStorage)*8
	}
	
	brainSpace <- if (is.null(brainData)) {
		createSpace(header)
	 } else {
		space(brainData)
	}
	
	browser()
	
	ndim <- numdim(brainSpace)
	dims <- dim(brainSpace)
	info$version <- "7.5"
	info$headerFile <- fname
	info$dataFile <- sub("\\.hdr$", ".img", fname)
	info$endian <- endian
	info$encoding <- encoding
	info$diminfo <- 0
    odims <- rep(1,8)
	odims[2:(length(dims)+1)] = dims
    odims[1] <- ndim
	info$dimensions <- odims
	info$numDimensions <- ndim
  
	info$pixdim <- c(1, spacing(brainSpace))
	info$pixdim = c(info$pixdim, rep(1, (8-length(info$pixdim))))

	
    ######################################
	## assumes 4th dimension is time ####
	info$pixdim[5] <- reptime(brainSpace)
	#####################################   
	
	info
	 
}

  


          
