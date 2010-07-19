#' @include common.R
roxygen()


.checkDimensions <- function(dimvec) {
	if (any(dimvec < 0)) {
		stop(paste("nifti(checkDimensons): illegal dimension vector in header: ", dimvec))
	}
}

readNIfTIHeader <- function(fname) {
	
	header <- list()
	header$fileType <- "NIfTI"
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
	
	header$fileName <- fname
	header$endian <- endian
	
	readBin(conn, what=integer(), n=10+18+4+2+1, size=1)
	
	header$diminfo <- readBin(conn, what=integer(), n=1, size=1)
	header$dimensions <- readBin(conn, integer(), n=8, size=2, endian=endian)
	
	header$dimensions[header$dimensions == 0] <- 1
	
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
	header$qform <- zapsmall(.quaternToMatrix(header$quaternion, header$qoffset, header$pixdim[2:4], header$qfac))
	
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
	
	header
	
}
