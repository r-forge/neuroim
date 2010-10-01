

#' @include AllClass.R
roxygen()
#' @include AllGeneric.R
roxygen()
#' @include BrainFileDescriptor.R
roxygen()
#' @include Axis.R
roxygen()
#' @include NIFTI_IO.R
roxygen()

#' Generic function to create data reader
#' @param x an object specifying the infromation required to produce the reader
#' @param offset the byte offset (number of bytes to skip before reading)
#' @export dataReader
setGeneric("dataReader", function(x, offset) standardGeneric("dataReader"))


setMethod(f="dim", signature=signature("FileMetaInfo"), 
		def=function(x) {
			x@Dim
		})

setMethod(f="dataReader", signature=signature("NIfTIMetaInfo"), 
		def=function(x, offset=0) {
			BinaryReader(x@dataFile, x@dataOffset+offset, .getRStorage(x@dataType), x@bytesPerElement, x@endian)
		})
			
setMethod(f="dataReader", signature=signature("AFNIMetaInfo"), 
		def=function(x, offset=0) {
			#### won't work for sub-bricks with different data types
			BinaryReader(x@dataFile, x@dataOffset+offset, .getRStorage(x@dataType), x@bytesPerElement, x@endian)
			#### won't work for sub-bricks with different data types
		})			

niftiDim <- function(nifti_header) {
	dimarray <- nifti_header$dimensions
	lastidx <- min(which(dimarray == 1)) - 1
	dimarray[2:lastidx]
}
#' This class contains meta information from an image
#'
#' @param Dim image dimensions
#' @param spacing voxel dimensions
#' @param origin coordinate origin
#' @param label name(s) of images 
#' @param spatialAxes image axes for spatial dimensions (x,y,z)
#' @param additionalAxes axes for dimensions > 3 (e.g. time, color band, direction)
#' @return an instance of class \code{\linkS4class{BrainMetaInfo}}
#' @export BrainMetaInfo
BrainMetaInfo <- function(Dim, spacing, origin=rep(0, length(spacing)), dataType="FLOAT", label="", spatialAxes=OrientationList3D$AXIAL_LPI, additionalAxes=NullAxis) {
	new("BrainMetaInfo",
			Dim=Dim,
			spacing=spacing,
			origin=origin,
			dataType=dataType,
			label=label,
			spatialAxes=spatialAxes,
			additionalAxes=additionalAxes)
	
}						

#' Constructor for \code{\linkS4class{NIfTIMetaInfo}} class
#' @param descriptor an instance of class \code{\linkS4class{NIfTIFileDescriptor}}
#' @param nifti_header a \code{list} returned by \code{readNIftiHeader}
#' @return an instance of class \code{\linkS4class{NIfTIMetaInfo}}
#' @export NIfTIMetaInfo
NIfTIMetaInfo <- function(descriptor, nifti_header) {
	stopifnot(!is.null(nifti_header$fileType) || (nifti_header$fileType == "NIfTI"))

	new("NIfTIMetaInfo",
			headerFile=headerFile(descriptor, nifti_header$fileName),
			dataFile=dataFile(descriptor, nifti_header$fileName),
			fileDescriptor=descriptor,
			endian=nifti_header$endian,
			dataOffset=nifti_header$voxOffset,
			dataType=nifti_header$dataStorage,
			bytesPerElement=as.integer(nifti_header$bitpix/8),
			Dim=niftiDim(nifti_header),
			spatialAxes=OrientationList3D$AXIAL_LPI,
			additionalAxes=NullAxis,
			spacing=nifti_header$pixdim[2:4],
			origin=nifti_header$qoffset,
			label=stripExtension(descriptor, basename(nifti_header$fileName)),
			intercept=nifti_header$sclIntercept,
			slope=nifti_header$sclSlope,
			header=nifti_header)
}



setMethod(f="show", signature=signature("FileMetaInfo"), 
		def=function(object) {
			cat("an instance of class",  class(object), "\n\n")
			cat("headerFile:", "\t", object@headerFile, "\n")
			cat("dataFile:", "\t", object@dataFile, "\n")
			cat("endian:", "\t", object@endian, "\n")
			cat("dataOffset:", "\t", object@dataOffset, "\n")
			cat("dataType:", "\t", object@dataType, "\n")
			cat("dimensions:", "\t", object@Dim, "\n")
			cat("voxel size:", "\t", object@spacing, "\n")
			cat("origin:", "\t", object@origin, "\n")
			cat("label(s):", "\t", object@label, "\n")
			cat("intercept:", "\t", object@intercept, "\n")
			cat("slope:", "\t\t", object@slope, "\n\n")
			
			cat("additional format-specific info may be contained in @header slot", "\n")			
		})



#' Constructor for \code{\linkS4class{AFNIMetaInfo}} class
#' @param descriptor an instance of class \code{\linkS4class{AFNIFileDescriptor}}
#' @param afni_header a \code{list} returned by \code{readAFNIHeader}
#' @return an instance of class \code{\linkS4class{AFNIMetaInfo}}
#' @export AFNIMetaInfo
AFNIMetaInfo <- function(descriptor, afni_header) {
		.Dim <- afni_header$DATASET_DIMENSIONS$content[afni_header$DATASET_DIMENSIONS$content > 0]
		if (afni_header$DATASET_RANK$content[2] > 1) {
			.Dim <- c(.Dim, afni_header$DATASET_RANK$content[2])			
		}
		
		new("AFNIMetaInfo",
			headerFile=headerFile(descriptor, afni_header$fileName),
			dataFile=dataFile(descriptor, afni_header$fileName),
			fileDescriptor=descriptor,
			endian=ifelse(afni_header[["BYTEORDER_STRING"]]$content == "MSB_FIRST", "big", "little"),
			dataOffset=0,
			dataType=switch(afni_header$BRICK_TYPES$content[1], "0"="BYTE", "1"="SHORT", "3"="FLOAT"),
			bytesPerElement=as.integer(switch(afni_header$BRICK_TYPES$content[1], "0"=1, "1"=2, "3"=4)),
			Dim=.Dim,
			spatialAxes=OrientationList3D$AXIAL_LPI,   # incorrect
			additionalAxes=NullAxis,            # incorrect
			spacing=abs(afni_header$DELTA$content),
			origin=afni_header$ORIGIN$content,
			label=afni_header$BRICK_LABS$content,
			intercept=0,
			slope=ifelse(afni_header$BRICK_FLOAT_FACS$content == 0, 1, afni_header$BRICK_FLOAT_FACS$content),
			header=afni_header)
}
			
			
#' read header information of an image file
#'
#'
#' @param fileName the name of the file to read
#' @return an instance of class \code{\linkS4class{FileMetaInfo}} 
readHeader <- function(fileName) {
	desc <- findDescriptor(fileName) 
	if (is.null(desc)) {
		stop(paste("could not find reader for file: ", fileName))
	}
	
	readMetaInfo(desc, fileName)			
}

setAs("BrainMetaInfo", "NIfTIMetaInfo", function(from) {
			if (inherits(from, "NIfTIMetaInfo")) {
				from
			} else {
				hdr <- as.nifti.header(from)
				desc <- findDescriptor(hdr$fileName)
				NIfTIMetaInfo(desc, hdr)
			}
						
		})



