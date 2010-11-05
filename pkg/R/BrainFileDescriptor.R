#' @include AllClass.R
roxygen()
#' @include NIFTI_IO.R
roxygen()

#' @include AFNI_IO.R
roxygen()



#' Generic function to test whether a file name conforms to the given \code{\linkS4class{BrainFileDescriptor}} instance.
#' Will test for match to either header file or data file
#' @param x object for which the file name is to matched to
#' @param fileName file name to be matched
#' @return TRUE for match, FALSE otherwise
#' @exportMethod fileMatches
setGeneric(name="fileMatches", def=function(x, fileName) standardGeneric("fileMatches"))


#' Generic function to test whether a file name conforms to the given \code{\linkS4class{BrainFileDescriptor}} instance.
#' Will test for match to header file only
#' @param x object for which the file name is to matched to
#' @param fileName file name to be matched
#' @return TRUE for match, FALSE otherwise
#' @exportMethod headerFileMatches
setGeneric(name="headerFileMatches", def=function(x, fileName) standardGeneric("headerFileMatches"))

#' Generic function to test whether a file name conforms to the given a \code{\linkS4class{BrainFileDescriptor}} instance.
#' Will test for match to data file only
#' @param x object for which the file name is to matched to
#' @param fileName file name to be matched
#' @return TRUE for match, FALSE otherwise
#' @exportMethod dataFileMatches
setGeneric(name="dataFileMatches", def=function(x, fileName) standardGeneric("dataFileMatches"))

#' Generic function to get the name of the header file, given a file name and a \code{\linkS4class{BrainFileDescriptor}} instance.
#' @param x descriptor instance
#' @param fileName file name to be stripped of its extension
#' @return the correct header name
#' @exportMethod headerFile
setGeneric(name="headerFile", def=function(x, fileName) standardGeneric("headerFile"))

#' Generic function to get the name of the data file, given a file name and a \code{\linkS4class{BrainFileDescriptor}} instance.
#' @param x descriptor instance
#' @param fileName file name to be stripped of its extension
#' @return the correct header name
#' @exportMethod dataFile
setGeneric(name="dataFile", def=function(x, fileName) standardGeneric("dataFile"))

#' Generic function to strip extension from file name, given a \code{\linkS4class{BrainFileDescriptor}} instance.
#' @param x descriptor instance
#' @param fileName file name to be stripped of its extension
#' @return fileName without extension
#' @exportMethod stripExtension
setGeneric(name="stripExtension", def=function(x, fileName) standardGeneric("stripExtension"))

#' Generic function to read image meta info given a file and a \code{\linkS4class{BrainFileDescriptor}} instance.
#' @param x descriptor instance
#' @param fileName file name contianing meta information
#' @exportMethod readMetaInfo
setGeneric(name="readMetaInfo", def=function(x, fileName) standardGeneric("readMetaInfo"))






setMethod(f="fileMatches", signature=signature(x= "BrainFileDescriptor", fileName="character"),
		def=function(x, fileName) {
			headerFileMatches(x,fileName) || dataFileMatches(x,fileName)			
		})


setMethod(f="headerFileMatches", signature=signature(x= "BrainFileDescriptor", fileName="character"),
		def=function(x, fileName) {
			regexpr(paste(".*", x@headerExtension, "$", sep=""), fileName) > 0
					
		})
 
setMethod(f="dataFileMatches", signature=signature(x= "BrainFileDescriptor", fileName="character"),
		def=function(x, fileName) {
			regexpr(paste(".*", x@dataExtension, "$", sep=""), fileName) > 0
		})

setMethod(f="headerFile",signature=signature(x= "BrainFileDescriptor", fileName="character"),
		def=function(x, fileName) {
			if (headerFileMatches(x, fileName)) {
				fileName
			} else if (dataFileMatches(x, fileName)) {
				paste(stripExtension(x, fileName), x@headerExtension, sep=".")				
			} else {
				stop(paste("could not derive header file name from: ", fileName))
			}		
		})

setMethod(f="dataFile",signature=signature(x= "BrainFileDescriptor", fileName="character"),
		def=function(x, fileName) {
			if (dataFileMatches(x, fileName)) {
				fileName
			} else if (headerFileMatches(x, fileName)) {
				paste(stripExtension(x, fileName), x@dataExtension, sep=".")
			} else {
				stop(paste("could not derive data file name from: ", fileName))
			}				
		})

setMethod(f="stripExtension",signature=signature(x= "BrainFileDescriptor", fileName="character"),
		def=function(x, fileName) {
			if (headerFileMatches(x, fileName)) {
				ret <- strsplit(fileName, paste(x@headerExtension, "$", sep=""))[[1]][1]	
				substr(ret, 1, nchar(ret)-1)
			} else if (dataFileMatches(x, fileName)) {
				ret <- strsplit(fileName, paste(x@dataExtension, "$", sep=""))[[1]][1]		
				substr(ret, 1, nchar(ret)-1)
			} else {
				stop("file does not match descriptor: " + x)
			}		
		})


setMethod(f="readMetaInfo",signature=signature(x= "NIfTIFileDescriptor"),
		def=function(x, fileName) {
			header <- readNIfTIHeader(fileName)			
			NIfTIMetaInfo(x, header)
		})

setMethod(f="readMetaInfo",signature=signature(x= "AFNIFileDescriptor"),
		def=function(x, fileName) {
			header <- readAFNIHeader(fileName)	
			header$fileName <- fileName
			AFNIMetaInfo(x, header)
		})

findDescriptor <- function(fileName) {
	if (fileMatches(NIFTI, fileName)) NIFTI
	else if (fileMatches(NIFTI_GZ, fileName)) NIFTI_GZ
	else if (fileMatches(NIFTI_PAIR, fileName)) NIFTI_PAIR
	else if (fileMatches(NIFTI_PAIR_GZ, fileName)) NIFTI_PAIR_GZ
	else if (fileMatches(AFNI, fileName)) AFNI
	else if (fileMatches(AFNI_GZ, fileName)) AFNI_GZ
	else NULL
}

AFNI <- new("AFNIFileDescriptor",
		fileFormat="AFNI",
		headerEncoding="raw",
		headerExtension="HEAD",
		dataEncoding="raw",
		dataExtension="BRIK")

AFNI_GZ <- new("AFNIFileDescriptor",
		fileFormat="AFNI",
		headerEncoding="gzip",
		headerExtension="HEAD.gz",
		dataEncoding="gzip",
		dataExtension="BRIK.gz")

NIFTI <- new("NIfTIFileDescriptor",
		fileFormat="NIfTI",
		headerEncoding="raw",
		headerExtension="nii",
		dataEncoding="raw",
		dataExtension="nii")

NIFTI_GZ <- new("NIfTIFileDescriptor",
		fileFormat="NIfTI",
		headerEncoding="gzip",
		headerExtension="nii.gz",
		dataEncoding="gzip",
		dataExtension="nii.gz")

NIFTI_PAIR <- new("NIfTIFileDescriptor",
		fileFormat="NIfTI",
		headerEncoding="raw",
		headerExtension="hdr",
		dataEncoding="raw",
		dataExtension="img")

NIFTI_PAIR_GZ <- new("NIfTIFileDescriptor",
		fileFormat="NIfTI",
		headerEncoding="gzip",
		headerExtension="hdr.gz",
		dataEncoding="gzip",
		dataExtension="img.gz")




