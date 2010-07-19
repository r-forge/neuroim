
#' @include AllClass.R
roxygen()

#' Constructor for  \code{\linkS4class{BinaryReader}} class
#' @param input of file to read from or else a \code{connection} object
#' @param byteOffset the number of bytes to skip at the start of input
#' @param dataType R data type of binary elements
#' @param bytesPerElement number of bytes in each data element (e.g. 4 or 8 for floating point numbers)
#' @param endian endianness of binary input connection
#' @export BinaryReader
BinaryReader <- function(input, byteOffset, dataType, bytesPerElement, endian=.Platform$endian) {
	if (is.character(input)) { 
		new("BinaryReader", input=file(input, open="rb"), byteOffset=as.integer(byteOffset), dataType=dataType, bytesPerElement=as.integer(bytesPerElement), endian=endian)
	} else {
		stopifnot(inherits(input, connection)) 
		new("BinaryReader", input=input, byteOffset=as.integer(byteOffset), dataType=dataType, bytesPerElement=as.integer(bytesPerElement), endian=endian)		
	}

}



#' Generic function to read a sequence of elements from an input source
#' @param x the input source
#' @param numElements the number of elements to be read
#' @return the elemtns as a vector
#' @export readElements
setGeneric("readElements", function(x, numElements) standardGeneric("readElements"))

setMethod("initialize", "BinaryReader", function(.Object, input, byteOffset, dataType, bytesPerElement, endian) {
			.Object@input <- input
			.Object@byteOffset <- byteOffset
			.Object@dataType <- dataType
			.Object@bytesPerElement <- bytesPerElement
			.Object@endian <- endian
			
			## must be seekable connection, should enforce this
			## 
			seek(.Object@input, where=.Object@byteOffset, origin="start")		
			.Object
		}) 

setMethod(f="readElements", signature=signature(x= "BinaryReader", numElements="numeric"),
		def=function(x, numElements) {
			readBin(x@input, what=x@dataType, size=x@bytesPerElement, n=numElements, endian=x@endian)		
		})

setMethod(f="close", signature=signature(con= "BinaryReader"),
		def=function(con) {
			base::close(con@input)				
		})


