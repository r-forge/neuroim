roxygen <- function() NULL



#' Generic function to load data from a data source
#' @param x a data source
#' @export loadData
setGeneric("loadData", function(x, ...) standardGeneric("loadData"))

#' Generic function to extract the number of dimensions of an object
#' @param x n-dimensional object
#' @export ndim
setGeneric("ndim", function(x, ...) standardGeneric("ndim"))

#' Generic function to add a dimension to an object
#' @param x a dimensioned object
#' @param n the size of the dimension to add
#' @export addDim
setGeneric("addDim", function(x, n) standardGeneric("addDim"))

#' Generic function to drop a dimension from an object
#' @param x a dimensioned object
#' @export dropDim
setGeneric("dropDim", function(x) standardGeneric("dropDim"))

#' Generic function to extract the \code{space} member variable
#' @param x the object to query
#' @return an object representing the geometric space of the image
#' @export space
setGeneric("space", function(x, ...) standardGeneric("space"))


#' Generic function to extract the voxel dimensions of an image
#' @param x the object
#' @return a numeric vector
#' @export spacing
setGeneric("spacing",     function(x) standardGeneric("spacing"))

#' generic function to extract the spatial bounds (origin + dim * spacing) of an image
#' param x the object
setGeneric("bounds",     function(x) standardGeneric("bounds"))


#' Generic getter function to extract image axes
#' @param x an object with a set of axes
#' @export axes
setGeneric("axes",  function(x) standardGeneric("axes"))

#' Generic getter to extract image origin
setGeneric("origin", function(x) standardGeneric("origin"))

#' Generic getter to extract image coordinate transformation
setGeneric("trans",  function(x) standardGeneric("trans"))

#' Generic getter to extract inverse image coordinate transformation
setGeneric("inverseTrans", function(x) standardGeneric("inverseTrans"))

#' Generic function to read a sequence of elements from an input source
#' @param x the input channel
#' @param numElements the number of elements to read
#' @return the elements as a vector
#' @export readElements
setGeneric("readElements", function(x, numElements) standardGeneric("readElements"))


#' Generic function to write a sequence of elements from an input source
#' @param x the output channel
#' @param els the elements to write
#' @export writeElements
setGeneric("writeElements", function(x, els) standardGeneric("writeElements"))


#### generics related to BrainVolume class
setGeneric("writeVolume",       function(x, fileName, format, dataType) standardGeneric("writeVolume"))
setGeneric("writeVector",       function(x, fileName, format, dataType) standardGeneric("writeVector"))
setGeneric("value",       function(object, x,y, ...) standardGeneric("value"))

#' Generic function to convert a 1D index to N-dimensional grid coordinate
#' @param x the object
#' @param idx the 1D indices
#' @return a matrix of grid coordinates
setGeneric("indexToGrid",   function(x, idx) standardGeneric("indexToGrid"))


#' Generic function to convert N-dimensional grid coordinate to 1D indices
#' @param x the object
#' @param coords a matrix where each row is a corodinate or a vector of length N
#' @return a vector of indices
setGeneric("gridToIndex",   function(x, coords) standardGeneric("gridToIndex"))


#' Generic function to apply a function to each volume of a four-dimensional image
#' @param x four-dimensional image
#' @param FUN a \code{function} taking one or two arguments (depending on the value of \code{withIndex}
#' @param withIndex whether the index of the volume supplied as the second argument to the function
#' @export eachVolume
setGeneric("eachVolume", function(x, FUN, withIndex, ...) standardGeneric("eachVolume"))

#' Generic function to extract a volume from a four-dimensional image
#' @param x four-dimensional image
#' @param i the indices of the volume(s) to extract
#' @export takeVolume
setGeneric("takeVolume", function(x, i, ...) standardGeneric("takeVolume"))


#' Generic functions to apply a function to each (2D) slice of an image
#' @param x the object
#' @param FUN a \code{function} taking one or two arguments (depending on the value of \code{withIndex}
#' @param withIndex whether the index of the slice is supplied as the second argument to the function
#' @export eachSlice
setGeneric("eachSlice", function(x, FUN, withIndex, ...) standardGeneric("eachSlice"))

#' Generic functions to apply a function to each series of a 4D image
#' That is, if the 4th dimension is 'time' each series is a 1D time series.
#' @param x a four dimensional image
#' @param FUN a \code{function} taking one or two arguments (depending on the value of \code{withIndex}
#' @param withIndex whether the index of the series is supplied as the second argument to the function
#' @export eachSeries
setGeneric("eachSeries", function(x, FUN, withIndex, ...) standardGeneric("eachSeries"))


#' Generic function to extract a set of series from a 4D image
#' @param x a four dimensional image
#' @param i the indices of the series' to extract
#' @export takeSeries
setGeneric("takeSeries", function(x, indices, ...) standardGeneric("takeSeries"))



setGeneric("as.sparse", function(x, mask, ...) standardGeneric("as.sparse"))

setGeneric("pick", function(x, mask, ...) standardGeneric("pick"))

setGeneric("coords", function(x, ...) standardGeneric("coords"))
setGeneric("indices", function(x) standardGeneric("indices"))
setGeneric("lookup", function(x, i, ...) standardGeneric("lookup"))
setGeneric("series", function(x, i, ...) standardGeneric("series"))          
setGeneric("concat", function(x,y, ...) standardGeneric("concat"))

setGeneric("connComp", function(x, ...) standardGeneric("connComp"))
setGeneric("seriesIter", function(x) standardGeneric("seriesIter"))

#### generics related to BrainHeader class
#setGeneric("headerFile",     function(x) standardGeneric("headerFile"))
#setGeneric("dataFile",     function(x) standardGeneric("dataFile"))
#setGeneric("fileType",  function(x) standardGeneric("fileType"))
#setGeneric("versionNum",  function(x) standardGeneric("versionNum"))
#setGeneric("encoding",  function(x) standardGeneric("encoding"))
#setGeneric("endian", function(x) standardGeneric("endian"))
#setGeneric("dataOffset", function(x) standardGeneric("dataOffset"))
#setGeneric("extraInfo", function(x) standardGeneric("extraInfo"))
#setGeneric("dataType", function(x) standardGeneric("dataType"))
#setGeneric("dataDim",function(x) standardGeneric("dataDim"))
#setGeneric("createSpace", function(x) standardGeneric("createSpace"))

####

#setGeneric("readData",function(x, indices) standardGeneric("readData"))
#setGeneric("readHeader",function(x) standardGeneric("readHeader"))
#setGeneric("writeData", function(x, header, data) standardGeneric("writeData"))
#setGeneric("writeHeader", function(x, header) standardGeneric("writeHeader"))

#setGeneric("fileName", function(x) standardGeneric("fileName"))
#setGeneric("path", function(x) standardGeneric("path"))
#setGeneric("openFor", function(x) standardGeneric("openFor"))
#setGeneric("headerFileExt", function(x) standardGeneric("headerFileExt"))
#setGeneric("dataFileExt", function(x) standardGeneric("dataFileExt"))

