roxygen <- function() NULL



#' Generic function to load data from a data source
#' @param x a data source
#' @exportMethod loadData
setGeneric(name="loadData", def=function(x, ...) standardGeneric("loadData"))

#' Generic function to extract the number of dimensions of an object
#' @param x n-dimensional object
#' @exportMethod ndim
setGeneric(name="ndim", def=function(x, ...) standardGeneric("ndim"))

#' Generic function to add a dimension to an object
#' @param x a dimensioned object
#' @param n the size of the dimension to add
#' @exportMethod addDim
setGeneric(name="addDim", def=function(x, n) standardGeneric("addDim"))

#' Generic function to drop a dimension from an object
#' @param x a dimensioned object
#' @exportMethod dropDim
setGeneric(name="dropDim", def=function(x) standardGeneric("dropDim"))

#' Generic function to extract the \code{space} member variable
#' @param x the object to query
#' @return an object representing the geometric space of the image
#' @exportMethod space
setGeneric(name="space", def=function(x, ...) standardGeneric("space"))


#' Generic function to extract the voxel dimensions of an image
#' @param x the object
#' @return a numeric vector
#' @exportMethod spacing 
setGeneric(name="spacing", def=function(x) standardGeneric("spacing"))

#' generic function to extract the spatial bounds (origin + dim * spacing) of an image
#' param x the object
#' @exportMethod bounds
setGeneric(name="bounds",     def=function(x) standardGeneric("bounds"))


#' Generic getter function to extract image axes
#' @param x an object with a set of axes
#' @exportMethod axes
setGeneric(name="axes",  def=function(x) standardGeneric("axes"))

#' Generic getter to extract image origin
#' @param x an object with an origin
#' @exportMethod origin
setGeneric(name="origin", def=function(x) standardGeneric("origin"))

#' Generic getter to extract image coordinate transformation
#' @param x an object with a transformation
#' @exportMethod trans
setGeneric(name="trans",  def=function(x) standardGeneric("trans"))

#' Generic getter to extract inverse image coordinate transformation
#' @exportMethod inverseTrans
setGeneric(name="inverseTrans", def=function(x) standardGeneric("inverseTrans"))

#' Generic function to read a sequence of elements from an input source
#' @param x the input channel
#' @param numElements the number of elements to read
#' @return the elements as a vector
#' @exportMethod readElements
setGeneric(name="readElements", def=function(x, numElements) standardGeneric("readElements"))


#' Generic function to write a sequence of elements from an input source
#' @param x the output channel
#' @param els the elements to write
#' @exportMethod writeElements
setGeneric(name="writeElements", def=function(x, els) standardGeneric("writeElements"))


#### generics related to BrainVolume class
#' @exportMethod writeVolume
setGeneric(name="writeVolume",  def=function(x, fileName, format, dataType) standardGeneric("writeVolume"))

#' @exportMethod writeVector 
setGeneric(name="writeVector",  def=function(x, fileName, format, dataType) standardGeneric("writeVector"))


setGeneric(name="value",       def=function(object, x,y, ...) standardGeneric("value"))

#' Generic function to convert a 1D index to N-dimensional grid coordinate
#' @param x the object
#' @param idx the 1D indices
#' @return a matrix of grid coordinates
#' @exportMethod indexToGrid
setGeneric(name="indexToGrid",   def=function(x, idx) standardGeneric("indexToGrid"))


#' Generic function to convert N-dimensional grid coordinate to 1D indices
#' @param x the object
#' @param coords a matrix where each row is a corodinate or a vector of length N
#' @return a vector of indices
#' @exportMethod gridToIndex
setGeneric(name="gridToIndex",   def=function(x, coords) standardGeneric("gridToIndex"))


#' Generic function to apply a function to each volume of a four-dimensional image
#' @param x four-dimensional image
#' @param FUN a \code{function} taking one or two arguments (depending on the value of \code{withIndex}
#' @param withIndex whether the index of the volume supplied as the second argument to the function
#' @exportMethod eachVolume
setGeneric(name="eachVolume", def=function(x, FUN, withIndex, ...) standardGeneric("eachVolume"))

#' Generic function to extract a volume from a four-dimensional image
#' @param x four-dimensional image
#' @param i the indices of the volume(s) to extract
#' @exportMethod takeVolume
setGeneric(name="takeVolume", def=function(x, i, ...) standardGeneric("takeVolume"))


#' Generic functions to apply a function to each (2D) slice of an image
#' @param x the object
#' @param FUN a \code{function} taking one or two arguments (depending on the value of \code{withIndex}
#' @param withIndex whether the index of the slice is supplied as the second argument to the function
#' @exportMethod eachSlice
setGeneric(name="eachSlice", def=function(x, FUN, withIndex, ...) standardGeneric("eachSlice"))

#' Generic functions to apply a function to each series of a 4D image
#' That is, if the 4th dimension is 'time' each series is a 1D time series.
#' @param x a four dimensional image
#' @param FUN a \code{function} taking one or two arguments (depending on the value of \code{withIndex}
#' @param withIndex whether the index of the series is supplied as the second argument to the function
#' @exportMethod eachSeries
setGeneric(name="eachSeries", def=function(x, FUN, withIndex, ...) standardGeneric("eachSeries"))


#' Generic function to extract a set of series from a 4D image
#' @param x a four dimensional image
#' @param i the indices of the series' to extract
#' @exportMethod takeSeries
setGeneric(name="takeSeries", def=function(x, indices, ...) standardGeneric("takeSeries"))


#' Convert to sparse representation
#' @param x the object to sparsify
#' @param mask the elements to retain
#' @exportMethod as.sparse
setGeneric(name="as.sparse", def=function(x, mask, ...) standardGeneric("as.sparse"))

setGeneric(name="pick", def=function(x, mask, ...) standardGeneric("pick"))

setGeneric(name="coords", def=function(x, ...) standardGeneric("coords"))
setGeneric(name="indices", def=function(x) standardGeneric("indices"))
setGeneric(name="lookup", def=function(x, i, ...) standardGeneric("lookup"))
setGeneric(name="series", def=function(x, i, ...) standardGeneric("series"))          
setGeneric(name="concat", def=function(x,y, ...) standardGeneric("concat"))

setGeneric(name="connComp", def=function(x, ...) standardGeneric("connComp"))
setGeneric(name="seriesIter", def=function(x) standardGeneric("seriesIter"))

