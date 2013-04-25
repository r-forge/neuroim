roxygen <- function() NULL



#' Generic function to load data from a data source
#' @param x a data source
#' @export loadData
#' @rdname loadData-methods
setGeneric(name="loadData", def=function(x, ...) standardGeneric("loadData"))

#' Generic function to apply a function to an object
#' @param x the object that is mapped
#' @param m the mapping object
#' @export map
#' @rdname map-methods
setGeneric(name="map", def=function(x, m, ...) standardGeneric("map"))



#' Generic function to extract the number of dimensions of an object
#' @param x n-dimensional object
#' @export ndim
#' @rdname ndim-methods
setGeneric(name="ndim", def=function(x, ...) standardGeneric("ndim"))

#' Generic function to add a dimension to an object
#' @param x a dimensioned object
#' @param n the size of the dimension to add
#' @export addDim
#' @rdname addDim-methods
setGeneric(name="addDim", def=function(x, n) standardGeneric("addDim"))

#' Generic function to drop a dimension from an object
#' @param x a dimensioned object
#' @export dropDim
#' @rdname dropDim-methods
setGeneric(name="dropDim", def=function(x, dimnum) standardGeneric("dropDim"))

#' Generic function to extract the \code{space} member variable
#' @param x the object to query
#' @return an object representing the geometric space of the image
#' @export space
#' @rdname space-methods
setGeneric(name="space", def=function(x, ...) standardGeneric("space"))

#' Generic function to fill disjoint sets of values with the output of a function
#' @param x the object to split
#' @param fac the factor to split by
#' @param FUN the function to summarize the the clusters
#' @return a new object where the original values have been replaced by the function output
#' @export splitFill
#' @rdname splitFill-methods
setGeneric(name="splitFill", def=function(x, fac, FUN) standardGeneric("splitFill"))

#' Generic function to map values from one set to another using a user-supplied lookup table
#' @param x the object to map values from
#' @param lookup the lookup table
#' @return a new object where the original values have been filled in with the values in the lookup table
#' @export fill
#' @rdname fill-methods
setGeneric(name="fillWith", def=function(x, lookup) standardGeneric("fill"))



#' Generic function to center/scale subsets of an object
#' @param x a numeric matrix(like) object
#' @param f the conditioning expression (usually a factor)
#' @return a new matrix(like) object where the original values have been scaled
#' @export splitScale
#' @rdname splitScale-methods
setGeneric(name="splitScale", def=function(x, f, center, scale) standardGeneric("splitScale"))



#' Generic function to extract the voxel dimensions of an image
#' @param x the object
#' @return a numeric vector
#' @export spacing 
#' @rdname spacing-methods
setGeneric(name="spacing", def=function(x) standardGeneric("spacing"))

#' Generic function to extract the spatial bounds (origin + dim * spacing) of an image
#' param x the object
#' @export bounds
#' @rdname bounds-methods
setGeneric(name="bounds",     def=function(x) standardGeneric("bounds"))


#' Generic getter function to extract image axes
#' @param x an object with a set of axes
#' @export axes
#' @rdname axes-methods
setGeneric(name="axes",  def=function(x) standardGeneric("axes"))

#' Generic getter to extract image origin
#' @param x an object with an origin
#' @export origin
#' @rdname origin-methods
setGeneric(name="origin", def=function(x) standardGeneric("origin"))

#' Generic getter to extract image coordinate transformation
#' @param x an object with a transformation
#' @export trans
#' @rdname trans-methods
setGeneric(name="trans",  def=function(x) standardGeneric("trans"))

#' Generic getter to extract inverse image coordinate transformation
#' @param x an object
#' @export inverseTrans
#' @rdname inverseTrans-methods
setGeneric(name="inverseTrans", def=function(x) standardGeneric("inverseTrans"))

#' Generic function to read a sequence of elements from an input source
#' @param x the input channel
#' @param numElements the number of elements to read
#' @return the elements as a vector
#' @export readElements
#' @rdname readElements-methods
setGeneric(name="readElements", def=function(x, numElements) standardGeneric("readElements"))


#' Generic function to write a sequence of elements from an input source
#' @param x the output channel
#' @param els the elements to write
#' @export writeElements
#' @rdname writeElements-methods
setGeneric(name="writeElements", def=function(x, els) standardGeneric("writeElements"))


#' Generic function to write an image volume to disk
#' @param x an image object
#' @param fileName a file name
#' @param format file format string
#' @param dataType output data type
#' @export writeVolume
#' @rdname writeVolume-methods
setGeneric(name="writeVolume",  def=function(x, fileName, format, dataType) standardGeneric("writeVolume"))


#' Generic function to write an image vector to disk
#' @param x the image to write
#' @export writeVector 
#' @rdname writeVector-methods
setGeneric(name="writeVector",  def=function(x, fileName, format, dataType) standardGeneric("writeVector"))

#' Generic function to extract a value
setGeneric(name="value",       def=function(object, x,y, ...) standardGeneric("value"))

#' Generic function to convert 1D indices to N-dimensional grid coordinates
#' @param x the object
#' @param idx the 1D indices
#' @return a matrix of grid coordinates
#' @export indexToGrid
#' @rdname indexToGrid-methods
setGeneric(name="indexToGrid",   def=function(x, idx) standardGeneric("indexToGrid"))

#' Generic function to convert 1D indices to N-dimensional real world coordinates
#' @param x the object
#' @param idx the 1D indices
#' @return a matrix of real coordinates
#' @export indexToCoord
#' @rdname indexToCoord-methods
setGeneric(name="indexToCoord",   def=function(x, idx) standardGeneric("indexToCoord"))

#' Generic function to convert N-dimensional real world coordinates to 1D indices
#' @param x the object
#' @param coords a matrix of real world coordinates
#' @return a vector of indices
#' @export coordToIndex
#' @rdname coordToIndex-methods
setGeneric(name="coordToIndex",   def=function(x, coords) standardGeneric("coordToIndex"))

#' Generic function to convert N-dimensional real world coordinates to grid coordinates
#' @param x the object
#' @param coords a matrix of real world coordinates
#' @return a matrix of grid coordinates
#' @export coordToGrid
#' @rdname coordToGrid-methods
setGeneric(name="coordToGrid",   def=function(x, coords) standardGeneric("coordToGrid"))


#' Generic function to convert N-dimensional grid coordinate to 1D indices
#' @param x the object
#' @param coords a matrix where each row is a corodinate or a vector of length N
#' @return a vector of indices
#' @export gridToIndex
#' @rdname gridToIndex-methods
setGeneric(name="gridToIndex",   def=function(x, coords) standardGeneric("gridToIndex"))


#' Generic function to apply a function to each volume of a four-dimensional image
#' @param x four-dimensional image
#' @param FUN a \code{function} taking one or two arguments (depending on the value of \code{withIndex}
#' @param withIndex whether the index of the volume supplied as the second argument to the function
#' @export eachVolume
#' @rdname eachVolume-methods
setGeneric(name="eachVolume", def=function(x, FUN, withIndex, ...) standardGeneric("eachVolume"))

#' Generic function to extract a volume from a four-dimensional image
#' @param x four-dimensional image
#' @param i the indices of the volume(s) to extract
#' @export takeVolume
#' @rdname takeVolume-methods
setGeneric(name="takeVolume", def=function(x, i, ...) standardGeneric("takeVolume"))


#' Generic functions to apply a function to each (2D) slice of an image
#' @param x the object
#' @param FUN a \code{function} taking one or two arguments (depending on the value of \code{withIndex}
#' @param withIndex whether the index of the slice is supplied as the second argument to the function
#' @export eachSlice
#' @rdname eachSlice-methods
setGeneric(name="eachSlice", def=function(x, FUN, withIndex, ...) standardGeneric("eachSlice"))

#' Generic functions to apply a function to each series of a 4D image
#' That is, if the 4th dimension is 'time' each series is a 1D time series.
#' @param x a four dimensional image
#' @param FUN a \code{function} taking one or two arguments (depending on the value of \code{withIndex}
#' @param withIndex whether the index of the series is supplied as the second argument to the function
#' @export eachSeries
#' @rdname eachSeries-methods
setGeneric(name="eachSeries", def=function(x, FUN, withIndex, ...) standardGeneric("eachSeries"))


#' Generic function to extract a set of series from a 4D image
#' @param x a four dimensional image
#' @param i the indices of the series' to extract
#' @export takeSeries
#' @rdname takeSeries-methods
setGeneric(name="takeSeries", def=function(x, indices, ...) standardGeneric("takeSeries"))


#' Convert to sparse representation
#' @param x the object to sparsify
#' @param mask the elements to retain
#' @export as.sparse
#' @rdname as.sparse-methods
setGeneric(name="as.sparse", def=function(x, mask, ...) standardGeneric("as.sparse"))

#' Convert to a LogicalBrainVolume
#' @param x the object to binarize
#' @param indices the indices to set to TRUE
#' @export as.mask
#' @rdname as.mask-methods
setGeneric(name="as.mask", def=function(x, indices) standardGeneric("as.mask"))


#' tesselate
#' @param x the object to tesselate
#' @param K the number of partitions
#' @export tesselate
#' @rdname tesselate-methods
setGeneric(name="tesselate", def=function(x, K, ...) standardGeneric("tesselate"))

#' partition
#' @param x the object to partition
#' @param K the number of partitions
#' @param features the features used to define the partition
#' @export partition
#' @rdname partition-methods
setGeneric(name="partition", def=function(x, K, features, ...) standardGeneric("partition"))

#' mergePartitions
#' @param x the object to merge
#' @param K the number of merged partitions
#' @param features the features used to define the partition
#' @export mergePartitions
#' @rdname mergePartitions-methods
setGeneric(name="mergePartitions", def=function(x, K, features, ...) standardGeneric("mergePartitions"))

#' numClusters
#' @param x the object to extract number of clusters 
#' @export numClusters
#' @rdname numClusters-methods
setGeneric(name="numClusters", def=function(x) standardGeneric("numClusters"))

#' clusterCenters
#' @param x the object to extract cluster centers from
#' @export clusterCenters
#' @rdname clusterCenters-methods
setGeneric(name="clusterCenters", def=function(x, features, FUN) standardGeneric("clusterCenters"))



setGeneric(name="pick", def=function(x, mask, ...) standardGeneric("pick"))


#' Extract coordinates
#' @param x the object to extract coordinates from
#' @export coords
#' @rdname coords-methods
setGeneric(name="coords", def=function(x, ...) standardGeneric("coords"))

#' Extract indices
#' @param x the object to extract indices
#' @export indices
#' @docType methods
#' @rdname indices-methods
setGeneric(name="indices", def=function(x) standardGeneric("indices"))

#' Index Lookup operation
#' @param x the object to query
#' @export lookup
#' @docType methods
#' @rdname lookup-methods
setGeneric(name="lookup", def=function(x, i, ...) standardGeneric("lookup"))

#' Extract vector series from object
#' @param x the object
#' @export series
#' @rdname series-methods
setGeneric(name="series", def=function(x, i, ...) standardGeneric("series"))   

#' extract a 2D slice from an image volume
#' @param x the object
#' @param zlevel coordinate (in voxel units) along the sliced axis
#' @param along the axis along which to slice
#' @param orientation the target orientation of the 2D slice
#' @export slice
#' @rdname slice-methods
setGeneric(name="slice", def=function(x, zlevel, along, orientation, ...) standardGeneric("slice"))   


#' extract permutation matrix
#' @param x the object
#' @export permMat
#' @rdname permMat-methods
setGeneric(name="permMat", def=function(x, ...) standardGeneric("permMat"))   

#' Concatenate two objects
#' @param x the first object
#' @param y the second object
#' @param ... additonal objects
#' @export concat
#' @rdname concat-methods
setGeneric(name="concat", def=function(x,y, ...) standardGeneric("concat"))

#' Find connected components
#' @name connComp
#' @param x the image object
#' @param ... additonal arguments
#' @export
#' @docType methods
#' @rdname connComp-methods
setGeneric(name="connComp", def=function(x, ...) standardGeneric("connComp"))

#' Construct a series iterator
#' @param x the object to be iterated
#' @export seriesIter
#' @rdname seriesIter-methods
setGeneric(name="seriesIter", def=function(x) standardGeneric("seriesIter"))


#' extract voxel coordinates
#' @param x the object to extract voxels from
#' @export voxels
#' @rdname voxels-methods
setGeneric(name="voxels", def=function(x, ...) standardGeneric("voxels"))


if (!isGeneric("image"))
  setGeneric("image", function(x, ...) standardGeneric("image"))

