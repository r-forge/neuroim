#' @include AllGeneric.R
roxygen()



setOldClass(c("file", "connection"))
setOldClass(c("gzfile", "connection"))

.package_env <- new.env()


setClass("Base", contains=c("VIRTUAL"))


#' This class represents an axis with a name attribute
#'
#' @slot axis the name of the axis
#' @exportClass NamedAxis		 
setClass("NamedAxis", representation=
				representation(axis="character"))


#' Virtual base class representing an ordered set of named axes.
#'
#' @slot ndim the number of axes (or dimensions)
#' @exportClass AxisSet
setClass("AxisSet", representation=representation(ndim="integer"))


#' A one-dimensional axis set
#'
#' @slot i the first axis
#' @exportClass AxisSet1D
setClass("AxisSet1D", representation=representation(i="NamedAxis"), contains=c("AxisSet"))

#' A two-dimensional axis set
#'
#' @slot j the first axis
#' @exportClass AxisSet2D
setClass("AxisSet2D", representation=representation(j="NamedAxis"), 
		contains=c("AxisSet1D"))

#' A three-dimensional axis set
#'
#' @slot k the third axis
#' @exportClass AxisSet3D
setClass("AxisSet3D", representation=representation(k="NamedAxis"),
		contains=c("AxisSet2D"))

#' A four-dimensional axis set
#'
#' @slot l the fourth axis
#' @exportClass AxisSet4D
setClass("AxisSet4D", representation=representation(l="NamedAxis"),
		contains=c("AxisSet3D"))

#' A five-dimensional axis set
#'
#' @slot m the fifth axis
#' @exportClass AxisSet5D
setClass("AxisSet5D", representation=representation(m="NamedAxis"),
		contains=c("AxisSet4D"))





#' This class represents a neuroimaging file format
#'
#' @slot fileFormat the name of the file format (e.g. NIfTI)
#' @slot headerEncoding the file encoding of the header file (e.g. 'raw' for binary, 'gzip' for gz compressed')
#' @slot headerExtension the file extension for the header file (e.g. 'nii' for NIfTI single files)
#' @slot dataEncoding the file encoding for the data file
#' @slot dataExtension the file extension for the data file (e.g. 'nii' for NIfTI single files)
#' @exportClass BrainFileDescriptor
setClass("BrainFileDescriptor",
		representation=
				representation(fileFormat="character",
						headerEncoding="character",
						headerExtension="character",
						dataEncoding="character",
						dataExtension="character"),
		contains=c("Base"))

#' This class supports the NIfTI file format
#'
#' @exportClass NIfTIFileDescriptor
setClass("NIfTIFileDescriptor", contains=c("BrainFileDescriptor"))


#' This class supports the AFNI file format
#'
#' @exportClass NIfTIFileDescriptor
setClass("AFNIFileDescriptor", contains=c("BrainFileDescriptor"))


#' This is a base class to represent meta information
setClass("BaseMetaInfo")

#' This is class is used to denote the absense of meta information
setClass("NullMetaInfo", contains=c("BaseMetaInfo"))


setMethod(f="show",
		signature=signature(object="BaseMetaInfo"),
		def=function(object) {
			cat("an instance of class",  class(object), "\n\n")
		})

setMethod(f="show",
		signature=signature(object="NullMetaInfo"),
			def=function(object) {
				cat("an instance of class",  class(object), "\n\n")
				cat("meta info is null \n")
			})
	
	
#' This class contains meta information from an image
#' @slot dataType the data type code, e.g. FLOAT
#' @slot Dim image dimensions
#' @slot spatialAxes image axes for spatial dimensions (x,y,z)
#' @slot additionalAxes axes for dimensions > 3 (e.g. time, color band, direction)
#' @slot spacing voxel dimensions
#' @slot origin coordinate origin
#' @slot label name(s) of images 
#' @exportClass BrainMetaInfo		 							 
setClass("BrainMetaInfo",
			representation=
					representation(
							dataType="character",
							Dim="numeric",
							spatialAxes="AxisSet3D",
							additionalAxes="AxisSet",
							spacing="numeric",
							origin="numeric",
							label="character"),
			
			#prototype=prototype(),
			contains=c("BaseMetaInfo"))

#' This class contains meta information from an image data file
#'
#' @slot headerFile name of the file containing meta information
#' @slot dataFile name of the file containing data
#' @slot fileDescriptor descriptor of image file format
#' @slot endian byte order of data ('little' or 'big')
#' @slot dataOffset the number of bytes preceding the start of image data in data file
#' @slot bytesPerElement number of bytes per element
#' @slot intercept constant value added to image -- multiple values allowed (must equal numer of sub-images)
#' @slot slope image multiplier -- multiple values allowed (must equal numer of sub-images)  
#' @slot header a list of format specific attributes
#' @exportClass FileMetaInfo		 							 
setClass("FileMetaInfo",
		representation=
				representation(headerFile="character",
					   dataFile="character",
					   fileDescriptor="BrainFileDescriptor",
					   endian="character",
					   dataOffset="numeric",					   
					   bytesPerElement="integer",
					   intercept="numeric",
					   slope="numeric",
					   header="list"),
					   
	    #prototype=prototype(),
		contains=c("BrainMetaInfo"))

#' This class contains meta information for a NIfTI image file
#'
#' @slot nifti_header a list of attributes specific to the NIfTI file format 
#' @exportClass NIfTIMetaInfo		 	
setClass("NIfTIMetaInfo",
		contains=c("FileMetaInfo"))


#' This class contains meta information for a AFNI image file
#'
#' @slot afni_header a list of attributes specific to the AFNI file format 
#' @exportClass AFNIMetaInfo		 	
setClass("AFNIMetaInfo",
		contains=c("FileMetaInfo"))


#' This is a base class to rpresent a data source
setClass("BaseSource", representation=representation(metaInfo="BaseMetaInfo"))



NullSource <- function() {
	ex <- exists(".NullSource", envir=.package_env)
	if (!ex) {
		ret <- new("BaseSource", metaInfo=new("NullMetaInfo"))	
		assign(".NullSource", ret, envir=.package_env)
		ret
	} else {
		get(".NullSource", envir=.package_env)
	}
	
}


#' Base class for representing a data source for images. The purpose of this class is to provide a layer in between 
#' low level IO and image loading functionality.
#' @slot metaInfo meta information for the data source
#' @exportClass BrainSource
setClass("BrainSource", representation=
				representation(metaInfo="BrainMetaInfo"),
				contains=c("BaseSource"))
		
#' Base class for representing a data source for images. The purpose of this class is to provide a layer in between 
#' low level IO and image loading functionality.
#' @slot metaInfo meta information for the data source
#' @exportClass BrainSource
setClass("BrainFileSource", representation=
				representation(metaInfo="FileMetaInfo"),
				contains=c("BrainSource"))
		
#' A class is used to produce a \code{\linkS4class{BrainVolume}} instance
#' @slot index the index of the volume to be read -- must be of length 1.
#' @exportClass BrainVolumeSource
		setClass("BrainVolumeSource", representation=
						representation(index="integer"),
				contains=c("BrainSource"))
		
#' A class that is used to produce a \code{\linkS4class{BrainVector}} instance
#' @slot indices the index vector of the volumes to be loaded
#' @exportClass BrainVectorSource
		setClass("BrainVectorSource", representation=
						representation(indices="integer"),
				contains=c("BrainSource"))
		

		
#' A class that is used to produce a \code{\linkS4class{SparseBrainVector}} instance
#' @slot mask the subset of voxels that will be stored in memory
#' @exportClass BrainVectorSource
		setClass("SparseBrainVectorSource", representation=
						representation(mask="LogicalBrainVolume"),
				contains=c("BrainVectorSource"))
		

#' A class that is used to produce a \code{\linkS4class{BrainBucket}} instance
#' @slot sourceList a list of sources for the bucket sub-volumes
#' @slot cache a cache used to store data in memory
#' @exportClass BrainBucketSource
		setClass("BrainBucketSource",
				representation=representation(sourceList="list", cache="environment"),
				contains=c("BrainVectorSource"))
		
		


#' This class supports reading of bulk binary data from a connection
#'
#' @slot input the binary input connection
#' @slot byteOffset the number of bytes to skip at the start of input
#' @slot dataType the dataType of the binary Elements
#' @slot bytesPerElement number of bytes in each data element (e.g. 4 or 8 for floating point numbers)
#' @slot endian endianness of binary input connection
#' @exportClass BinaryReader		 
setClass("BinaryReader", representation=
				representation(input="connection",
							   byteOffset="numeric",
							   dataType="character",
							   bytesPerElement="integer",
							   endian="character"))

#' This class supports writing of bulk binary data to a connection
#'
#' @slot output the binary output connection
#' @slot byteOffset the number of bytes to skip at the start of input
#' @slot dataType the dataType of the binary Elements
#' @slot bytesPerElement number of bytes in each data element (e.g. 4 or 8 for floating point numbers)
#' @slot endian endianness of binary output connection
#' @exportClass BinaryReader		 
setClass("BinaryWriter", representation=
							   representation(output="connection",	
							   byteOffset="numeric",
							   dataType="character",
							   bytesPerElement="integer",
							   endian="character"))
					   

#' This class represents the geometry of a brain image
#' @slot Dim the grid dimensions of the image
#' @slot origin the coordinates of the spatial origin
#' @slot spacing the dimensions (in mm) of the grid units (voxels)
#' @slot axes the set of named spatial axes
#' @slot trans an affine transformation matrix that moves from grid -> real world coordinates
#' @slot inverseTrans an inverse matrix that moves from real world -> grid coordinates
#' @exportClass BrainSpace
setClass("BrainSpace",
		representation=
				representation(Dim = "integer", origin = "numeric", spacing = "numeric",
                   axes="AxisSet", trans="matrix", inverseTrans="matrix"),
 
    validity = function(object) {
      Dim <- object@Dim
      if (length(Dim) < length(object@spacing)) {
        return("Dim slot must be of same length as spacing slot")
      }
      if (length(Dim) < length(object@origin)) {
        return("Dim slot must be of same length as origin slot")
      }
      if (length(Dim) < ndim(object@axes)) {
        return("Dim slot must be of same length as number of axes in AxisSet")
      }
      
      if (any(Dim) < 0) {
        return("Dim slot must contain non-negative values")
      }
    })
         

#' Base class for brain image data
#' @slot source an instance of class \code{\linkS4class{BaseSource}} to store the source of the data
#' @slot space an instance of class \code{\linkS4class{BrainSpace}} to represent the geometry of the data space
#' @exportClass BrainData
setClass("BrainData",
    representation=
			representation(source="BaseSource", 
					       space="BrainSpace"), 
	contains=c("VIRTUAL"))


#' Two-dimensional brain image
#' @exportClass BrainSlice
setClass("BrainSlice",       
	    contains=c("BrainData", "array"))

#' Three-dimensional brain image	   
#' @exportClass BrainVolume    
setClass("BrainVolume", 	
	    contains=c("BrainData"))



#' Three-dimensional brain image, backed by an \code{array}	   
#' @exportClass DenseBrainVolume  
setClass("DenseBrainVolume", 	
		contains=c("BrainVolume", "array"))

#' Three-dimensional brain image where all values are either TRUE or FALSE	   
#' @exportClass LogicalBrainVolume    
setClass("LogicalBrainVolume", 	
		contains=c("DenseBrainVolume"))


#' Three-dimensional brain image that can be used as a map between 1D grid indices and a table of values
#' Currently used in the \code{\linkS4class{SparseBrainVector}} class. 
#' @exportClass IndexLookupVolume
setClass("IndexLookupVolume", 
		representation=
				representation(space="BrainSpace", indices="integer", map="integer"),
		contains=c("BrainVolume"))


#setClass("SparseBrainVolume", 	
#		contains=c("BrainVolume", "numeric"))

#' Four-dimensional brain image	   
#' @exportClass BrainVector  
setClass("BrainVector", 
		contains=c("BrainData"))

#' Four-dimensional brain image, backed by an array   
#' @exportClass BrainVector  
setClass("DenseBrainVector", 
		contains=c("BrainVector", "array"))

#' a sparse four-dimensional brain image, backed by a \code{matrix}, where each column represents 
#' a vector spanning the fourth dimension (e.g. time)
#' @slot mask the mask defining the sparse domain
#' @slot data the matrix of series, where rows span across voxel space and columns span the fourth dimensions
#' @slot map instance of class \code{\linkS4class{IndexLookupVolume}} is used to map between spatial and index/row coordinates
#' @exportClass SparseBrainVector  
setClass("SparseBrainVector", 
		representation=representation(mask="LogicalBrainVolume",data="matrix", map="IndexLookupVolume"),
		contains=c("BrainVector")) 



setClass("TiledBrainVector", 		 
		representation=representation(cache="list", filename="character", indexList="list", mask="BrainVolume",capacity="numeric"),		
	    contains=c("BrainVector"))


setClass("ROIVolume", 
		representation=representation(data="numeric", coords="matrix"),
		contains=c("BrainData"))

#' a four-dimensional image this conists of a sequence of labelled image volumes backed by a list
#' @slot source the data source for the bucket volumes
#' @slot labels the names of the sub-volumes contained in the bucket
#' @slot data a list of \code{\linkS4class{BrainVolume}} instances with names corresponding to volume labels
#' @exportClass BrainBucket  
setClass("BrainBucket", 
		representation=representation(labels="character"),
		contains=c("BrainVector"))
				

setClassUnion(name="index", members =  c("numeric", "logical", "character"))

