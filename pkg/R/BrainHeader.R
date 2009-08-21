setClass("BrainHeader",
         representation(headerFile="character",
                        dataFile="character",
                        fileType="character",
                        versionNum="character",
                        encoding="character",
                        endian="character",
                        dataOffset="numeric",
                        dataType="character",
                        extraInfo="list"),
         contains="VIRTUAL")




setMethod("headerFile", "BrainHeader", function(x) x@headerFile)
setMethod("dataFile",   "BrainHeader", function(x) x@dataFile)
setMethod("fileType",   "BrainHeader", function(x) x@fileType)
setMethod("versionNum", "BrainHeader", function(x) x@versionNum)
setMethod("encoding",   "BrainHeader", function(x) x@encoding)
setMethod("endian",     "BrainHeader", function(x) x@endian)
setMethod("dataOffset", "BrainHeader", function(x) x@dataOffset)
setMethod("dataType",   "BrainHeader", function(x) x@dataType)
setMethod("extraInfo",   "BrainHeader", function(x) x@extraInfo)

