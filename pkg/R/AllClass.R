
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


setClass("NIFTIHeader", contains="BrainHeader")

setClass("ANALYZEHeader", contains="BrainHeader")


setClass("BrainFile",   representation(path="character", open="character", "VIRTUAL"))

setClass("ANALYZEFile", representation(fileType="character"), contains="BrainFile")

setClass("NIFTIFile",   representation(fileType="character"), contains="BrainFile")

setClass("AFNIFile",    representation(fileType="character"), contains="BrainFile")



setClass("BrainSpace",
    representation(Dim = "integer", origin = "numeric", spacing = "numeric",
                   orientation="character", trans="matrix", invTrans="matrix", reptime="numeric"),
    prototype(Dim = as.integer(c(1,1,1)), origin=c(0,0,0), spacing=c(0,0,0),
              orientation=c("L", "P", "I"), trans=diag(1, 4), invTrans=diag(1,4),reptime=1),
    validity = function(object) {
      Dim <- object@Dim
      if (length(Dim) < length(object@spacing)) {
        return("Dim slot must be of same length as spacing slot")
      }
      if (length(Dim) < length(object@origin)) {
        return("Dim slot must be of same length as origin slot")
      }
      #if (length(Dim) < length(object@orientation)) {
      #  return("Dim slot must be of same length as orientation slot")
      #}
      
      if (any(Dim) < 0) {
        return("Dim slot must contain non-negative values")
      }
    })
         

#setClass("BrainVectorSpace",
#         contains="BrainSpace",
#         representation(reptime=1))

setClass("BrainData",
    representation(space="BrainSpace", "VIRTUAL"))

setClass("BrainList", contains=c("list", "BrainData"), representation(names="character"))

setClass("BrainSlice",
         contains=c("BrainData", "array"),
         prototype=prototype(space=new("BrainSpace", Dim=as.integer(c(1,1)),
                               origin=c(0,0), spacing=c(1,1))))
         
setClass("BrainVolume", contains="BrainSlice",
         prototype=prototype(space=new("BrainSpace", Dim=as.integer(c(1,1,1)), origin=c(0,0,0),
                               spacing=c(1,1,1))))

setClass("IndexLookupVolume", representation(space="BrainSpace", indices="integer", map="integer"))


setClass("BrainVector", contains="BrainVolume")

setClass("SparseBrainVector", contains="BrainData", representation(mask="BrainVolume",data="matrix", map="IndexLookupVolume"))

setClass("TiledBrainVector", contains="BrainData", representation(cache="list", filename="character", indexList="list", mask="BrainVolume",
                               capacity="numeric"))

#setClass("BrainRegion", contains="BrainData")

setClass("BrainRegion3D", contains="BrainData", representation(data="numeric", coords="matrix"))




setClassUnion("index", members =  c("numeric", "logical", "character"))

