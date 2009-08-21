
ANALYZEHeader <- function(headerFile, dataFile, extraInfo, encoding=extraInfo$encoding, endian=.Platform$endian, dataOffset=348,
                        dataType=NULL) {

  if (is.null(dataType)) {
    dataType <- extraInfo$dataStorage
  } else {
    extraInfo$dataStorage <- dataType
  }

  
  new("ANALYZEHeader", headerFile=headerFile, dataFile=dataFile, fileType= "ANALYZE", versionNum="7.5",
      encoding=encoding, endian=endian, dataOffset=dataOffset, dataType=dataType, extraInfo=extraInfo)
}


createANALYZEHeader <- function(data, fileName, dataType="FLOAT") {
  if ("BrainData" %in% extends(class(data))) {
    info <- createANALYZEInfo(fileName, dataType, brainData=data)
    
    hdr <- ANALYZEHeader(info$headerFile, info$dataFile, info)
    return(hdr)
  } else {
    stop("data argument must inherit from class : BrainData")
  }
}
  
  
  
setMethod("dataDim",signature(x="ANALYZEHeader"),
          function(x) {
            dimarray <- extraInfo(x)$dimensions            
            lastidx <- min(which(dimarray == 1)) - 1            
            return(dimarray[1:lastidx])
          })
        
          
setMethod("createSpace",signature(x="ANALYZEHeader"),
          function(x) {
            origin <- extraInfo(x)$qoffset
            spacing <- extraInfo(x)$pixdim[2:4]
            numdim <- extraInfo(x)$numDimensions

            pixdim <- extraInfo(x)$pixdim

            ######################################
            ## !! assumes pixdim[5] is time dimension !!
            reptime <- ifelse(length(pixdim) > 4, pixdim[5], 1)
            ######################################
            
            if (numdim == 3) {
              BrainSpace(Dim=as.integer(extraInfo(x)$dimension[2:4]), origin=origin, spacing=spacing,
                        orientation=c("L", "P", "I"), trans=extraInfo(x)$qform, reptime=reptime)
            } else if (numdim == 4) {
              
               BrainSpace(Dim=as.integer(extraInfo(x)$dimension[2:5]), origin=origin, spacing=spacing,
                        orientation=c("L", "P", "I"), trans=extraInfo(x)$qform, reptime=reptime)
             } else {
               stop("number of dimensions must be 3 or 4")
             }
              
          })