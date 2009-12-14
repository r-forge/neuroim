





#### generics related to BrainSpace
setGeneric("numdim",     function(x) standardGeneric("numdim"))
#setGeneric("length",     function(x) standardGeneric("length"))
setGeneric("spacing",     function(x) standardGeneric("spacing"))
setGeneric("bounds",     function(x) standardGeneric("bounds"))
setGeneric("orientation",  function(x) standardGeneric("orientation"))
setGeneric("origin", function(x) standardGeneric("origin"))
setGeneric("trans",  function(x) standardGeneric("trans"))
setGeneric("invTrans", function(x) standardGeneric("invTrans"))
setGeneric("trans<-", function(x, value) standardGeneric("trans<-"))
setGeneric("reptime", function(x) standardGeneric("reptime"))

#### generics related to BrainVolume class
setGeneric("writeVolume",       function(x, fileName, format) standardGeneric("writeVolume"))
setGeneric("writeVector",       function(x, fileName, format) standardGeneric("writeVector"))
setGeneric("takeVolume",       function(x, i) standardGeneric("takeVolume"))



#### generics related to BrainData class
setGeneric("value",       function(object, x,y, ...) standardGeneric("value"))
setGeneric("space",       function(x) standardGeneric("space"))
setGeneric("indexToGrid",   function(x, idx) standardGeneric("indexToGrid"))
setGeneric("gridToIndex",   function(x, coords) standardGeneric("gridToIndex"))

setGeneric("sliceMeans", function(x) standardGeneric("sliceMeans"))
setGeneric("pick", function(x, mask, ...) standardGeneric("pick"))

setGeneric("coords", function(x, ...) standardGeneric("coords"))
setGeneric("indices", function(x) standardGeneric("indices"))
setGeneric("lookup", function(x, i, ...) standardGeneric("lookup"))
setGeneric("series", function(x, i, ...) standardGeneric("series"))          
setGeneric("concat", function(x,y, ...) standardGeneric("concat"))
setGeneric("eachSeries", function(x, FUN, ...) standardGeneric("eachSeries"))
setGeneric("connComp", function(x, ...) standardGeneric("connComp"))

#### generics related to BrainHeader class
setGeneric("headerFile",     function(x) standardGeneric("headerFile"))
setGeneric("dataFile",     function(x) standardGeneric("dataFile"))
setGeneric("fileType",  function(x) standardGeneric("fileType"))
setGeneric("versionNum",  function(x) standardGeneric("versionNum"))
setGeneric("encoding",  function(x) standardGeneric("encoding"))
setGeneric("endian", function(x) standardGeneric("endian"))
setGeneric("dataOffset", function(x) standardGeneric("dataOffset"))
setGeneric("extraInfo", function(x) standardGeneric("extraInfo"))
setGeneric("dataType", function(x) standardGeneric("dataType"))
setGeneric("dataDim",function(x) standardGeneric("dataDim"))
setGeneric("createSpace", function(x) standardGeneric("createSpace"))

####

setGeneric("readData",function(x, indices) standardGeneric("readData"))
setGeneric("readHeader",function(x) standardGeneric("readHeader"))
setGeneric("writeData", function(x, header, data) standardGeneric("writeData"))
setGeneric("writeHeader", function(x, header) standardGeneric("writeHeader"))
setGeneric("fileName", function(x) standardGeneric("fileName"))
setGeneric("path", function(x) standardGeneric("path"))
setGeneric("openFor", function(x) standardGeneric("openFor"))
setGeneric("headerFileExt", function(x) standardGeneric("headerFileExt"))
setGeneric("dataFileExt", function(x) standardGeneric("dataFileExt"))

