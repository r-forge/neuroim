

IndexLookupVolume <- function(space, indices) {
  
  new("IndexLookupVolume", space=space, indices=indices)

}

setMethod("initialize", "IndexLookupVolume",
          function(.Object, space, indices) {
            
            #print("initializing IndexLookupVolume")
            .Object@space <- space
            .Object@indices <- as.integer(indices)
            nels <- prod(dim(space)[1:3])
            map <- integer(nels)
            map[indices] <- 1:length(indices)
            .Object@map <- as.integer(map)
            .Object
          })


setMethod("indices", signature(x="IndexLookupVolume"),
          function(x) {
            x@indices
          })

setMethod("lookup", signature(x="IndexLookupVolume", i="numeric"),
          function(x,i) {
            x@map[i]
          })

setMethod("space", signature(x="IndexLookupVolume"),
          function(x) {
            x@space
          })
        
          
setMethod("coords", signature(x="IndexLookupVolume"),
          function(x,i) {
            #browser()
            idx <- lookup(x,i)
            idx <- idx[idx!=0]
            if (length(idx) == 0) {
              return(NA)
            }
            
            indexToGrid(space(x), idx)
            
          })            


  
