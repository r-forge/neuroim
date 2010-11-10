#' IndexLookupVolume
#' 
#' @param space a BrainSpace object
#' @param indices the set of 1-d indices defining the lookup map
#' @export IndexLookupVolume
#' @rdname IndexLookupVolume-class
IndexLookupVolume <- function(space, indices) { 
  new("IndexLookupVolume", space=space, indices=indices)
}

#' @rdname IndexLookupVolume-class
setMethod(f="initialize", signature=signature("IndexLookupVolume"),
          def=function(.Object, space, indices) {
            
            #print("initializing IndexLookupVolume")
            .Object@space <- space
            .Object@indices <- as.integer(indices)
            nels <- prod(dim(space)[1:3])
            map <- integer(nels)
            map[indices] <- 1:length(indices)
            .Object@map <- as.integer(map)
            .Object
          })

#' @exportMethod indices
#' @rdname indices-methods
setMethod(f="indices", signature=signature(x="IndexLookupVolume"),
          def=function(x) {
            x@indices
          })
  
#' @exportMethod lookup
#' @rdname lookup-methods
setMethod(f="lookup", signature=signature(x="IndexLookupVolume", i="numeric"),
          def=function(x,i) {
            x@map[i]
          })

#' @exportMethod space
#' @rdname space-methods
setMethod(f="space", signature=signature(x="IndexLookupVolume"),
          def=function(x) {
            x@space
          })
        
#' @exportMethod coords   
#' @rdname coords-methods       
setMethod(f="coords", signature(x="IndexLookupVolume"),
          def=function(x,i) {
            idx <- lookup(x,i)
            idx <- idx[idx!=0]
            if (length(idx) == 0) {
              return(NA)
            }
            
            indexToGrid(space(x), idx)
            
          })            


  
