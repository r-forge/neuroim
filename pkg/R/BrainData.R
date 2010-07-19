

setMethod(f="ndim", signature=signature(x = "BrainData"),
          function(x) numdim(x@space))

setMethod(f="dim", signature=signature(x = "BrainData"),
          function(x) dim(x@space))

setMethod(f="space", signature=signature(x = "BrainData"),
          function(x) x@space)

setMethod(f="spacing",signature= signature(x = "BrainData"),
          function(x) {
            sp <- space(x)
            spacing(sp)
          })
          
setMethod(f="as.matrix", signature=signature(x = "BrainData"), def=function(x) as(x, "matrix"))
setMethod(f="as.array", signature=signature(x = "BrainData"), def=function(x) as(x, "array"))
setMethod(f="as.vector", signature=signature(x = "BrainData"), def=function(x) as(x, "vector"))
