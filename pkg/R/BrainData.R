

setMethod("ndim", signature(x = "BrainData"),
          function(x) numdim(x@space))

setMethod("dim", signature(x = "BrainData"),
          function(x) dim(x@space))

setMethod("space", signature(x = "BrainData"),
          function(x) x@space)

setMethod("spacing", signature(x = "BrainData"),
          function(x) {
            sp <- space(x)
            spacing(sp)
          })
          
setMethod("as.matrix", signature(x = "BrainData"), function(x) as(x, "matrix"))
setMethod("as.array", signature(x = "BrainData"), function(x) as(x, "array"))
setMethod("as.vector", signature(x = "BrainData"), function(x) as(x, "vector"))
