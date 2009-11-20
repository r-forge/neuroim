setMethod("numdim", signature(x = "BrainData"),
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
setMethod("as.vector", signature(x = "BrainData"), function(x) as(x, "vector"))
