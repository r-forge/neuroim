
#' @rdname ndim-methods
setMethod(f="ndim", signature=signature(x = "BrainData"),
          def=function(x) numdim(x@space))

#' @rdname dim-methods
setMethod(f="dim", signature=signature(x = "BrainData"),
          def=function(x) dim(x@space))

#' @rdname space-methods
setMethod(f="space", signature=signature(x = "BrainData"),
          def=function(x) x@space)

#' @rdname spacing-methods
setMethod(f="spacing",signature= signature(x = "BrainData"),
          def=function(x) {
            sp <- space(x)
            spacing(sp)
          })

#' @rdname as.matrix-methods
setMethod(f="as.matrix", signature=signature(x = "BrainData"), def=function(x) as(x, "matrix"))

#' @rdname as.array-methods
setMethod(f="as.array", signature=signature(x = "BrainData"), def=function(x) as(x, "array"))

#' @rdname as.vector-methods
setMethod(f="as.vector", signature=signature(x = "BrainData"), def=function(x) as(x, "vector"))
