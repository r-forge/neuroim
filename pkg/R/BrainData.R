#' @include AllClass.R
{}
#' @include AllGeneric.R
{}


#' ndim
#' 
#' @rdname ndim-methods
setMethod(f="ndim", signature=signature(x = "BrainData"),
          def=function(x) numdim(x@space))

  
#' dim
#' 
#' @nord
setMethod(f="dim", signature=signature(x = "BrainData"),
          def=function(x) dim(x@space))

  
#' space
#' 
#' @rdname space-methods
setMethod(f="space", signature=signature(x = "BrainData"),
          def=function(x) x@space)

  
#' spacing
#' 
#' @rdname spacing-methods
setMethod(f="spacing",signature= signature(x = "BrainData"),
          def=function(x) {
            sp <- space(x)
            spacing(sp)
          })

#' @nord 
setMethod(f="as.matrix", signature=signature(x = "BrainData"), def=function(x) as(x, "matrix"))

#' @nord 
setMethod(f="as.array", signature=signature(x = "BrainData"), def=function(x) as(x, "array"))

#' @nord 
setMethod(f="as.vector", signature=signature(x = "BrainData"), def=function(x) as(x, "vector"))
