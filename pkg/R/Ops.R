#' @include AllClass.R
roxygen()
#' @include AllGeneric.R
roxygen()
#' @include BrainVector.R
roxygen()
#' @include BrainVolume.R
roxygen()



#' @nord Arith-methods
setMethod(f="Arith", signature=signature(e1="BrainVolume", e2="BrainVolume"),
          def=function(e1, e2) {
            if (!all(dim(e1) == dim(e2))) {
              stop("cannot perform operation on argument with different dimensions")
            }
            
            ret <- callGeneric(e1@.Data,e2@.Data)
            bv <- DenseBrainVolume(ret, space(e1))
     
          })

#' @nord Arith-methods
setMethod(f="Arith", signature=signature(e1="BrainVector", e2="BrainVector"),
          def=function(e1, e2) {
            if (!all(dim(e1) == dim(e2))) {
              stop("cannot perform operation on argument with different dimensions")
            }
            
            ret <- callGeneric(e1@.Data,e2@.Data)
            bv <- DenseBrainVector(ret, space(e1))
     
          })
 
#' @nord Arith-methods
 setMethod(f="Arith", signature=signature(e1="BrainVector", e2="BrainVolume"),
		  def=function(e1, e2) {
			  if (!all(dim(e1)[1:3] == dim(e2))) {
				  stop("cannot perform operation on argument with different dimensions")
			  }
			  
			  ret <- eachVolume(e1, function(vol) vol + e2)
			  do.call("concat", ret)
		  
		  })

#setMethod("sum", signature()
          
