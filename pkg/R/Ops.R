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
            
			if (inherits(e1, "DenseBrainVector") && inherits(e2, "DenseBrainVector")) {
            	ret <- callGeneric(e1@.Data,e2@.Data)
            	DenseBrainVector(ret, space(e1))
			} else {
				D4 <- dim(e1)[4]		  
				vols <- list()
				for (i in 1:D4) {
					vols[[i]] <- callGeneric(takeVolume(e1,i), takeVolume(e2,i))
				}
				
				mat <- do.call(cbind, vols)
				dspace <- addDim(space(vols[[1]]), length(vols))	
				DenseBrainVector(mat, dspace)
				
			}
     
          })
 
#' @nord Arith-methods
 setMethod(f="Arith", signature=signature(e1="BrainVector", e2="BrainVolume"),
		  def=function(e1, e2) {
			  if (!all(dim(e1)[1:3] == dim(e2))) {
				  stop("cannot perform operation on argument with different dimensions")
			  }
			  
			  D4 <- dim(e1)[4]	
			  vols <- list()
			  for (i in 1:D4) {
				  vols[[i]] <- callGeneric(takeVolume(e1,i), e2)
			  }
			  
			  mat <- do.call(cbind, vols)
			  dspace <- addDim(space(vols[[1]]), length(vols))	
			  DenseBrainVector(mat, dspace)
			  
		  
		  })
  

#setMethod("sum", signature()
          
