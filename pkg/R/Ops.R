

setMethod("Arith", signature(e1="BrainVolume", e2="BrainVolume"),
          function(e1, e2) {
            if (!all(dim(e1) == dim(e2))) {
              stop("cannot perform operation on argument with different dimensions")
            }
            
            ret <- callGeneric(e1@.Data,e2@.Data)
            bv <- BrainVolume(ret, space(e1))
     
          })
          
setMethod("Arith", signature(e1="BrainVector", e2="BrainVector"),
          function(e1, e2) {
            if (!all(dim(e1) == dim(e2))) {
              stop("cannot perform operation on argument with different dimensions")
            }
            
            ret <- callGeneric(e1@.Data,e2@.Data)
            bv <- BrainVector(ret, space(e1))
     
          })

#setMethod("sum", signature()
          
