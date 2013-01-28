#' display BrainVolume
#' 
#' @export
#' @rdname image-methods
setMethod(f="image", signature=signature(x = "BrainVolume"),
          def=function(x, slice, col=heat.colors(128, alpha = 1), zero.col = "#00000000") {    
            imslice <- t(x[,,slice])
            vrange <- range(imslice)
            imcols <- col[(imslice - vrange[1])/diff(vrange) * (length(col) -1) + 1]
            dim(imcols) <- dim(imslice)
            ras <- as.raster(imcols)
            ras[imslice == 0] <- zero.col
            
            grid.newpage()
            grid.raster(ras)
          })
