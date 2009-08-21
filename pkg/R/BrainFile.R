setMethod("show", "BrainFile",
    function(object) {
        cat("Brain file\n")
        cat("  Path           :", object@path, "\n")
        cat("  Open           :", object@open, "\n")     
    }
)

setMethod("fileType", signature(x = "BrainFile"),
    function(x) {
      x@fileType
    })

setMethod("fileName", signature(x = "BrainFile"),
	function(x) {
	      res <- strsplit(x@path, split=.Platform$file.sep)  
	      fname <- res[[1]][[length(res[[1]])]]
	      stem <- strsplit(fname, split="\\.")[[1]]
	      if (length(stem) >= 2) {
	        return(stem[1])
	      }

	      stem <- paste(stem[1:length(stem)-1], collapse=".")
	      return(stem)   

	    })



setMethod("path", signature(x = "BrainFile"),
    function(x) {
      x@path
    })

setMethod("openFor", signature(x = "BrainFile"),
    function(x) {
      x@open
    })

