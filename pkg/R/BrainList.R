
BrainList <- function(volList, labels=NULL, files=NULL) {
  if (!all(sapply(volList, is, "BrainData"))) {
    stop("object in list must derive from class BrainData")
  }

  dimList <- lapply(volList, dim)
  if (!all(unlist(lapply(2:length(dimList), function(i) dimList[[i]] == dimList[[1]])))) {
    stop("BrainList must consist of data sets with of equal dimensions")
  }

  if (is.null(labels)) {
    labels <- 1:length(volList)
  }

  vol1 <- volList[[1]]  
  exspace <- space(vol1)
  ret <- new("BrainList", .Data=volList, space=exspace, names=labels) 

  if (!is.null(files)) {
    attr(ret, "filenames") <- files
  }

  return(ret)
  
}

loadBrainList <- function(tableSpec, path=NULL) {
  labels <- tableSpec$label
  fnames <- as.character(tableSpec$file)

  if (!is.null(path)) {
    fnames <- paste(path, "/", fnames, sep="")
  }

  volList <- lapply(fnames, loadVolume)
  ret <- BrainList(volList, labels=labels, files=fnames)
}
         
  
