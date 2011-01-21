# TODO: Add comment
# 
# Author: brad
###############################################################################


test.DenseBrainVector <- function() {
	dat <- array(0, c(64,64,64,4))
	spc <- BrainSpace(c(64,64,64,4))
	bv <- DenseBrainVector(dat, spc)
	checkTrue(!is.null(bv))
	checkEquals(bv[1,1,1,1], 0)
	checkEquals(bv[64,64,64,4], 0)
	checkEquals(dim(bv), c(64,64,64,4))
}

test.DenseBrainVector.concat <- function() {
	dat <- array(0, c(64,64,64,4))
	spc <- BrainSpace(c(64,64,64,4))
	bv1 <- DenseBrainVector(dat, spc)
	bv2 <- DenseBrainVector(dat, spc)
	
	bv3 <- concat(bv1, bv2)
	checkTrue(inherits(bv3, "BrainVector"))
	checkEquals(dim(bv3), c(64,64,64,8))
	
	bv4 <- concat(bv1,bv2, bv1, bv3)
	checkTrue(inherits(bv4, "BrainVector"))
	checkEquals(dim(bv4), c(64,64,64,20))
	checkEquals(bv4[1,1,1,1],0)
	
}

test.DenseBrainVector.takeVolume <- function() {
	dat <- array(0, c(64,64,64,4))
	spc <- BrainSpace(c(64,64,64,4))
	bv1 <- DenseBrainVector(dat, spc)
	bv2 <- DenseBrainVector(dat, spc)
	
	bv3 <- concat(bv1, bv2)
	
	vol1 <- takeVolume(bv3, 1)
	checkEquals(dim(vol1), c(64,64,64))
	
	vec1 <- takeVolume(bv3, 1:2)
	checkTrue(inherits(vec1, "list"))
	checkEquals(dim(vec1[[1]]), c(64,64,64))
	
	vec2 <- takeVolume(bv3, 1:2, merge=TRUE)
	checkTrue(inherits(vec2, "BrainVector"))
	checkEquals(dim(vec2), c(64,64,64,2))
	
}

test.SparseBrainVector.takeVolume <- function() {
	dat <- array(0, c(64,64,64,4))
	spc <- BrainSpace(c(64,64,64,4))
	bv1 <- DenseBrainVector(dat, spc)
	bv2 <- DenseBrainVector(dat, spc)
	
	bv3 <- concat(bv1, bv2)
	
	vol1 <- takeVolume(bv3, 1)
	checkEquals(dim(vol1), c(64,64,64))
	
	vec1 <- takeVolume(bv3, 1:2)
	checkTrue(inherits(vec1, "list"))
	checkEquals(dim(vec1[[1]]), c(64,64,64))
	
	vec2 <- takeVolume(bv3, 1:2, merge=TRUE)
	checkTrue(inherits(vec2, "BrainVector"))
	checkEquals(dim(vec2), c(64,64,64,2))
	
}



test.BrainVector.eachVolume <- function() {
	dat <- array(rnorm(64*64*64*4), c(64,64,64,4))
	spc <- BrainSpace(c(64,64,64,4))
	bv1 <- DenseBrainVector(dat, spc)
	
	mean.vol1 <- eachVolume(bv1, mean)
	checkEquals(length(mean.vol1), 4)
	
	mean.vol2 <- eachVolume(bv1, withIndex=TRUE, FUN=function(slice, i) mean(slice))
	checkEquals(length(mean.vol2), 4)
	checkEquals(mean.vol1, mean.vol2)
	
	checkEquals(unlist(mean.vol1), apply(bv1, 4, mean))
}

test.BrainVector.series <- function() {
	dat <- array(rnorm(64*64*64*4), c(64,64,64,4))
	spc <- BrainSpace(c(64,64,64,4))
	bv1 <- DenseBrainVector(dat, spc)
	checkEquals(series(bv1, 1,1,1), bv1[1,1,1,])
	
    mat <- rbind(c(1,1,1), c(2,2,2), c(3,3,3))
	
	r1 <- apply(mat, 1, function(i) { series(bv1, i[1], i[2], i[3]) })
			
	r2 <- series(bv1, mat)
	
	checkEquals(r1, r2)
	
	
}


test.BrainVector.as.matrix <- function() {
	dat <- array(rnorm(64*64*64*4), c(64,64,64,4))
	spc <- BrainSpace(c(64,64,64,4))
	bv1 <- DenseBrainVector(dat, spc)
	mat <- as.matrix(bv1)
	
	ind <- 1:(64*64*64)
	mat2 <- t(do.call(cbind, lapply(ind, function(i) series(bv1, i))))
	
	checkEquals(mat, mat2)
	
}

test.SparseBrainVector.as.sparse <- function() {
	dat <- array(rnorm(64*64*64*4), c(64,64,64,4))
	spc <- BrainSpace(c(64,64,64,4))
	bv1 <- DenseBrainVector(dat, spc)
	svec <- as.sparse(bv1, c(1,100,1000))
	checkEquals(dim(svec), c(64,64,64,4))
	
	
}

test.SparseBrainVector <- function() {
	dat <- array(rnorm(64*64*64*4), c(64,64,64,4))
	spc <- BrainSpace(c(64,64,64,4))
	tmp <- rnorm(64*64*64)
	mask <- tmp > .8
	mask <- LogicalBrainVolume(mask, dropDim(spc))
	bvec <- SparseBrainVector(dat, spc, mask)
	
	checkEquals(dim(bvec), dim(dat))
	checkEquals(dat[1,1,1,1], bvec[1,1,1,1])
	
}

test.SparseBrainVector.concat <- function() {
	dat <- array(0, c(64,64,64,4))
	spc <- BrainSpace(c(64,64,64,4))
	tmp <- rnorm(64*64*64)
	mask <- tmp > .8
	mask <- LogicalBrainVolume(mask, dropDim(spc))
	
	bv1 <- SparseBrainVector(dat, spc, mask)
	
	bv2 <- concat(bv1, bv1)
	checkTrue(inherits(bv2, "BrainVector"))
	checkEquals(dim(bv2), c(64,64,64,8))
	
	bv3 <- concat(bv1,bv2, bv1, bv2)
	checkTrue(inherits(bv3, "BrainVector"))
	checkEquals(dim(bv3), c(64,64,64,24))
	#checkEquals(bv4[1,1,1,1],0)
}
	


	


