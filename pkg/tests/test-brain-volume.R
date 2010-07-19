
library(testthat)

#library_if_available(package)


#context("BrainVolume")

test_that("can load a BrainVolume from an unzipped 3D NIfTI file", {			
			vol <- loadVolume("../data/global_mask.nii")
			expect_that(dim(vol), equals(c(64,64,25)))			
		})


