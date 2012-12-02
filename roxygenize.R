#! /usr/bin/env Rscript

library(methods)


options(useFancyQuotes = FALSE)
library(roxygen2)
roxygenize("pkg",
 roxygen.dir="pkg",
 copy.package=FALSE,
 unlink.target=FALSE,
 overwrite=TRUE)
 #use.Rd2=TRUE)

library(roxygen3)
roxygenise("pkg")
 #use.Rd2=TRUE)

#setwd("pkg/man")

#lapply(list.files(".", ".*Rd"), function(fname) {
#	system(paste("sed s/[”“]/'\"'/g", fname, " > tmp"))
#	system(paste("mv tmp", fname))
#})

#setwd("../..")