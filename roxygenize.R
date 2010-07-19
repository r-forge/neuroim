#! /usr/bin/env Rscript

library(methods)



library(roxygen)
roxygenize("pkg",
 roxygen.dir="pkg",
 copy.package=FALSE,
 unlink.target=FALSE,
 use.Rd2=TRUE)