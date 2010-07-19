#! /usr/bin/env Rscript

library(RUnit)
source("/Users/brad/Rcode/neuroim/pkg/workspace.R")


testsuite <- defineTestSuite("neuroim", dirs = "tests",
	testFileRegexp = "^runit.+\\.R",
	testFuncRegexp = "^test.+",
	rngKind = "Marsaglia-Multicarry",
	rngNormalKind = "Kinderman-Ramage")

testResult <- runTestSuite(testsuite)
printTextProtocol(testResult)
