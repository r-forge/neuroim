\name{BrainFileDescriptor-class}
\alias{BrainFileDescriptor-class}
\alias{BrainFileDescriptor}
\title{This class represents a neuroimaging file format...}
\description{This class represents a neuroimaging file format}
\section{Slots}{\describe{\item{\code{fileFormat}:}{(\code{\link{character}}) the name of the file format (e.g. NIfTI)}\item{\code{headerEncoding}:}{(\code{\link{character}}) the file encoding of the header file (e.g. 'raw' for binary, 'gzip' for gz compressed')}\item{\code{headerExtension}:}{(\code{\link{character}}) the file extension for the header file (e.g. 'nii' for NIfTI single files)}\item{\code{dataEncoding}:}{(\code{\link{character}}) the file encoding for the data file}\item{\code{dataExtension}:}{(\code{\link{character}}) the file extension for the data file (e.g. 'nii' for NIfTI single files)}}}
\section{Extends}{\code{\linkS4class{Base}}}
\section{Methods}{\describe{\item{\code{\link[=fileMatches,BrainFileDescriptor,character-method]{fileMatches}}}{}\item{\code{\link[=headerFileMatches,BrainFileDescriptor,character-method]{headerFileMatches}}}{}\item{\code{\link[=dataFileMatches,BrainFileDescriptor,character-method]{dataFileMatches}}}{}\item{\code{\link[=headerFile,BrainFileDescriptor,character-method]{headerFile}}}{}\item{\code{\link[=dataFile,BrainFileDescriptor,character-method]{dataFile}}}{}\item{\code{\link[=stripExtension,BrainFileDescriptor,character-method]{stripExtension}}}{}}}

