\name{FileMetaInfo-class}
\alias{FileMetaInfo-class}
\alias{FileMetaInfo}
\title{This class contains meta information from an image data file...}
\description{This class contains meta information from an image data file}
\section{Slots}{\describe{\item{\code{headerFile}:}{(\code{\link{character}}) name of the file containing meta information}\item{\code{dataFile}:}{(\code{\link{character}}) name of the file containing data}\item{\code{fileDescriptor}:}{(\code{\link{BrainFileDescriptor}}) descriptor of image file format}\item{\code{endian}:}{(\code{\link{character}}) byte order of data ('little' or 'big')}\item{\code{dataOffset}:}{(\code{\link{numeric}}) the number of bytes preceding the start of image data in data file}\item{\code{bytesPerElement}:}{(\code{\link{integer}}) number of bytes per element}\item{\code{intercept}:}{(\code{\link{numeric}}) constant value added to image -- multiple values allowed (must equal numer of sub-images)}\item{\code{slope}:}{(\code{\link{numeric}}) image multiplier -- multiple values allowed (must equal numer of sub-images)}\item{\code{header}:}{(\code{\link{list}}) a list of format specific attributes}}}
\section{Extends}{\code{\linkS4class{BrainMetaInfo}}}
\section{Methods}{\describe{\item{\code{\link[=dim,FileMetaInfo-method]{dim}}}{}\item{\code{\link[=show,FileMetaInfo-method]{show}}}{}}}

