\name{BrainMetaInfo-class}
\alias{BrainMetaInfo-class}
\alias{BrainMetaInfo}
\title{This class contains meta information from an image data file...}
\description{This class contains meta information from an image data file}
\section{Slots}{\describe{\item{\code{headerFile}:}{(\code{\link{character}}) name of the file containing meta information}\item{\code{dataFile}:}{(\code{\link{character}}) name of the file containing data}\item{\code{fileDescriptor}:}{(\code{\link{BrainFileDescriptor}}) descriptor of image file format}\item{\code{compression}:}{(\code{\link{character}}) type of image compression ('none" or 'gz', currently)}\item{\code{endian}:}{(\code{\link{character}}) byte order of data ('little' or 'big')}\item{\code{dataOffset}:}{(\code{\link{numeric}}) the number of bytes preceding the start of image data in data file}\item{\code{dataType}:}{(\code{\link{character}}) can be one of BYTE, SHORT, INT, FLOAT, DOUBLE}\item{\code{dim}:}{(\code{\link{numeric}}) image dimensions}\item{\code{spacing}:}{(\code{\link{numeric}}) voxel dimensions}}}
\section{Extends}{\code{\linkS4class{VIRTUAL}}}

