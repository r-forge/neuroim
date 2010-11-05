\name{BinaryWriter-class}
\alias{BinaryWriter-class}
\alias{BinaryWriter}
\title{This class supports writing of bulk binary data to a connection...}
\description{This class supports writing of bulk binary data to a connection}
\section{Slots}{\describe{\item{\code{output}:}{(\code{\link{connection}}) the binary output connection}\item{\code{byteOffset}:}{(\code{\link{numeric}}) the number of bytes to skip at the start of input}\item{\code{dataType}:}{(\code{\link{character}}) the dataType of the binary Elements}\item{\code{bytesPerElement}:}{(\code{\link{integer}}) number of bytes in each data element (e.g. 4 or 8 for floating point numbers)}\item{\code{endian}:}{(\code{\link{character}}) endianness of binary output connection}}}
\section{Methods}{\describe{\item{\code{\link[=initialize,BinaryWriter-method]{initialize}}}{}\item{\code{\link[=writeElements,BinaryWriter,numeric-method]{writeElements}}}{}\item{\code{\link[=close,BinaryWriter-method]{close}}}{}}}

