\name{BinaryWriter-class}
\alias{BinaryWriter-class}
\alias{BinaryWriter}
\title{BinaryWriter...}
\description{BinaryWriter}
\details{\code{BinaryWriter-class}: This class supports writing of bulk binary data to a connection

\code{BinaryWriter}: Constructor for  \code{\linkS4class{BinaryWriter}} class


}
\section{Slots}{\describe{\item{\code{output}:}{(\code{\link{connection}}) the binary output connection}\item{\code{byteOffset}:}{(\code{\link{numeric}}) the number of bytes to skip at the start of input}\item{\code{dataType}:}{(\code{\link{character}}) the dataType of the binary Elements}\item{\code{bytesPerElement}:}{(\code{\link{integer}}) number of bytes in each data element (e.g. 4 or 8 for floating point numbers)}\item{\code{endian}:}{(\code{\link{character}}) endianness of binary output connection}}}
\section{Methods}{\describe{\item{\code{\link[=initialize,BinaryWriter-method]{initialize}}}{}\item{\code{\link[=writeElements,BinaryWriter,numeric-method]{writeElements}}}{}\item{\code{\link[=close,BinaryWriter-method]{close}}}{}}}
\alias{BinaryWriter}
\usage{BinaryWriter(output, byteOffset, dataType, bytesPerElement, endian=.Platform$endian)
}
\arguments{\item{output}{file name to write to or else a \code{connection} object}
\item{byteOffset}{the number of bytes to skip at the start of output}
\item{dataType}{R data type of binary elements}
\item{bytesPerElement}{number of bytes in each data element (e.g. 4 or 8 for floating point numbers)}
\item{endian}{endianness of binary output connection}
}

