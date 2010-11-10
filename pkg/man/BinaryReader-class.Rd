\name{BinaryReader-class}
\alias{BinaryReader-class}
\alias{BinaryReader}
\title{This class supports reading of bulk binary data from a connection...}
\description{This class supports reading of bulk binary data from a connection}
\section{Slots}{\describe{\item{\code{input}:}{(\code{\link{connection}}) the binary input connection}\item{\code{byteOffset}:}{(\code{\link{numeric}}) the number of bytes to skip at the start of input}\item{\code{dataType}:}{(\code{\link{character}}) the dataType of the binary Elements}\item{\code{bytesPerElement}:}{(\code{\link{integer}}) number of bytes in each data element (e.g. 4 or 8 for floating point numbers)}\item{\code{endian}:}{(\code{\link{character}}) endianness of binary input connection}}}
\section{Methods}{\describe{\item{\code{\link[=initialize,BinaryReader-method]{initialize}}}{}\item{\code{\link[=readElements,BinaryReader,numeric-method]{readElements}}}{}\item{\code{\link[=close,BinaryReader-method]{close}}}{}}}
\alias{BinaryReader}
\usage{BinaryReader(input, byteOffset, dataType, bytesPerElement, endian=.Platform$endian)
}
\details{\code{BinaryReader}: Constructor for  \code{\linkS4class{BinaryReader}} class

}
\arguments{\item{input}{file name to read from or else a \code{connection} object}
\item{byteOffset}{the number of bytes to skip at the start of input}
\item{dataType}{R data type of binary elements}
\item{bytesPerElement}{number of bytes in each data element (e.g. 4 or 8 for floating point numbers)}
\item{endian}{endianness of binary input connection}
}

