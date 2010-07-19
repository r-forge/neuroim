\name{BinaryReader}
\alias{BinaryReader}
\title{Constructor for  \linkS4class{BinaryReader} class...}
\usage{BinaryReader(input, byteOffset, dataType, bytesPerElement, endian=.Platform$endian)
}
\description{Constructor for  \code{\linkS4class{BinaryReader}} class}
\arguments{\item{input}{of file to read from or else a \code{connection} object}
\item{byteOffset}{the number of bytes to skip at the start of input}
\item{dataType}{R data type of binary elements}
\item{bytesPerElement}{number of bytes in each data element (e.g. 4 or 8 for floating point numbers)}
\item{endian}{endianness of binary input connection}
}

