\name{BinaryWriter}
\alias{BinaryWriter}
\title{Constructor for  \linkS4class{BinaryWriter} class...}
\usage{BinaryWriter(output, byteOffset, dataType, bytesPerElement, endian=.Platform$endian)
}
\description{Constructor for  \code{\linkS4class{BinaryWriter}} class}
\arguments{\item{output}{file name to write to or else a \code{connection} object}
\item{dataType}{R data type of binary elements}
\item{bytesPerElement}{number of bytes in each data element (e.g. 4 or 8 for floating point numbers)}
\item{endian}{endianness of binary output connection}
}

