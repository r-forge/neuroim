\name{BinaryWriter-class}
\alias{BinaryWriter}
\alias{BinaryWriter-class}
\title{BinaryWriter}
\usage{
  BinaryWriter(output, byteOffset, dataType,
    bytesPerElement, endian = .Platform$endian)
}
\arguments{
  \item{output}{file name to write to or else a
  \code{connection} object}

  \item{byteOffset}{the number of bytes to skip at the
  start of output}

  \item{dataType}{R data type of binary elements}

  \item{bytesPerElement}{number of bytes in each data
  element (e.g. 4 or 8 for floating point numbers)}

  \item{endian}{endianness of binary output connection}
}
\description{
  This class supports writing of bulk binary data to a
  connection

  Constructor for \code{\linkS4class{BinaryWriter}} class
}

