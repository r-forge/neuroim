\name{NIFTIHeader}
\alias{NIFTIHeader}

\title{ function to create a an instance of the "NIFTIHeader" class }
\description{
  function to creatte a n instance of the "NIFTIHeader" class
}
\usage{
NIFTIHeader(headerFile, dataFile, extraInfo, fileType = "NIFTI", versionNum = "1", encoding = extraInfo$encoding, endian = .Platform$endian, dataOffset = 348, dataType = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{headerFile}{ the path to the header file }
  \item{dataFile}{ the path to the data file (may be same as above) }
  \item{extraInfo}{ list containing auxiallary header information  }
  \item{fileType}{ the file format }
  \item{versionNum}{ the version of the file format }
  \item{encoding}{ the encoding type ("binary", or "gz") }
  \item{endian}{ the endianess of the stored image data }
  \item{dataOffset}{ the byte offset in the image file where the first value
    may be found }
  \item{dataType}{ the stored dataType }
  
}

\value{
an instance of the "NIFTIHeader" class
}
\examples{}
\keyword{ manip }

