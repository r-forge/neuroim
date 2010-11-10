\name{AFNIMetaInfo-class}
\alias{AFNIMetaInfo-class}
\alias{AFNIMetaInfo}
\title{This class contains meta information for a AFNI image file...}
\description{This class contains meta information for a AFNI image file}
\section{Slots}{\describe{\item{\code{afni_header}:}{}}}
\section{Extends}{\code{\linkS4class{FileMetaInfo}}}
\section{Methods}{\describe{\item{\code{\link[=dataReader,AFNIMetaInfo-method]{dataReader}}}{}}}
\alias{AFNIMetaInfo}
\usage{AFNIMetaInfo(descriptor, afni_header)
}
\details{\code{AFNIMetaInfo}: Constructor for \code{\linkS4class{AFNIMetaInfo}} class

}
\value{\code{AFNIMetaInfo}: an instance of class \code{\linkS4class{AFNIMetaInfo}}

}
\arguments{\item{descriptor}{an instance of class \code{\linkS4class{AFNIFileDescriptor}}}
\item{afni_header}{a \code{list} returned by \code{readAFNIHeader}}
}

