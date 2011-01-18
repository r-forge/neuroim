\name{NIfTIMetaInfo-class}
\alias{NIfTIMetaInfo-class}
\alias{NIfTIMetaInfo}
\title{This class contains meta information for a NIfTI image file...}
\description{This class contains meta information for a NIfTI image file}
\section{Slots}{\describe{\item{\code{nifti_header}:}{}}}
\section{Extends}{\code{\linkS4class{FileMetaInfo}}}
\section{Methods}{\describe{\item{\code{\link[=dataReader,NIfTIMetaInfo-method]{dataReader}}}{}}}
\alias{NIfTIMetaInfo}
\usage{NIfTIMetaInfo(descriptor, nifti_header)
}
\value{\code{NIfTIMetaInfo}: an instance of class \code{\linkS4class{NIfTIMetaInfo}}

}
\arguments{\item{descriptor}{an instance of class \code{\linkS4class{NIfTIFileDescriptor}}}
\item{nifti_header}{a \code{list} returned by \code{readNIftiHeader}}
}

