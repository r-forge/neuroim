\name{BrainVector-class}
\docType{class}
\alias{BrainVector-class}

\title{Class "BrainVector" }
\description{ a class for storing an array or sequence of image volumes,
  such as fMRI data
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BrainVector", ...)}.
	 
}
\section{Slots}{
	 \describe{
    \item{\code{.Data}:}{Object of class \code{"array"} : the stored
      image data }
    \item{\code{space}:}{Object of class \code{"BrainSpace"} : the
      geometry of the image volumes, which must be identical for images in the sequence }
  }
}
\section{Extends}{
Class \code{"\linkS4class{BrainVolume}"}, directly.
Class \code{"\linkS4class{BrainSlice}"}, by class "BrainVolume", distance 2.
Class \code{"\linkS4class{array}"}, by class "BrainVolume", distance 3.
Class \code{"\linkS4class{BrainData}"}, by class "BrainVolume", distance 3.
Class \code{"\linkS4class{structure}"}, by class "BrainVolume", distance 4.
Class \code{"\linkS4class{matrix}"}, by class "BrainVolume", distance 4, with explicit test and coerce.
Class \code{"\linkS4class{vector}"}, by class "BrainVolume", distance 5, with explicit coerce.
Class \code{"\linkS4class{vector}"}, by class "BrainVolume", distance 6, with explicit test and coerce.
Class \code{"\linkS4class{vector}"}, by class "BrainVolume", distance 7, with explicit test and coerce.
}
\section{Methods}{
No methods defined with class "BrainVector" in the signature.
}

\examples{

}
\keyword{classes}
