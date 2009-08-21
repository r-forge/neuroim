\name{BrainVolume-class}
\docType{class}
\alias{BrainVolume-class}
\alias{writeVolume,BrainVolume,character-method}

\title{Class "BrainVolume" }
\description{ a class for storing 3D volumetric image data }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BrainVolume", ...)}
}
\section{Slots}{
	 \describe{
    \item{\code{.Data}:}{Object of class \code{"array"} : the image data
    array }
    \item{\code{space}:}{Object of class \code{"BrainSpace"} : the image
    geometry }
  }
}
\section{Extends}{
Class \code{"\linkS4class{BrainSlice}"}, directly.
Class \code{"\linkS4class{array}"}, by class "BrainSlice", distance 2.
Class \code{"\linkS4class{BrainData}"}, by class "BrainSlice", distance 2.
Class \code{"\linkS4class{structure}"}, by class "BrainSlice", distance 3.
Class \code{"\linkS4class{matrix}"}, by class "BrainSlice", distance 3, with explicit test and coerce.
Class \code{"\linkS4class{vector}"}, by class "BrainSlice", distance 4, with explicit coerce.
Class \code{"\linkS4class{vector}"}, by class "BrainSlice", distance 5, with explicit test and coerce.
Class \code{"\linkS4class{vector}"}, by class "BrainSlice", distance 6, with explicit test and coerce.
}
\section{Methods}{
  \describe{
    \item{writeVolume}{\code{signature(x="BrainVolume",fileName="character")} : writes volume to disk using default NIFTI format }
    }
    
}
\examples{

}
\keyword{classes}
