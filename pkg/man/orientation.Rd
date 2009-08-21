\name{orientation}
\alias{orientation}
\title{ function returns the orientation of a brain image }
\description{
  generic function that returns a character string coding the
  orientation of a brain image
}
\usage{
orientation(x)
}

\arguments{
  \item{x}{ an object deriving from class "BrainSpace" or "BrainData" }
}
\details{
  The orientation string encodes the anatomical direction of each of the
  (spatial) image axes.  Thus, a standard "neurologically" oriented
  image volume has the orientatrion string "LPI", whcih stands for
  "Left, Posterior, Inferior".  Currently this function is used only for
  information.
  
}
\value{
  character string representing image orientation
}

\examples{
}
\keyword{ manip }

