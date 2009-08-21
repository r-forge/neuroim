\name{value}
\alias{value}

\title{ a function to access the value given a real world coordinate }
\description{
  this function takes real world coordinate locations and returns an
  interpolated image value.
}
\usage{
value(object, x, y, ...)
}

\arguments{
  \item{object}{ an object derived from class "BrainData" }
  \item{x}{ the x real world coordinate }
  \item{y}{ the y real worold coordinate }
  \item{\dots}{ world coordinates for additional dimensions }
}
\details{
  coordinates supplied to the function will first be multiplied by the
  inverse transformation matrix associated with the "BrainSpace" object,
  yileding a set of numeric grid positions. An image value will be
  computed via linear interpolation.
}
\value{
  interploated value at real world coordinates.
}
\examples{

}
\keyword{ manip }

