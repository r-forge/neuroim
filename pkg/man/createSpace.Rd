\name{createSpace}
\alias{createSpace}
\title{ creates a "BrainSpace" object }
\description{
  function to instantiate a "BrainSpace" instance given a "BrainHeader" object 
}
\usage{
createSpace(x)
}

\arguments{
  \item{x}{ an object derived from the "BrainHeader" class }
}
\details{
  The prupose of the generic method is to provide a way of generating a
  "BrainSpace" object on the basis of information available from an
  image header.
}
\value{
  an object derived from class "BrainSpace"
}

\examples{
}
\keyword{ manip }

