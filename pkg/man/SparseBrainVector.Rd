\name{SparseBrainVector}
\Rdversion{1.1}
\alias{SparseBrainVector}

\title{
	a function to create a new SparseBrainVector instance
}
\description{
	A convenience creation function for the SparseBrainVector class.  
	This function can construct a 4-dimensional sparse data structure from a matrix of values and a mask or 1d index vector.
}
\usage{
	SparseBrainVector(dat, space, mask = NULL, indices = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{ a \code{matrix} of values where the number of rows or columns must equal the number of elements in the \code{mask} or \code{ndices} arguments.
	
}
  \item{space}{ a \code{BrainSpace} instance that encodes the 3d geometry of the to-be-created structure
		
}
  \item{mask}{ an mask which must have the same dimensions as the \code{space} argument.
	
}
  \item{indices}{ an alternative to \code{mask} (either one or the other must be supplied) which is simple a 1d vector of indices that map to the 3d coordinates.
}
}
\details{
	
}
\value{
	an instance of the \code{SparseBrainVector} class
}

\author{
	bbuchsbaum@rotman-baycrest.on.ca
}



