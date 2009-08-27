\name{SparseBrainVector}
\Rdversion{1.1}
\alias{SparseBrainVector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SparseBrainVector(dat, space, mask = NULL, indices = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{space}{
%%     ~~Describe \code{space} here~~
}
  \item{mask}{
%%     ~~Describe \code{mask} here~~
}
  \item{indices}{
%%     ~~Describe \code{indices} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(dat, space, mask=NULL, indices=NULL) {
  
  if (!is.null(mask) && !is.null(indices)) {
    stop("can provide either mask or indices but not both")
  }

  if (is.null(mask) && is.null(indices)) {
    stop("must provide either mask or indices")
  }

    
  if (is.null(mask)) {
    mspace <- BrainSpace(dim(space)[1:3], origin(space), spacing(space)[1:3], orientation(space), trans(space))
    mask <- BrainVolume(rep(1, length(indices)), mspace, indices=indices)
  }

  if (is.null(indices)) {
    indices <- which(mask > 0)
  }

  if (length(indices) == NROW(dat)) {
    dat <- t(dat)  
  } 
  
  NVOLS <- NROW(dat)
  sdim <- c(dim(space)[1:3], NVOLS)
  vspace <- BrainSpace(sdim, origin(space), spacing(space),  orientation(space), trans(space), reptime=1)
  
  new("SparseBrainVector", space=vspace, mask=mask, data=dat, map=IndexLookupVolume(space(mask), as.integer(indices)))
  
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
