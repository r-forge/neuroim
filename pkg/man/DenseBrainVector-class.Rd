\name{DenseBrainVector-class}
\alias{DenseBrainVector-class}
\alias{DenseBrainVector}
\title{DenseBrainVector...}
\description{DenseBrainVector}
\details{\code{DenseBrainVector-class}: Four-dimensional brain image, backed by an array

\code{DenseBrainVector}: constructor function for class \code{\linkS4class{DenseBrainVector}}


}
\section{Extends}{\code{\linkS4class{BrainVector}}, \code{\linkS4class{array}}}
\section{Methods}{\describe{\item{\code{\link[=as.matrix,DenseBrainVector-method]{as.matrix}}}{}\item{\code{\link[=as.sparse,DenseBrainVector,numeric-method]{as.sparse}}}{}}}
\alias{DenseBrainVector}
\usage{DenseBrainVector(data, space, source, label="")
}
\value{\code{DenseBrainVector}: \code{\linkS4class{DenseBrainVector}} instance

}
\arguments{\item{data}{a 4-dimensonal \code{array}}
\item{space}{a \code{\linkS4class{BrainSpace}} object}
\item{source}{an optional \code{\linkS4class{BrainSource}} object}
\item{label}{a label of type \code{character}}
}

