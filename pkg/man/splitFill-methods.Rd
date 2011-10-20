\name{splitFill-methods}
\alias{splitFill-methods}
\alias{splitFill}
\title{Generic function to fill disjoint sets of values with the output of a function...}
\description{Generic function to fill disjoint sets of values with the output of a function}
\value{\code{splitFill-methods}: a new object where the original values have been replaced by the function output

}
\arguments{\item{x}{the object to split}
\item{fac}{the factor to split by}
\item{FUN}{the function to summarize the the clusters}
}
\alias{splitFill,BrainVolume,factor,function-method}
\alias{splitFill}
\note{FUN can return one value per category or one value per voxel}

