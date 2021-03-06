% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/findstk.R
\name{findstk}
\alias{findstk}
\title{Optimize stkmod function within depth interval}
\usage{
findstk(profin, sigdiff = 3, expand = 100, plot = FALSE)
}
\arguments{
\item{profin}{\code{data.frame} of a single depth profile}

\item{sigdiff}{numeric of density difference beyond which the model is estimated}

\item{expand}{numeric for expansion factor to increase resolution of search}

\item{plot}{logical if the model is plotted}
}
\value{
The depth of the mixing layer if \code{plot = FALSE}, otherwise a plot of the stick model.
}
\description{
Optimize stkmod function within depth interval
}
\details{
This function finds the optimal value for \code{strt} in \code{\link{stkmod}} that minimizes RMSE for the stick model.  The function will return the maximum depth value in \code{profin} if the range of seawater densities is less than \code{sigdiff}, implying no stratification. 

The \code{profin} data frame must have two columns for depth (\code{binmd}) and seawater density (\code{sig}).
}

