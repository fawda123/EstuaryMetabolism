% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/depsd.R
\name{depsd}
\alias{depsd}
\title{Get midpoints and widths of depth bins}
\usage{
depsd(dat, maxd = NULL)
}
\arguments{
\item{dat}{data.frame in long format with rows as one sample time and depth bin}

\item{maxd}{numeric for maximum depth of the site, see details}
}
\value{
A data.frame with two columns, \code{binmd} for midpoint depth (m) and \code{binwd} for midpoint width
}
\description{
Get midpoints and widths of depth bins
}
\details{
If \code{maxd} is not supplied, the site depth is estimated as the maximum bin midpoint plus half the average binwidth. 

All depth values are positive, surface is assumed to be zero depth.
}

