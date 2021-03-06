% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getn2kv.R
\name{getn2kv}
\alias{getn2kv}
\title{Get Brunt-Vaisala frequency (N2), vertical turbulent diffusivity (Kv)}
\usage{
getn2kv(dat, g = 9.7963)
}
\arguments{
\item{dat}{input data.frame pre-processed with \code{\link{getwithin}}}

\item{g}{gravity constant, m2 s-1}
}
\value{
Input data frame with two additional columns for \code{n2} (s-2) and \code{kv} (m2 hr-1)
}
\description{
Get Brunt-Vaisala frequency (N2), vertical turbulent diffusivity (Kv)
}
\details{
Brunt-Vaisala frequency and the vertical turbulent diffusivity are appended for each row where estimable in \code{dat}. Estimates are based on equations 3 and 4 in Staehr et al. 2012.  Estimated values for each bin are in reference to the current bin and the bin immediately below.  As such, values are not estimated for the deepest depth bin.
}
\references{
Staehr, P.A., Christensen, J.P.A., Batt, R.D., Read, J.S. 2012. Ecosystem metabolism in a stratified lake. 57(5):1317-1330.
}

