\name{estres}

\alias{estres}

\title{Estimate Signal Resolution}

\description{
    Estimate the resolution (approximate sampling rate) of a signal based on its domain values.
}

\usage{
estres(x, tol = NA, ref = NA_character_)
}

\arguments{
	\item{x}{A numeric vector giving the domain values of the signal.}

    \item{tol}{The tolerance allowed when determining if the estimated resolution is valid (i.e., actually matches the given domain values). Noise in the sampling rate will be allowed up to this amount. If \code{NA} (the default), then the resolution is simply calculated as the smallest difference between sorted domain values.}

    \item{ref}{If 'abs', then comparison is done by taking the absolute difference. If 'x', then relative differences are used. If missing, then the funciton will try to determine which gives a better fit to the domain values.}
}

\value{
    A single number named "absolute" or "relative" giving the approximate constant sampling rate matching the given domain values. \code{NA} if a sampling rate could not be determined.
}

\author{Kylie A. Bemis}

\examples{
x <- seq_rel(501, 600, by=1e-3)

estres(x)
}

\keyword{ts}
