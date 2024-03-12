\name{rescale}

\alias{rescale}

\alias{rescale_sum}
\alias{rescale_rms}
\alias{rescale_ref}
\alias{rescale_range}
\alias{rescale_iqr}

\title{Signal Normalization}

\description{
    Normalize or rescale a signal.
}

\usage{
# Rescale the root-mean-squared of the signal
rescale_rms(x, scale = 1)

# Rescale the sum of (absolute values of) the signal
rescale_sum(x, scale = length(x))

# Rescale according to a specific sample
rescale_ref(x, ref = 1L, scale = 1, domain = NULL)

# Rescale the lower and upper limits of the signal
rescale_range(x, limits = c(0, 1))

# Rescale the interquartile range
rescale_iqr(x, width = 1, center = 0)
}

\arguments{
	\item{x}{A numeric vector.}

    \item{scale}{The scaling value.}

    \item{ref}{The sample (index or domain value) to use.}

    \item{domain}{The domain variable of the signal.}

    \item{limits}{The lower and upper limits to use.}

    \item{width}{The interquartile range to use.}

    \item{center}{The center to use.}
}

\details{
    \code{rescale_rms()} and \code{rescale_sum()} simply divide the signal by its root-mean-square or absolute sum, respectively, before multiplying by the given scaling factor.

    \code{rescale_ref()} finds the closest matching sample to \code{ref} in \code{domain} if given (it is treated as an index if \code{domain} is NULL), and then scales the entire signal to make that sample equal to \code{scale}.

    \code{rescale_range()} simply rescales the signal to match the given upper and lower limits.

    \code{rescale_iqr()} attempts to rescale the signal so that its interquartile range is approximately \code{width}.
}

\value{
    A rescaled numeric vector the same length as \code{x}.
}

\author{Kylie A. Bemis}

\examples{
set.seed(1)
x <- rnorm(100)

sqrt(mean(x^2))
y <- rescale_rms(x, 1)
sqrt(mean(y^2))

range(x)
z <- rescale_range(x, c(-1, 1))
range(z)
}

\keyword{ts}
