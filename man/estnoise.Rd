\name{estnoise}

\alias{estnoise}

\alias{estnoise_sd}
\alias{estnoise_mad}
\alias{estnoise_quant}
\alias{estnoise_diff}
\alias{estnoise_filt}

\title{Local Noise Estimation}

\description{
    Estimate the noise across a signal.
}

\usage{
# Difference-based noise estimation
estnoise_diff(x, nbins = 1L, overlap = 0.5,
    index = NULL)

# Dynamic noise level filtering
estnoise_filt(x, nbins = 1L, overlap = 0.5,
    msnr = 2, threshold = 0.5, isPeaks = FALSE, index = NULL)

# SD-based noise estimation
estnoise_sd(x, nbins = 1, overlap = 0.5,
    k = 9, index = NULL)

# MAD-based noise estimation
estnoise_mad(x, nbins = 1, overlap = 0.5,
    k = 9, index = NULL)

# Quantile-based noise estimation
estnoise_quant(x, nbins = 1, overlap = 0.5,
    k = 9, prob = 0.95, index = NULL)
}

\arguments{
	\item{x}{A numeric vector.}

    \item{nbins}{The number of bins to use if estimating a variable noise level.}

    \item{overlap}{The fraction of overlap between bins if estimating a variable noise level.}

    \item{msnr}{The minimum signal-to-noise ratio for distinguishing signal peaks from noise peaks.}

    \item{threshold}{The required signal-to-noise difference for the first non-noise peak.}

    \item{isPeaks}{Does \code{x} represent a profile signal (\code{FALSE}) or its peaks (\code{TRUE})?}

    \item{index}{A matrix or list of domain vectors giving the sampling locations of \code{x} if it is a multidimensional signal.}

    \item{k}{The size of the smoothing window when calculating the difference between the raw signal and a smoothed version of the signal.}

    \item{prob}{The quantile used when estimating the noise.}
}

\details{
    \code{estnoise_diff()} estimates the local noise from the mean absolute differences between the signal and the average of its derivative. For noisy signals, the derivative is dominated by the noise, making it a useful estimator of the noise.

    \code{estnoise_sd()}, \code{estnoise_mad()}, and \code{estnoise_quant()} all estimate the local noise by first smoothing the signal with a Gaussian filter, and then subtracting the raw signal from the smoothed signal to isolate the noise component. The noise is then summarized with the corresponding statistic.

    \code{estnoise_filt()} uses the dynamic noise level filtering algorithm of Xu and Freitas (2010) based on the local signal in an approach similar to Gallia et al. (2013). The peaks in the signal are sorted, and the smallest peak is assumed to be noise and is used to estimate the noise level. Each peak is then compared to the previous peak. A peak is labeled a signal peak only if it exceeds a minimum signal-to-noise ratio. Otherwise, the peak is labeled noise, and the noise level is re-estimated. This process continues until a signal peak is found, and the noise level is estimated from the noise peaks.
}

\value{
    A numeric vector the same length as \code{x} with the estimated local noise level.
}

\author{Kylie A. Bemis}

\references{
    H. Xu and M. A. Freitas. ``A Dynamic Noise Level Algorithm for Spectral Screening of Peptide MS/MS Spectra.'' BMC Bioinformatics, vol. 11, no. 436, Aug. 2010.

    J. Gallia, K. Lavrich, A. Tan-Wilson, and P. H. Madden. ``Filtering of MS/MS data for peptide identification.'' BMC Genomics, vol. 14, suppl. 7, Nov. 2013.
}

\examples{
# simple signal
set.seed(1)
n <- 500
x <- rnorm(n)
x <- x + 90 * dnorm(seq_along(x), mean=n/4)
x <- x + 80 * dnorm(seq_along(x), mean=n/2)
x <- x + 70 * dnorm(seq_along(x), mean=3*n/4)

ns <- estnoise_quant(x)
plot(x, type="l")
lines(ns, col="blue")

# simulated spectrum
set.seed(1)
x <- simspec(size=5000)

ns <- estnoise_quant(x, nbins=20)
plot(x, type="l")
lines(ns, col="blue")
}

\keyword{ts}
