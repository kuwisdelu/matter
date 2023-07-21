\name{plot}

\alias{vizi_points}
\alias{vizi_lines}
\alias{vizi_peaks}

\alias{plot.vizi_points}
\alias{plot.vizi_lines}
\alias{plot.vizi_peaks}

\alias{plot,vizi_points,ANY-method}
\alias{plot,vizi_lines,ANY-method}
\alias{plot,vizi_peaks,ANY-method}

\title{Plotting Graphical Marks}

\description{
    These functions provide plotting methods for various graphical marks.
}

\usage{
\S3method{plot}{vizi_points}(x, plot = NULL, add = FALSE, \dots,
    nmax = NULL, sampler = NULL)

\S3method{plot}{vizi_lines}(x, plot = NULL, add = FALSE, \dots,
    nmax = NULL, sampler = NULL)

\S3method{plot}{vizi_peaks}(x, plot = NULL, add = FALSE, \dots,
    nmax = NULL, sampler = NULL)
}

\arguments{
    \item{x}{A graphical mark.}

    \item{plot}{A \code{vizi_plot} object.}

    \item{add}{Should the plot be added to the current panel or start a new panel?}

    \item{\dots}{Additional graphical parameters passed to the underlying base graphics plotting function.}

    \item{nmax}{Maximum number of points to plot. This is useful for downsampling series with far more data points than are useful to plot. See \code{\link{downsample}} for details.}

    \item{sampler}{If \code{nmax} is specified, then this is the downsampling method to use. See \code{\link{downsample}} for details.}
}

\seealso{
    \code{\link{vizi}},
    \code{\link{add_mark}}
}

\author{Kylie A. Bemis}

\keyword{aplot}
\keyword{hplot}
