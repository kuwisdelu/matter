\name{plot-vizi}

\alias{vizi_points}
\alias{vizi_lines}
\alias{vizi_peaks}
\alias{vizi_pixels}

\alias{plot.vizi_points}
\alias{plot.vizi_lines}
\alias{plot.vizi_peaks}
\alias{plot.vizi_pixels}

\alias{plot,vizi_points,ANY-method}
\alias{plot,vizi_lines,ANY-method}
\alias{plot,vizi_peaks,ANY-method}
\alias{plot,vizi_pixels,ANY-method}

\title{Plotting Graphical Marks}

\description{
    These functions provide plotting methods for various graphical marks. They are not intended to be called directly.
}

\usage{
\S3method{plot}{vizi_points}(x, plot = NULL, add = FALSE, \dots,
    n = Inf, downsampler = "lttb")

\S3method{plot}{vizi_lines}(x, plot = NULL, add = FALSE, \dots,
    n = Inf, downsampler = "lttb")

\S3method{plot}{vizi_peaks}(x, plot = NULL, add = FALSE, \dots,
    n = Inf, downsampler = "lttb")

\S3method{plot}{vizi_pixels}(x, plot = NULL, add = FALSE, \dots,
    enhance = FALSE, smooth = FALSE, scale = FALSE,
    useRaster = TRUE)
}

\arguments{
    \item{x}{A graphical mark.}

    \item{plot}{A \code{vizi_plot} object.}

    \item{add}{Should the plot be added to the current panel or start a new panel?}

    \item{\dots}{Additional graphical parameters passed to the underlying base graphics plotting function.}

    \item{n}{Maximum number of points to plot. This is useful for downsampling series with far more data points than are useful to plot. See \code{\link{downsample}} for details.}

    \item{downsampler}{If \code{n} is less than the number of points, then this is the downsampling method to use. See \code{\link{downsample}} for details.}

    \item{enhance}{The name of a contrast enhancement method, such as \code{"hist"} or \code{"adapt"} for \code{enhance_hist()} and \code{enhance_adapt()}, etc. See \code{\link{enhance}} for details.}

    \item{smooth}{The name of a smoothing method, such as \code{"gauss"} or \code{"bi"} for \code{filt2_gauss()} and \code{filt2_bi()}, etc. See \code{\link{filt2}} for details.}

    \item{scale}{If \code{TRUE}, then all image values will be scaled to the range [0, 100]. This is useful for comparing images with differing intensity levels across facets or layers.}

    \item{useRaster}{Should a bitmap raster be used for plotting? This is typically faster on supported devices. A fallback to polygon-based plotting is used if raster plotting is not supported.}
}

\seealso{
    \code{\link{vizi}},
    \code{\link{add_mark}}
}

\author{Kylie A. Bemis}

\keyword{aplot}
\keyword{hplot}
