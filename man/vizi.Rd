\name{vizi}

\alias{vizi}
\alias{set_channel}
\alias{set_coord}
\alias{add_mark}
\alias{add_facets}
\alias{as_facets}

\alias{vizi_par}
\alias{vizi_style}

\alias{cpal}
\alias{dpal}

\alias{preplot,vizi_plot-method}
\alias{preplot,vizi_facets-method}

\alias{plot,vizi_plot,ANY-method}
\alias{plot,vizi_facets,ANY-method}

\alias{plot,vizi_points,ANY-method}
\alias{plot,vizi_lines,ANY-method}
\alias{plot,vizi_peaks,ANY-method}

\alias{plot,vizi_key,ANY-method}
\alias{plot,vizi_colorkey,ANY-method}

\title{A Simple Grammar of Base Graphics}

\description{
    These functions provide a simple grammar of graphics approach to programming with R's base graphics system.
}

\usage{
vizi(data, \dots, encoding = NULL, params = NULL)

set_channel(plot, channel, label = NULL,
    limits = NULL, scheme = NULL, \dots)

set_coord(plot, xlim = NULL, ylim = NULL,
    log = "", asp = NA, grid = TRUE, \dots)

add_mark(plot, mark, encoding = NULL, data = NULL,
    trans = NULL, \dots)

add_facets(plot, by = NULL, data = NULL,
    nrow = NA, ncol = NA, labels = NULL,
    drop = TRUE, free = "")

as_facets(plotlist, nrow = NA, ncol = NA,
    labels = NULL, drop = TRUE, free = "")
}

\arguments{
    \item{data}{A \code{data.frame}.}

    \item{\dots}{For \code{vizi}, these should be named arguments specifying the encoding for the plot. The argument names should specify channels, using either base R-style (e.g., pch, cex) \emph{or} ggplot-style names (e.g., shape, size). One-sided formula arguments will be evaluated in the environment of \code{data}. Non-formula arguments will be used as-is. For other functions, these specify additional graphical parameters.}

    \item{encoding}{Encodings specified as a list rather than using \dots.}

    \item{params}{Additional graphical parameters that are not mapped to the data, using either base R-style (e.g., pch, cex) \emph{or} ggplot-style names (e.g., shape, size)}

    \item{plot, plotlist}{A \code{vizi_plot} object, or a list of such objects, respectively.}

    \item{channel}{The channel to modify, using ggplot-style names (e.g., shape, size).}

    \item{label, labels}{Plotting labels.}

    \item{limits}{The limits for the channel, specified as \code{c(min, max)} for continuous variables or a character vector of possible levels for discrete variables. The data will be constrained to these limits \emph{before} plotting.}

    \item{scheme}{A function or vector giving the scheme for encoding the channel. For example, a vector of colors, or a function that returns a vector of \code{n} colors.}

    \item{xlim, ylim}{The plot limits. These only affect the plotting window, not the data. See \code{\link[graphics]{plot.window}}}

    \item{log, asp}{See \code{\link[graphics]{plot.window}}.}

    \item{grid}{Should a rectangular grid be included in the plot?}

    \item{mark}{The name of a supported mark, such as "points", "lines", etc.}

    \item{trans}{A list providing parameters for any transformations the mark supports.}

    \item{by}{A vector or formula giving the facet specification.}

    \item{nrow, ncol}{The number of rows and columns in the facet grid.}

    \item{drop}{Should empty facets be dropped?}

    \item{free}{A string specifying the free spatial dimensions during faceting. E.g., \code{""}, \code{"x"}, \code{"y"}, \code{"xy"}, \code{"yx"}.}
}

\value{
	An object of class \code{vizi_plot}.
}

\author{Kylie A. Bemis}

\examples{
if ( interactive() ) {
    vizi(mtcars, x=~disp, y=~mpg) |>
        add_mark("points") |>
        add_facets(~as.factor(mtcars$am)) |>
        plot()
}
}

\keyword{aplot}
\keyword{dplot}
\keyword{hplot}
