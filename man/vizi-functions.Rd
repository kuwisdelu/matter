\name{vizi}

\alias{vizi}
\alias{add_mark}
\alias{add_facets}
\alias{set_title}
\alias{set_channel}
\alias{set_coord}
\alias{set_par}
\alias{set_engine}
\alias{as_facets}

\alias{preplot,vizi_plot-method}
\alias{preplot,vizi_facets-method}

\alias{plot,vizi_plot,ANY-method}
\alias{plot,vizi_facets,ANY-method}

\alias{print,vizi_plot,ANY-method}
\alias{print,vizi_facets,ANY-method}

\alias{plot,vizi_key,ANY-method}
\alias{plot,vizi_colorkey,ANY-method}

\alias{c,vizi_plot-method}
\alias{c,vizi_facets-method}

\alias{combine,vizi_plot,vizi_plot-method}
\alias{combine,vizi_facets,vizi_facets-method}

\title{A Simple Grammar of Base Graphics}

\description{
    These functions provide a simple grammar of graphics approach to programming with R's base graphics system.
}

\usage{
## Initialize a plot
vizi(data, \dots, encoding = NULL, params = NULL)

## Add plot components
add_mark(plot, mark, \dots, encoding = NULL, data = NULL,
    trans = NULL, params = NULL)

add_facets(plot, by = NULL, data = NULL,
    nrow = NA, ncol = NA, labels = NULL,
    drop = TRUE, free = "")

## Set plot attributes
set_title(plot, title)

set_channel(plot, channel, label = NULL,
    limits = NULL, scheme = NULL, key = TRUE)

set_coord(plot, xlim = NULL, ylim = NULL, zlim = NULL,
    rev = "", log = "", asp = NA, grid = TRUE)

set_engine(plot, engine = c("base", "plotly"))

set_par(plot, \dots, style = NULL)

## Combine plots
as_facets(plotlist, \dots, nrow = NA, ncol = NA,
    labels = NULL, drop = TRUE, free = "")
}

\arguments{
    \item{data}{A \code{data.frame}.}

    \item{\dots}{For \code{vizi} and \code{add_mark}, these should be named arguments specifying the encoding for the plot. The argument names should specify channels, using either base R-style (e.g., pch, cex) \emph{or} ggplot-style names (e.g., shape, size). One-sided formula arguments will be evaluated in the environment of \code{data}. Non-formula arguments will be used as-is. For \code{set_par}, these specify additional graphical parameters (as in \code{\link[graphics]{par}}) or arguments to \code{\link[graphics]{persp}} for 3D plots. For \code{as_facets}, these should be additional subplots.}

    \item{encoding}{Encodings specified as a list rather than using \dots.}

    \item{params}{Additional graphical parameters that are not mapped to the data, using either base R-style (e.g., pch, cex) \emph{or} ggplot-style names (e.g., shape, size)}

    \item{plot, plotlist}{A \code{vizi_plot} object, or a list of such objects, respectively.}

    \item{title}{The title of the plot.}

    \item{channel}{The channel to modify, using ggplot-style names (e.g., shape, size).}

    \item{label, labels}{Plotting labels.}

    \item{limits}{The limits for the channel, specified as \code{c(min, max)} for continuous variables or a character vector of possible levels for discrete variables. The data will be constrained to these limits \emph{before} plotting.}

    \item{scheme}{A function or vector giving the scheme for encoding the channel. For example, a vector of colors, or a function that returns a vector of \code{n} colors.}

    \item{key}{Should a key be generated for the channel?}

    \item{xlim, ylim, zlim}{The plot limits. These only affect the plotting window, not the data. See \code{\link[graphics]{plot.window}}}

    \item{rev}{A string specifying spatial dimensions that should be reversed. E.g., \code{""}, \code{"x"}, \code{"y"}, or \code{"xy"}.}

    \item{log, asp}{See \code{\link[graphics]{plot.window}}.}

    \item{grid}{Should a rectangular grid be included in the plot?}

    \item{engine}{The plotting engine. Default is to use base graphics. Using "plotly" requires the \code{plotly} package to be installed.}

    \item{style}{The visual style to use for plotting. Currently supported styles are "light", "dark", and "classic".}

    \item{mark}{The name of a supported mark, such as "points", "lines", etc.}

    \item{trans}{A list providing parameters for any transformations the mark supports.}

    \item{by}{A vector or formula giving the facet specification.}

    \item{nrow, ncol}{The number of rows and columns in the facet grid.}

    \item{drop}{Should empty facets be dropped?}

    \item{free}{A string specifying the free spatial dimensions during faceting. E.g., \code{""}, \code{"x"}, \code{"y"}, or \code{"xy"}.}
}

\details{
    Currently supported marks include:

    \itemize{
        \item{\code{points}: Points (i.e., scatterplots).}
        \item{\code{lines}: Lines (i.e., line charts).}
        \item{\code{peaks}: Vertical (histogram) lines.}
        \item{\code{text}: Text labels.}
        \item{\code{intervals}: Line intervals for representing error bars or confidence intervals.}
        \item{\code{boxplot}: Box-and-whisker plots.}
        \item{\code{pixels}: 2D image from pixels.}
        \item{\code{voxels}: 3D image from voxels.}
    }

    Currently supported encodings include:

    \itemize{
        \item{\code{x, y, z}: Position channels.}
        \item{\code{xmin, xmax, ymin, ymax}: For \code{intervals}.}
        \item{\code{shape}: Shape of points (pch).}
        \item{\code{size}: Size of points (cex).}
        \item{\code{color, colour}: Color (col, fg).}
        \item{\code{fill}: Fill color (bg).}
        \item{\code{alpha}: Opacity.}
        \item{\code{linetype, linewidth, lineend, linejoin, linemetre}: Line properties (lty, lwd, lend, ljoin, lmitre).}
    }
}

\value{
	An object of class \code{vizi_plot}.
}

\seealso{
    \code{\link{vizi_points}},
    \code{\link{vizi_lines}},
    \code{\link{vizi_peaks}},
    \code{\link{vizi_text}},
    \code{\link{vizi_intervals}},
    \code{\link{vizi_boxplot}},
    \code{\link{vizi_pixels}},
    \code{\link{vizi_voxels}}
}

\author{Kylie A. Bemis}

\examples{
require(datasets)

mtcars <- transform(mtcars,
    am=factor(am, labels=c("auto", "manual")))

# faceted scatter plot
vizi(mtcars, x=~disp, y=~mpg) |>
    add_mark("points") |>
    add_facets(~mtcars$am)

# faceted scatter plot with color
vizi(mtcars, x=~disp, y=~mpg, color=~am) |>
    add_mark("points",
        params=list(shape=20, size=2, alpha=0.8)) |>
    add_facets(~mtcars$am)


coords <- expand.grid(x=1:nrow(volcano), y=1:ncol(volcano))

# volcano image
vizi(coords, x=~x, y=~y, color=volcano) |>
    add_mark("pixels") |>
    set_coord(grid=FALSE) |>
    set_par(xaxs="i", yaxs="i")
}

\keyword{aplot}
\keyword{dplot}
\keyword{hplot}
