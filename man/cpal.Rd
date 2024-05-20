\name{cpal}

\alias{cpal}
\alias{dpal}
\alias{add_alpha}

\title{Color Palettes}

\description{
    These functions provide simple color palettes.
}

\usage{
## Continuous color palettes
cpal(palette = "Viridis")

## Discrete color palettes
dpal(palette = "Tableau 10")

# Add transparency to colors
add_alpha(colors, alpha = 1, pow = 1)
}

\arguments{
    \item{palette}{The name of a color palette. See \code{\link{palette.pals}} and \code{\link{hcl.pals}}.}

    \item{colors}{A character vector of colors to add transparency to.}

    \item{alpha}{A numeric vector giving the level of transparency in the range [0, 1] where 0 is fully transparent and 1 is fully opaque.}

    \item{pow}{The power scaling of the alpha channel. A linear alpha scale often results in poor interpretability for superposed images, so raising the alpha channel (already in range [0, 1]) to a power > 1 can improve interpretability in these cases. Conversely, for highly skewed data, using a power < 1 can reduce the impact of extreme values.}
}

\value{
	A character vector of colors or a function for generating \code{n} colors.
}

\seealso{
    \code{\link{vizi}},
    \code{\link{image}}
}

\author{Kylie A. Bemis}

\examples{
f <- cpal("viridis")
cols <- f(10)
add_alpha(cols, 1:10/10)
}

\keyword{color}
