\name{vizi_style}

\alias{vizi_style}
\alias{vizi_engine}
\alias{vizi_par}

\title{Set Graphical Parameters}

\description{
    Set global parameters for plotting \code{\link{vizi}} graphics.
}

\usage{
## Set style and palettes
vizi_style(style = "light", dpal = "Tableau 10", cpal = "Viridis")

## Set plotting engine
vizi_engine(engine = c("base", "plotly"))

## Set graphical parameters
vizi_par(\dots, style = getOption("matter.vizi.style"))
}

\arguments{
    \item{style}{The visual style to use for plotting. Currently supported styles are "light", "dark", "voidlight", "voiddark", and "transparent".}

    \item{dpal, cpal}{The name of discrete and continous color palettes. See \code{\link{palette.pals}} and \code{\link{hcl.pals}}.}

    \item{engine}{The plotting engine. Default is to use base graphics. Using "plotly" requires the \code{plotly} package to be installed.}

    \item{\dots}{These specify additional graphical parameters (as in \code{\link[graphics]{par}}).}
}

\value{
	A character vector or list with the current parameters.
}

\seealso{
    \code{\link{vizi}}
}

\author{Kylie A. Bemis}

\keyword{color}
\keyword{device}
