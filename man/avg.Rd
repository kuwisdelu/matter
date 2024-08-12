\name{avg}

\alias{avg}

\title{Central Tendency}

\description{
    Calculate the mean, median, or mode of a vector.
}

\usage{
avg(x, center = mean)
}

\arguments{
	\item{x}{A vector to summarize.}

	\item{center}{A function to use to calculate the central tendency for numeric vectors. Defaults to \code{mean}.}
}

\details{
    Missing values are always removed before calculating the central tendency. The \code{center} funtion will be used to calculate the central tendency for any \code{x} for which either \code{is.numeric} or \code{is.complex} is \code{TRUE}. Otherwise, the mode is calculated.
}

\value{
    A single value summarizing \code{x}.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link{mean}},
    \code{\link{median}}
}

\examples{
set.seed(1)
x <- sample(LETTERS, 50, replace=TRUE)
y <- runif(50)

avg(x)
avg(y)
avg(y, median)
}

\keyword{univar}
