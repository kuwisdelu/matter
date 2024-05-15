\name{stream-stats}
\docType{methods}

\alias{stream_stat}

\alias{s_range}
\alias{s_min}
\alias{s_max}
\alias{s_prod}
\alias{s_sum}
\alias{s_mean}
\alias{s_sd}
\alias{s_var}
\alias{s_any}
\alias{s_all}
\alias{s_nnzero}

\alias{s_stat}
\alias{s_rowstats}
\alias{s_colstats}

\alias{as.data.frame,stream_stat-method}
\alias{combine,stream_stat,ANY-method}
\alias{stat_c}

\title{Streaming Summary Statistics}

\description{
    These functions allow calculation of streaming statistics. They are useful, for example, for calculating summary statistics on small chunks of a larger dataset, and then combining them to calculate the summary statistics for the whole dataset.

    This is not particularly interesting for simpler, commutative statistics like \code{sum()}, but it is useful for calculating non-commutative statistics like running \code{sd()} or \code{var()} on pieces of a larger dataset.
}

\usage{
# calculate streaming univariate statistics
s_range(x, \dots, na.rm = FALSE)

s_min(x, \dots, na.rm = FALSE)

s_max(x, \dots, na.rm = FALSE)

s_prod(x, \dots, na.rm = FALSE)

s_sum(x, \dots, na.rm = FALSE)

s_mean(x, \dots, na.rm = FALSE)

s_var(x, \dots, na.rm = FALSE)

s_sd(x, \dots, na.rm = FALSE)

s_any(x, \dots, na.rm = FALSE)

s_all(x, \dots, na.rm = FALSE)

s_nnzero(x, \dots, na.rm = FALSE)

s_stat(x, stat, group, na.rm = FALSE, \dots)

# calculate streaming matrix statistics
s_rowstats(x, stat, group, na.rm = FALSE, \dots)

s_colstats(x, stat, group, na.rm = FALSE, \dots)

# calculate combined summary statistics
stat_c(x, y, \dots)
}

\arguments{
    \item{x, y, \dots}{Object(s) on which to calculate a summary statistic, or a summary statistic to combine.}

    \item{stat}{The name of a summary statistic to compute over the rows or columns of a matrix. Allowable values include: "range", "min", "max", "prod", "sum", "mean", "var", "sd", "any", "all", and "nnzero".}

    \item{group}{A factor or vector giving the grouping. If not provided, no grouping will be used.}

    \item{na.rm}{If \code{TRUE}, remove \code{NA} values before summarizing.}
}

\details{
    These summary statistics methods are intended to be applied to chunks of a larger dataset. They can then be combined either with the individual summary statistic functions, or with \code{stat_c()}, to produce the combined summary statistic for the full dataset. This is most useful for calculating running variances and standard deviations iteratively, which would be difficult or impossible to calculate on the full dataset.

    The variances and standard deviations are calculated using running sum of squares formulas which can be calculated iteratively and are accurate for large floating-point datasets (see reference).
}

\value{
    For all univariate functions except \code{s_range()}, a single number giving the summary statistic. For \code{s_range()}, two numbers giving the minimum and the maximum values.

    For \code{s_rowstats()} and \code{s_colstats()}, a vector of summary statistics.
}

\author{Kylie A. Bemis}

\references{
    B. P. Welford. ``Note on a Method for Calculating Corrected Sums of Squares and Products.'' Technometrics, vol. 4, no. 3, pp. 1-3, Aug. 1962.

    B. O'Neill. ``Some Useful Moment Results in Sampling Problems.'' The American Statistician, vol. 68, no. 4, pp. 282-296, Sep. 2014.
}

\seealso{
    \code{\link{Summary}}
}

\examples{
set.seed(1)
x <- sample(1:100, size=10)
y <- sample(1:100, size=10)

sx <- s_var(x)
sy <- s_var(y)

var(c(x, y))
stat_c(sx, sy) # should be the same

sxy <- stat_c(sx, sy)

# calculate with 1 new observation
var(c(x, y, 99))
stat_c(sxy, 99)

# calculate over rows of a matrix
set.seed(2)
A <- matrix(rnorm(100), nrow=10)
B <- matrix(rnorm(100), nrow=10)

sx <- s_rowstats(A, "var")
sy <- s_rowstats(B, "var")

apply(cbind(A, B), 1, var)
stat_c(sx, sy) # should be the same
}

\keyword{univar}
