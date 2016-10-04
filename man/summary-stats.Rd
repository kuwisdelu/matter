\name{summary-stats}
\docType{methods}

\alias{mean}
\alias{sum}
\alias{sd}
\alias{var}

\alias{mean,matter-method}
\alias{sum,matter-method}
\alias{sd,matter-method}
\alias{var,matter-method}

\alias{colSds}
\alias{colVars}
\alias{colMeans,matter-method}
\alias{colSums,matter-method}
\alias{colSds,matter-method}
\alias{colVars,matter-method}

\alias{rowSds}
\alias{rowVars}
\alias{rowMeans,matter-method}
\alias{rowSums,matter-method}
\alias{rowSds,matter-method}
\alias{rowVars,matter-method}

\title{Summary Statistics for ``matter'' Objects}

\description{
    These functions efficiently calculate summary statistics for \code{\linkS4class{matter}} objects. For matrices, they operate efficiently on both rows and columns.
}

\usage{
\S4method{mean}{matter}(x, na.rm)
\S4method{sum}{matter}(x, na.rm)
\S4method{sd}{matter}(x, na.rm)
\S4method{var}{matter}(x, na.rm)
\S4method{colMeans}{matter}(x, na.rm)
\S4method{colSums}{matter}(x, na.rm)
\S4method{colSds}{matter}(x, na.rm)
\S4method{colVars}{matter}(x, na.rm)
\S4method{rowMeans}{matter}(x, na.rm)
\S4method{rowSums}{matter}(x, na.rm)
\S4method{rowSds}{matter}(x, na.rm)
\S4method{rowVars}{matter}(x, na.rm)
}

\arguments{
    \item{x}{A \code{\linkS4class{matter}} object.}

    \item{na.rm}{If \code{TRUE}, remove \code{NA} values before summarizing.}
}

\details{
    These summary statistics methods operate on chunks of data (equal to the \code{chunksize} of \code{x}) which are loaded into memory and then freed before reading the next chunk.

    For row and column summaries on matrices, the iteration scheme is dependent on the layout of the data. Column-major matrices will always be iterated over by column, and row-major matrices will always be iterated over by row. Row statistics on column-major matrices and column statistics on row-major matrices are calculated iteratively.

    The efficiency of these methods is entirely dependent on the \code{chunksize} of \code{x}. Larger chunks will yield faster calculations, but greater memory usage. The row and column summary methods may be more or less efficient than the equivalent call to \code{\link[matter]{apply}}, depending on the chunk size.

    Varsiance and standard deviation are calculated using a running sum of squares formula which can be calculated iteratively and is accurate for large floating-point datasets (see reference).
}

\value{
    For \code{mean}, \code{sum}, \code{sd}, and \code{var}, a single number. For the column summaries, a vector of length equal to the number of columns of the matrix. For the row summaries, a vector of length equal to the number of rows of the matrix.
}

\author{Kylie A. Bemis}

\references{
    B. P. Welford, ``Note on a Method for Calculating Corrected Sums of Squares and Products,'' Technometrics, vol. 4, no. 3, pp. 1-3, Aug. 1962.
}

\seealso{
    \code{\link[base]{colSums}},
    \code{\link[base]{colMeans}},
    \code{\link[base]{rowSums}},
    \code{\link[base]{rowMeans}}
}

\examples{
x <- matter(1:100, nrow=10, ncol=10)

sum(x)
mean(x)
var(x)
sd(x)

colSums(x)
colMeans(x)
colVars(x)
colSds(x)

rowSums(x)
rowMeans(x)
rowVars(x)
rowSds(x)
}

\keyword{methods}
\keyword{univar}
