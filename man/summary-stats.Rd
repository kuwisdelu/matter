\name{summary-stats}
\docType{methods}

\alias{Summary}

\alias{mean}
\alias{sd}
\alias{var}

\alias{range,matter_arr-method}
\alias{min,matter_arr-method}
\alias{max,matter_arr-method}
\alias{prod,matter_arr-method}
\alias{sum,matter_arr-method}
\alias{mean,matter_arr-method}
\alias{sd,matter_arr-method}
\alias{var,matter_arr-method}
\alias{any,matter_arr-method}
\alias{all,matter_arr-method}

\alias{colMeans,matter_mat-method}
\alias{colSums,matter_mat-method}
\alias{colMeans,sparse_mat-method}
\alias{colSums,sparse_mat-method}

\alias{rowMeans,matter_mat-method}
\alias{rowSums,matter_mat-method}
\alias{rowMeans,sparse_mat-method}
\alias{rowSums,sparse_mat-method}

\alias{colSds}
\alias{colVars}
\alias{colSds,ANY-method}
\alias{colVars,ANY-method}

\alias{rowSds}
\alias{rowVars}
\alias{rowSds,ANY-method}
\alias{rowVars,ANY-method}

\title{Summary Statistics for ``matter'' Objects}

\description{
    These functions efficiently calculate summary statistics for \code{\linkS4class{matter_arr}} objects. For matrices, they operate efficiently on both rows and columns.
}

\usage{
\S4method{range}{matter_arr}(x, \dots, na.rm)
\S4method{min}{matter_arr}(x, \dots, na.rm)
\S4method{max}{matter_arr}(x, \dots, na.rm)
\S4method{prod}{matter_arr}(x, \dots, na.rm)
\S4method{mean}{matter_arr}(x, \dots, na.rm)
\S4method{sum}{matter_arr}(x, \dots, na.rm)
\S4method{sd}{matter_arr}(x, na.rm)
\S4method{var}{matter_arr}(x, na.rm)
\S4method{any}{matter_arr}(x, \dots, na.rm)
\S4method{all}{matter_arr}(x, \dots, na.rm)
\S4method{colMeans}{matter_mat}(x, na.rm, dims = 1, \dots)
\S4method{colSums}{matter_mat}(x, na.rm, dims = 1, \dots)
\S4method{rowMeans}{matter_mat}(x, na.rm, dims = 1, \dots)
\S4method{rowSums}{matter_mat}(x, na.rm, dims = 1, \dots)
}

\arguments{
    \item{x}{A \code{\linkS4class{matter_arr}} object.}

    \item{\dots}{Arguments passed to \code{chunk_lapply()}.}

    \item{na.rm}{If \code{TRUE}, remove \code{NA} values before summarizing.}

    \item{dims}{Not used.}
}

\details{
    These summary statistics methods operate on chunks of data which are loaded into memory and then freed before reading the next chunk.

    For row and column summaries on matrices, the iteration scheme is dependent on the layout of the data. Column-major matrices will always be iterated over by column, and row-major matrices will always be iterated over by row. Row statistics on column-major matrices and column statistics on row-major matrices are calculated iteratively.

    Variance and standard deviation are calculated using a running sum of squares formula which can be calculated iteratively and is accurate for large floating-point datasets (see reference).
}

\value{
    For \code{mean}, \code{sd}, and \code{var}, a single number. For the column summaries, a vector of length equal to the number of columns of the matrix. For the row summaries, a vector of length equal to the number of rows of the matrix.
}

\author{Kylie A. Bemis}

\references{
    B. P. Welford, ``Note on a Method for Calculating Corrected Sums of Squares and Products,'' Technometrics, vol. 4, no. 3, pp. 1-3, Aug. 1962.
}

\seealso{
    \code{\link{stream_stat}}
}

\examples{
x <- matter(1:100, nrow=10, ncol=10)

sum(x)
mean(x)
var(x)
sd(x)

colSums(x)
colMeans(x)

rowSums(x)
rowMeans(x)
}

\keyword{univar}
