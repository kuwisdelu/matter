\name{biglm}
\docType{methods}

\alias{bigglm}
\alias{bigglm,formula,matter_mat-method}
\alias{bigglm,formula,sparse_mat-method}

\title{Linear Regression for ``matter'' Matrices}

\description{
    This method allows bounded memory linear regression with \code{\linkS4class{matter_mat}} and \code{\linkS4class{sparse_mat}} matrices using the ``biglm'' package.
}

\usage{
\S4method{bigglm}{formula,matter_mat}(formula, data, \dots, nchunks = NA, verbose = FALSE)

\S4method{bigglm}{formula,sparse_mat}(formula, data, \dots, nchunks = NA, verbose = FALSE)
}

\arguments{
    \item{formula}{A model \code{formula}.}

    \item{data}{A \code{\linkS4class{matter}} matrix with column names.}

    \item{nchunks}{The number of chunks to use. If \code{NA} (the default), this is taken from \code{getOption("matter.default.nchunks")}. For IO-bound operations, using fewer chunks will often be faster, but use more memory.}

    \item{verbose}{Should user messages be printed with the current chunk being processed?}

    \item{\dots}{Additional options passed to \code{\link[biglm]{bigglm}}.}
}

\value{
	An object of class \code{\link[biglm]{bigglm}}.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link[biglm]{bigglm}}
}

\examples{
set.seed(1)

x <- matter_mat(rnorm(1000), nrow=100, ncol=10)

colnames(x) <- c(paste0("x", 1:9), "y")

fm <- paste0("y ~ ", paste0(paste0("x", 1:9), collapse=" + "))
fm <- as.formula(fm)

fit <- bigglm(fm, x, nchunks=10)
coef(fit)
}

\keyword{models}
\keyword{regression}
