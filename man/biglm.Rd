\name{biglm}
\docType{methods}

\alias{biglm}
\alias{biglm,formula,virtual_df-method}
\alias{bigglm}
\alias{bigglm,formula,virtual_df-method}
\alias{bigglm,formula,matter_mat-method}
\alias{bigglm,formula,sparse_mat-method}
\alias{bigglm,formula,virtual_mat-method}

\title{Using ``biglm'' with ``matter''}

\description{
    This method allows \code{\linkS4class{matter_mat}} matrices and \code{\linkS4class{matter_df}} data frames to be used with the \code{\link[biglm]{biglm}} and \code{\link[biglm]{bigglm}} functions from the ``biglm'' package.
}

\usage{
\S4method{biglm}{formula,virtual_df}(formula, data, weights = NULL, sandwich = FALSE)

\S4method{bigglm}{formula,virtual_df}(formula, data, \dots, chunksize = NULL)

\S4method{bigglm}{formula,matter_mat}(formula, data, \dots, chunksize = NULL, fc = NULL)

\S4method{bigglm}{formula,sparse_mat}(formula, data, \dots, chunksize = NULL, fc = NULL)

\S4method{bigglm}{formula,virtual_mat}(formula, data, \dots, chunksize = NULL, fc = NULL)
}

\arguments{
    \item{formula}{A model \code{formula}.}

    \item{data}{A \code{\linkS4class{matter}} matrix with column names.}

    \item{weights}{A one-sided, single-term formula specifying weights.}

    \item{sandwich}{If \code{TRUE}, compute the Huber/White sandwich covariance matrix (uses \code{p^4} memory rather than \code{p^2}).}

    \item{chunksize}{An integer giving the maximum number of rows to process at a time. If left \code{NULL}, this will be calculated by dividing the \code{chunksize} of \code{data} by the number of variables in the formula.}

    \item{fc}{Either column indices or names of variables which are factors.}

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

fit <- bigglm(fm, data=x, chunksize=50)
coef(fit)
}

\keyword{models}
\keyword{regression}
