\name{cv_do}

\alias{cv_do}
\alias{fitted.cv}

\title{Perform Cross Validation}

\description{
    Perform k-fold cross-validation with an arbitrary modeling function.
}

\usage{
cv_do(fit., x, y, folds, \dots,
	mi = !is.null(bags), bags = NULL, pos = 1L,
	predict. = predict, transpose = FALSE, keep.models = TRUE,
	trainProcess = NULL, trainArgs = list(),
	testProcess = NULL, testArgs = list(),
	verbose = NA, nchunks = NA, chunksize = NA,
	BPPARAM = bpparam())

\S3method{fitted}{cv}(object, type = c("response", "class"),
	simplify = TRUE, \dots)
}

\arguments{
	\item{fit.}{The function used to fit the model.}

	\item{x, y}{The data and response variable.}

	\item{folds}{A vector coercible to a factor giving the fold for each row or column of \code{x}.}

	\item{mi}{Should \code{\link{mi_learn}} be called with \code{fit.} for multiple instance learning?}

	\item{bags}{If provided, subsetted and passed to \code{fit.} or \code{\link{mi_learn}} if \code{mi=TRUE}.}

	\item{pos}{The positive class for multiple instance learning. Only used if \code{mi=TRUE}.}

	\item{\dots}{Additional arguments passed to \code{fit.} and \code{predict.}.}

	\item{predict.}{The function used to predict on new data from the fitted model. The fitted model is passed as the 1st argument and the test data is passed as the 2nd argument.}

    \item{transpose}{A logical value indicating whether \code{x} should be considered transposed or not. This can be useful if the input matrix is (P x N) instead of (N x P) and storing the transpose is expensive. This is not necessary for \code{\linkS4class{matter_mat}} and \code{\linkS4class{sparse_mat}} objects, but can be useful for large in-memory (P x N)  matrices.}

    \item{keep.models}{Should the models be kept and returned?}

    \item{trainProcess, trainArgs}{A function and arguments used for processing the training sets. The training set is passed as the 1st argument to \code{trainProcess}.}

    \item{testProcess, testArgs}{A function and arguments used for processing the test sets. The test set is passed as the 1st argument to \code{trainProcess}, and the processed training set is passed as the 2nd argument.}

	\item{verbose}{Should progress be printed for each iteration?}

	\item{nchunks}{The number of chunks to use. \emph{Passed} to \code{fit.}, \code{predict.}, \code{trainProcess} and \code{testProcess}.}

	\item{chunksize}{The approximate chunk size in bytes. \emph{Passed} to \code{fit.}, \code{predict.}, \code{trainProcess} and \code{testProcess}.}

	\item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}. \emph{Passed} to \code{fit.}, \code{predict.}, \code{trainProcess} and \code{testProcess}.}

	\item{object}{An object inheriting from \code{cv}.}

	\item{type}{The type of prediction, where \code{"response"} means the fitted response matrix and \code{"class"} will be the vector of class predictions (only for classification).}

	\item{simplify}{Should the predictions be simplified (from a list) to an array (\code{type="response"}) or data frame (\code{type="class"})?}
}

\details{
    The cross-validation is not performed in parallel, because it is assumed the pre-processing functions, modeling function, and prediction function may make use of parallelization. Therefore, these functions need to be able to handle (or ignore) the arguments \code{nchunks} and \code{BPPARAM}, which will be passed to them.

    If \code{bags} is specified, then multiple instance learning is assumed, where observations from the same bag are all assumed to have the same label. The labels for bags are automatically pooled (from \code{y}) so that if any observation in a bag is \code{pos}, then the entire bag is labeled \code{pos}. If \code{mi=TRUE} then \code{\link{mi_learn}} will be called by \code{cv_do}; otherwise it is assumed \code{fn} will handle the multiple instance learning.  The accuracy metrics are calculated with the original \code{y} labels.
}

\value{
    An object of class \code{cv}, with the following components:

    \itemize{
        \item{\code{average}: The average accuracy metrics.}
        \item{\code{scores}: The fold-specific accuracy metrics.}
        \item{\code{folds}: The fold memberships.}
        \item{\code{fitted.values}: The fold-specific predictions.}
        \item{\code{models}: (Optional) The fitted models.}
    }
}

\author{Kylie A. Bemis}

\seealso{
	\code{\link{predscore}}
}

\examples{
register(SerialParam())

set.seed(1)
n <- 100
p <- 5
nfolds <- 3
y <- rep(c(rep.int("yes", 60), rep.int("no", 40)), nfolds)
x <- matrix(rnorm(nfolds * n * p), nrow=nfolds * n, ncol=p)
x[,1L] <- x[,1L] + 2 * ifelse(y == "yes", runif(n), -runif(n))
x[,2L] <- x[,2L] + 2 * ifelse(y == "no", runif(n), -runif(n))
folds <- rep(paste0("set", seq_len(nfolds)), each=n)

cv_do(pls_nipals, x, y, k=1:5, folds=folds)
}

\keyword{utilities}
\keyword{regression}
\keyword{classif}
