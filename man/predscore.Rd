\name{predscore}

\alias{predscore}

\title{Score predictive performance}

\description{
    Calculate performance metrics for predictions from classification or regression.
}

\usage{
predscore(x, ref)
}

\arguments{
	\item{x}{The predicted values.}

    \item{ref}{The reference (observed) values.}
}

\value{
    For classification, a numeric matrix with a row for each class and columns called "Recall" and "Precision".

    For regression, a numeric vector with elements named "RMSE", "MAE", and "MAPE".
}

\author{Kylie A. Bemis}

\examples{
set.seed(1)
n <- 250
s <- c("a", "b", "c")
x <- sample(s, n, replace=TRUE)
pred <- ifelse(runif(n) > 0.1, x, sample(s, n, replace=TRUE))
predscore(pred, x)
}

\keyword{utilities}
