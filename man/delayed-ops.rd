\name{delayed-ops}
\docType{methods}

\alias{+,matter_vec,numeric-method}
\alias{+,matter_matc,numeric-method}
\alias{+,matter_matr,numeric-method}
\alias{+,numeric,matter_vec-method}
\alias{+,numeric,matter_matc-method}
\alias{+,numeric,matter_matr-method}

\alias{-,matter_vec,numeric-method}
\alias{-,matter_matc,numeric-method}
\alias{-,matter_matr,numeric-method}
\alias{-,numeric,matter_vec-method}
\alias{-,numeric,matter_matc-method}
\alias{-,numeric,matter_matr-method}

\alias{*,matter_vec,numeric-method}
\alias{*,matter_matc,numeric-method}
\alias{*,matter_matr,numeric-method}
\alias{*,numeric,matter_vec-method}
\alias{*,numeric,matter_matc-method}
\alias{*,numeric,matter_matr-method}

\alias{/,matter_vec,numeric-method}
\alias{/,matter_matc,numeric-method}
\alias{/,matter_matr,numeric-method}
\alias{/,numeric,matter_vec-method}
\alias{/,numeric,matter_matc-method}
\alias{/,numeric,matter_matr-method}

\alias{^,matter_vec,numeric-method}
\alias{^,matter_matc,numeric-method}
\alias{^,matter_matr,numeric-method}
\alias{^,numeric,matter_vec-method}
\alias{^,numeric,matter_matc-method}
\alias{^,numeric,matter_matr-method}

\alias{exp,matter_vec-method}
\alias{exp,matter_mat-method}

\alias{log,matter_vec-method}
\alias{log,matter_matc-method}
\alias{log,matter_matr-method}
\alias{log,matter_vec,numeric-method}
\alias{log,matter_matc,numeric-method}
\alias{log,matter_matr,numeric-method}

\alias{log2,matter_vec-method}
\alias{log2,matter_mat-method}

\alias{log10,matter_vec-method}
\alias{log10,matter_mat-method}


\title{Delayed Operations on ``matter'' Objects}

\description{
    Some arithmetic operations are available as delayed operations on \code{\linkS4class{matter}} objects. With these operations, no data is changed on disk, and the operation is only executed when elements of the object are actually accessed.
}

\details{
    Currently the following operations are supported:

    `Arith': `+', `-', `*', `/', `^'

    `Math': `exp', `log', `log2', `log10'

    Delayed operations are applied at the C++ layer immediately after the elements are read from disk. This means that operations that are implemented in C and/or C++ for efficiency (such as summary statistics) will also reflect the execution of the delayed operations.
}

\value{
    A new \code{\linkS4class{matter}} object with the registered delayed operation. Data on disk is not modified; only object metadata is changed.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link[methods]{Arith}},
    \code{\link[methods]{Math}}
}

\examples{
x <- matter(1:100, length=100)
y <- x + 1

x[1:10]
y[1:10]

mean(x)
mean(y)
}

\keyword{methods}
\keyword{arith}
