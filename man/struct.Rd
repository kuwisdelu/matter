\name{struct}

\alias{struct}

\title{C-Style Structs Stored in Virtual Memory}

\description{
    This is a convenience function for creating and reading C-style structs in a single file stored in virtual memory.
}

\usage{
struct(\dots, filename = NULL, readonly = FALSE, offset = 0)
}

\arguments{
	\item{\dots}{Named integers giving the members of the \code{struct}. They should be of the form \code{name=c(type=length)}.}

	\item{filename}{A single string giving the name of the file.}

	\item{readonly}{Should the file be treated as read-only?}

	\item{offset}{A scalar integer giving the offset from the beginning of the file.}
}

\details{
    This is simply a convenient wrapper around the wrapper around \code{\linkS4class{matter_list}} that allows easy specification of C-style structs in a file.
}

\value{
    A object of class \code{\linkS4class{matter_list}}.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{matter_list}}
}

\examples{
x <- struct(first=c(int=1), second=c(double=1))

x$first <- 2L
x$second <- 3.33

x$first
x$second
}

\keyword{utilities}
\keyword{array}
\keyword{IO}
