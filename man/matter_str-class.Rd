\name{matter_str-class}
\docType{class}

\alias{class:matter_str}
\alias{matter_str}
\alias{matter_str-class}

\alias{dim,matter_str-method}
\alias{length,matter_str-method}
\alias{lengths,matter_str-method}

\alias{Encoding,matter_str-method}
\alias{Encoding<-,matter_str-method}

\alias{[,matter_str-method}
\alias{[,matter_str,ANY,ANY,ANY-method}
\alias{[<-,matter_str-method}
\alias{[<-,matter_str,ANY,ANY,ANY-method}

\alias{combine,matter_str,ANY-method}
\alias{c,matter_str-method}

\alias{as.vector,matter_str-method}
\alias{as.character,matter_str-method}

\title{Out-of-Memory Strings}

\description{
    The \code{matter_str} class implements out-of-memory strings.
}

\usage{
## Instance creation
matter_str(data, type = "character", path = NULL,
    nchar = NA_integer_, names = NULL, offset = 0, extent = NA_real_,
    readonly = NA, encoding = "unknown", \dots)

## Additional methods documented below
}

\arguments{
        \item{data}{An optional data vector which will be initially written to virtual memory if provided.}

        \item{type}{Should be 'character' type for characters.}

        \item{path}{A 'character' vector of the path(s) to the file(s) where the data are stored. If 'NULL', then a temporary file is created using \code{\link{tempfile}}.}

        \item{nchar}{A vector giving the length of each element of the character vector.}

        \item{names}{The names of the data elements.}

        \item{offset}{A vector giving the offsets in number of bytes from the beginning of each file in 'path', specifying the start of the data to be accessed for each file.}

        \item{extent}{A vector giving the length of the data for each file in 'path', specifying the number of elements of size 'type' to be accessed from each file.}

        \item{readonly}{Whether the data and file(s) should be treated as read-only or read/write.}

        \item{encoding}{The character encoding to use (if known).}

        \item{\dots}{Additional arguments to be passed to constructor.}
}

\section{Slots}{
    \describe{
        \item{\code{data}:}{This slot stores any information necessary to access the data for the object (which may include the data itself and/or paths to file locations, etc.).}

        \item{\code{type}:}{The storage mode of the \emph{accessed} data when read into R. This is a 'factor' with levels 'raw', 'logical', 'integer', 'numeric', or 'character'.}

        \item{\code{dim}:}{Either 'NULL' for vectors, or an integer vector of length one of more giving the maximal indices in each dimension for matrices and arrays.}

        \item{\code{names}:}{The names of the data elements for vectors.}

        \item{\code{dimnames}:}{Either 'NULL' or the names for the dimensions. If not 'NULL', then this should be a list of character vectors of the length given by 'dim' for each dimension. This is always 'NULL' for vectors.}

        \item{\code{encoding}:}{The string encoding used.}
    }
}

\section{Extends}{
   \code{\linkS4class{matter_list}}
}

\section{Creating Objects}{
    \code{matter_str} instances can be created through \code{matter_str()} or \code{matter()}.
}

\section{Methods}{
    Standard generic methods:
    \describe{
        \item{\code{x[i], x[i] <- value}:}{Get or set the string elements of the vector.}

        \item{\code{lengths(x)}:}{Get the number of characters (in bytes) of all string elements in the vector.}
    }
}

\value{
    An object of class \code{\linkS4class{matter_str}}.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{matter}}
}

\examples{
x <- matter_str(rep(c("hello", "world!"), 50))
x
}

\keyword{classes}
\keyword{array}
\keyword{IO}
