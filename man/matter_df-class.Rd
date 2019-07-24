\name{matter_df-class}
\docType{class}

\alias{class:virtual_tbl}
\alias{virtual_tbl-class}

\alias{class:virtual_df}
\alias{virtual_df}
\alias{virtual_df-class}

\alias{class:matter_df}
\alias{matter_df}
\alias{matter_df-class}

\alias{names<-,virtual_tbl-method}
\alias{dimnames<-,virtual_tbl,ANY-method}

\alias{[,virtual_df-method}
\alias{[,virtual_df,ANY,ANY,ANY-method}
\alias{[,virtual_df,ANY,ANY,NULL-method}
\alias{[<-,virtual_df-method}
\alias{[<-,virtual_df,ANY,ANY,ANY-method}

\alias{[[,virtual_df,ANY,missing-method}
\alias{[[<-,virtual_df,ANY,missing-method}

\alias{$,virtual_df-method}
\alias{$<-,virtual_df-method}

\alias{head,virtual_tbl-method}
\alias{tail,virtual_tbl-method}

\alias{as.data.frame,virtual_df-method}
\alias{as.data.frame,matter_df-method}

\title{Out-of-Memory Data Frames}

\description{
    The \code{virtual_df} class implements lightweight data frames that may be a mixture of atomic vectors and \code{matter} vectors, simulating the behavior of \code{data.frame}.

    The \code{matter_df} class extends \code{virtual_df} to implement fully out-of-memory data frames where all columns are \code{matter} objects.

    Calling \code{as.matter()} on an ordinary R \code{data.frame} will coerce all columns to \code{matter} objects to create a \code{matter_df} data frame.
}

\usage{
## Instance creation
virtual_df(\dots, row.names = NULL, stringsAsFactors = default.stringsAsFactors())

matter_df(\dots, row.names = NULL, stringsAsFactors = default.stringsAsFactors())

## Additional methods documented below
}

\arguments{
        \item{\dots}{These arguments become the data columns or data frame variables. They should be named.}

        \item{row.names}{A character vector giving the row names.}

        \item{stringsAsFactors}{Should character vectors be converted to factors? This is recommended for \code{matter_df}, as accessing the underlying out-of-memory integer vectors (for a factor) is typically much faster than accessing a vector of out-of-memory strings.}
}

\section{Slots}{
    \describe{
        \item{\code{data}:}{This slot stores the information about locations of the data in virtual memory and within the files.}

        \item{\code{datamode}:}{The storage mode of the \emph{accessed} data when read into R. This is a 'character' vector of with possible values 'raw', 'logical', 'integer', 'numeric', or 'virtual'.}

        \item{\code{paths}:}{A 'character' vector of the paths to the files where the data are stored.}

        \item{\code{filemode}:}{The read/write mode of the files where the data are stored. This should be 'r' for read-only access, or 'rw' for read/write access.}

        \item{\code{chunksize}:}{The maximum number of elements which should be loaded into memory at once. Used by methods implementing summary statistics and linear algebra. Ignored when explicitly subsetting the dataset.}

        \item{\code{length}:}{The length of the data.}

        \item{\code{dim}:}{Either 'NULL' for vectors, or an integer vector of length one of more giving the maximal indices in each dimension for matrices and arrays.}

        \item{\code{names}:}{The names of the data elements for vectors.}

        \item{\code{dimnames}:}{Either 'NULL' or the names for the dimensions. If not 'NULL', then this should be a list of character vectors of the length given by 'dim' for each dimension. This is always 'NULL' for vectors.}

        \item{\code{ops}:}{Delayed operations to be applied on atoms.}
    }
}

\section{Extends}{
   \code{\linkS4class{matter}}
}

\section{Creating Objects}{
    \code{virtual_df} instances can be created through \code{virtual_df()}.

    \code{matter_df} instances can be created through \code{matter_df()}.
}

\section{Methods}{
    Standard generic methods:
    \describe{
        \item{\code{x$name, x$name <- value}:}{Get or set a single column.}

        \item{\code{x[[i]], x[[i]] <- value}:}{Get or set a single column.}

        \item{\code{x[i], x[i] <- value}:}{Get or set multiple columns.}

        \item{\code{x[i, j, ..., drop], x[i, j] <- value}:}{Get or set the elements of the data frame.}
    }
}

\value{
    An object of class \code{\linkS4class{virtual_df}} or \code{\linkS4class{matter_df}}.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{matter}}
}

\examples{
x <- matter_df(a=as.matter(1:10), b=11:20, c=as.matter(letters[1:10]))
x
x[1:2]
x[[2]]
x[["c"]]
x[,"c"]
x[1:5,c("a","c")]
x$c
x$c[1:5]
}

\keyword{classes}
\keyword{array}
\keyword{IO}
