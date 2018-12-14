\name{matter_df-class}
\docType{class}

\alias{class:matter_df}
\alias{matter_df}
\alias{matter_df-class}

\alias{names<-,matter_tbl-method}
\alias{dimnames<-,matter_tbl,ANY-method}

\alias{[,matter_df-method}
\alias{[,matter_df,ANY,ANY,ANY-method}
\alias{[,matter_df,ANY,ANY,NULL-method}
\alias{[,matter_df,ANY,missing,ANY-method}
\alias{[,matter_df,ANY,missing,NULL-method}
\alias{[,matter_df,missing,ANY,ANY-method}
\alias{[,matter_df,missing,ANY,NULL-method}
\alias{[,matter_df,missing,missing,ANY-method}
\alias{[<-,matter_df-method}
\alias{[<-,matter_df,ANY,ANY,ANY-method}
\alias{[<-,matter_df,ANY,missing,ANY-method}
\alias{[<-,matter_df,missing,ANY,ANY-method}
\alias{[<-,matter_df,missing,missing,ANY-method}

\alias{[[,matter_df,ANY,missing-method}
\alias{[[<-,matter_df,ANY,missing-method}

\alias{$,matter_df-method}
\alias{$<-,matter_df-method}

\alias{head,matter_tbl-method}
\alias{tail,matter_tbl-method}

\alias{as.data.frame,matter_df-method}

\title{Data Frames Stored on Disk}

\description{
    The \code{matter_df} class implements on-disk data frames.
}

\usage{
## Instance creation
matter_df(\dots, row.names = NULL)

## Additional methods documented below
}

\arguments{
        \item{\dots}{These arguments become the data columns or data frame variables. They should be named.}

        \item{row.names}{A character vector giving the row names.}
}

\section{Slots}{
    \describe{
        \item{\code{data}:}{This slot stores the information about locations of the data on disk and within the files.}

        \item{\code{datamode}:}{The storage mode of the \emph{accessed} data when read into R. This is a 'character' vector of length one with value 'integer' or 'numeric'.}

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
    \code{matter_df} instances can be created through \code{matter_df()} or \code{matter()}.
}

\section{Methods}{
    Standard generic methods:
    \describe{
        \item{\code{x$name, x$name <- value}:}{Get or set the data columns.}

        \item{\code{x[[i]], x[[i]] <- value}:}{Get or set the data columns.}

        \item{\code{x[i, j, ..., drop], x[i, j] <- value}:}{Get or set the elements of the data frame.}
    }
}

\value{
    An object of class \code{\linkS4class{matter_df}}.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{matter}}
}

\examples{
x <- matter_df(a=as.matter(1:10), b=as.matter(1:10))
x[]
x[[1]]
x[["a"]]
x[,"a"]
x[1:5,c("a","b")]
x$a
x$a[1:10]
}

\keyword{classes}
\keyword{array}
\keyword{IO}
