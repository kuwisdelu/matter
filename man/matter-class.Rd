\name{matter-class}
\docType{class}

\alias{class:matter}
\alias{matter}
\alias{matter-class}

\alias{datamode}
\alias{datamode<-}
\alias{datamode,atoms-method}
\alias{datamode<-,atoms-method}
\alias{datamode,matter-method}
\alias{datamode<-,matter-method}
\alias{filepaths}
\alias{filepaths<-}
\alias{filepaths,matter-method}
\alias{filepaths<-,matter-method}
\alias{filemode}
\alias{filemode<-}
\alias{filemode,matter-method}
\alias{filemode<-,matter-method}
\alias{chunksize}
\alias{chunksize<-}
\alias{chunksize,matter-method}
\alias{chunksize<-,matter-method}

\alias{length,matter-method}
\alias{length<-,matter-method}
\alias{dim,matter-method}
\alias{dim<-,matter-method}
\alias{names,matter-method}
\alias{names<-,matter-method}
\alias{dimnames,matter-method}
\alias{dimnames<-,matter-method}

\alias{c,atoms-method}
\alias{c,matter-method}

\title{Vectors, Matrices, and Arrays Stored on Disk}

\description{
    The \code{matter} class and its subclasses are designed for easy on-demand read/write access to binary on-disk data structures, and working with them as vectors, matrices, and arrays.
}

\usage{
## Instance creation
matter(\dots)

## Additional methods documented below
}

\arguments{
    \item{\dots}{Arguments passed to subclasses.}
}

\section{Slots}{
    \describe{
        \item{\code{data}:}{This slot stores the information about locations of the data on disk and within the files.}

        \item{\code{datamode}:}{The storage mode of the accessed data when read into R. This should a 'character' vector of length one with value 'integer' or 'numeric'.}

        \item{\code{filepaths}:}{A 'character' vector of the paths to the files where the data are stored.}

        \item{\code{filemode}:}{The read/write mode of the files where the data are stored. This should be 'rb' for read-only access, or 'rb+' for read/write access.}

        \item{\code{chunksize}:}{The maximum number of elements which should be loaded into memory at once. Used by methods implementing summary statistics and linear algebra. Ignored when explicitly subsetting the dataset.}

        \item{\code{length}:}{The length of the data.}

        \item{\code{dim}:}{Either 'NULL' for vectors, or an integer vector of length one of more giving the maximal indices in each dimension for matrices and arrays.}

        \item{\code{names}:}{The names of the data elements for vectors.}

        \item{\code{dimnames}:}{Either 'NULL' or the names for the dimensions. If not 'NULL', then this should be a list of character vectors of the length given by 'dim' for each dimension. This is always 'NULL' for vectors.}
    }
}

\section{Creating Objects}{
    \code{matter} is a virtual class and cannot be instantiated directly, but instances of its subclasses can be created through \code{matter()}.
}

\section{Methods}{
    Class-specific methods:
    \describe{
        \item{\code{datamode(x), datamode(x) <- value}:}{Get or set 'datamode'.}

        \item{\code{filepaths(x), filepaths(x) <- value}:}{Get or set 'filepaths'.}

        \item{\code{filemode(x), filemode(x) <- value}:}{Get or set 'filemode'.}

        \item{\code{chunksize(x), chunksize(x) <- value}:}{Get or set 'filemode'.}
    }

    Standard generic methods:
    \describe{
        \item{\code{dim(x), dim(x) <- value}:}{Get or set 'dim'.}

        \item{\code{names(x), names(x) <- value}:}{Get or set 'names'.}

        \item{\code{dimnames(x), dimnames(x) <- value}:}{Get or set 'dimnames'.}
    }
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{matter_vec}},
    \code{\linkS4class{matter_mat}}
}

\examples{
## Create a vector
x <- matter(1:100, length=100)
x[]

## Create a matrix
x <- matter(1:100, nrow=10, ncol=10)
x[]
}

\keyword{classes}
\keyword{array}
\keyword{IO}
