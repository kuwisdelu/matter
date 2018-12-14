\name{virtual_mat-class}
\docType{class}

\alias{class:virtual_mat}
\alias{virtual_mat}
\alias{virtual_matc}
\alias{virtual_matr}
\alias{virtual_mat-class}
\alias{virtual_matc-class}
\alias{virtual_matr-class}

\alias{[,virtual_mat-method}
\alias{[,virtual_mat,ANY,ANY,ANY-method}
\alias{[,virtual_mat,ANY,ANY,NULL-method}
\alias{[,virtual_mat,ANY,missing,ANY-method}
\alias{[,virtual_mat,ANY,missing,NULL-method}
\alias{[,virtual_mat,missing,ANY,ANY-method}
\alias{[,virtual_mat,missing,ANY,NULL-method}
\alias{[,virtual_mat,missing,missing,ANY-method}

\alias{datamode<-,virtual_mat-method}

\alias{t,virtual_mat-method}

\alias{\%*\%,matrix,virtual_matc-method}
\alias{\%*\%,matrix,virtual_matr-method}
\alias{\%*\%,virtual_matc,matrix-method}
\alias{\%*\%,virtual_matr,matrix-method}

\alias{as.matrix,virtual_mat-method}

\alias{is.virtual}
\alias{as.virtual}

\title{Virtual Matrices}

\description{
    The \code{virtual_mat} class implements virtual matrices, which may hold any matrix objects. It is provided primarily to allow combining of \code{matter} matrix classes that could not be combined otherwise.
}

\usage{
## Instance creation
virtual_mat(data, datamode = "double", rowMaj = FALSE,
            dimnames = NULL, index = NULL, \dots)

# Check if an object is a virtual matrix
is.virtual(x)

# Coerce an object to a virtual matrix
as.virtual(x, \dots)

## Additional methods documented below
}

\arguments{
        \item{data}{A list of matrices to combine.}

        \item{datamode}{A 'character' vector giving the storage mode of the data on disk. Allowable values are R numeric and logical types ('logical', 'integer', 'numeric') and their C equivalents.}

        \item{rowMaj}{Whether the matrices in \code{data} are combined by row (\code{TRUE}) or by column (\code{FALSE}.}

        \item{dimnames}{The names of the virtual matrix dimensions.}

        \item{index}{A length-2 list of row and column indices giving a submatrix, if desired.}

        \item{x}{An object to check if it is a virtual matrix or coerce to a virtual matrix.}

        \item{\dots}{Additional arguments to be passed to constructor.}
}

\section{Slots}{
    \describe{
        \item{\code{data}:}{This slot stores the information about locations of the data on disk and within the files.}

        \item{\code{datamode}:}{The storage mode of the accessed data when read into R. This should a 'character' vector of length one with value 'integer' or 'numeric'.}

        \item{\code{paths}:}{A 'character' vector of the paths to the files where the data are stored.}

        \item{\code{filemode}:}{The read/write mode of the files where the data are stored. This should be 'r' for read-only access, or 'rw' for read/write access.}

        \item{\code{chunksize}:}{The maximum number of elements which should be loaded into memory at once. Used by methods implementing summary statistics and linear algebra. Ignored when explicitly subsetting the dataset.}

        \item{\code{length}:}{The length of the data.}

        \item{\code{dim}:}{Either 'NULL' for vectors, or an integer vector of length one of more giving the maximal indices in each dimension for matrices and arrays.}

        \item{\code{names}:}{The names of the data elements for vectors.}

        \item{\code{dimnames}:}{Either 'NULL' or the names for the dimensions. If not 'NULL', then this should be a list of character vectors of the length given by 'dim' for each dimension. This is always 'NULL' for vectors.}

        \item{\code{ops}:}{Delayed operations to be applied on atoms.}

        \item{index}{A length-2 list of row and column indices giving a virtual submatrix.}

        \item{transpose}{\code{TRUE} if the virtual matrix should be transposed, and \code{FALSE} otherwise.}
    }
}

\section{Extends}{
   \code{\linkS4class{matter}}
}

\section{Creating Objects}{
    \code{virtual_mat} instances can be created through \code{virtual_mat()}.
}

\section{Methods}{
    Standard generic methods:
    \describe{
        \item{\code{x[i, j, ..., drop]}:}{Get or set the elements of the virtual matrix. Use \code{drop = NULL} to return a subset of the same class as the object.}

        \item{\code{cbind(x, ...), rbind(x, ...)}:}{Combine virtual matrices by row or column.}

        \item{\code{t(x)}:}{Transpose a matrix. This is a quick operation which only changes metadata and does not touch the data representation.}
    }
}

\value{
    An object of class \code{\linkS4class{virtual_mat}}.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\linkS4class{matter}}
}

\examples{
x <- matrix(runif(50), nrow=10, ncol=5)

x <- virtual_mat(list(x, x))
x[]
}

\keyword{classes}
\keyword{array}
