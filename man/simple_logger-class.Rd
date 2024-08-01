\name{simple_logger-class}
\docType{class}

\alias{class:simple_logger}
\alias{simple_logger}
\alias{simple_logger-class}

\alias{matter_logger}

\alias{path,simple_logger-method}
\alias{path<-,simple_logger-method}

\title{Simple Logging}

\description{
    A simple logger that uses R's built-in signaling conditions.
}

\usage{
## Instance creation
simple_logger(file = NULL, domain = NULL, bufferlimit = 50L)

## Additional methods documented below
}

\arguments{
    \item{file}{The name of the log file. If \code{NULL}, then a temporary file is created using \code{\link{tempfile}}.}

    \item{domain}{See \code{\link{gettext}} for details. If \code{NA}, log entries will not be translated.}

    \item{bufferlimit}{The maximum number of buffered log entries before they are flushed to the log file.}
}

\section{Fields}{
    \describe{
        \item{\code{id}:}{An identifier for file synchronization.}

        \item{\code{buffer}:}{A character vector of log entries that that have not been flushed to the log file.}

        \item{\code{bufferlimit}:}{The maximum number of buffered log entries before they are flushed to the log file.}

        \item{\code{logfile}:}{The path to the log file.}

        \item{\code{domain}:}{See \code{\link{gettext}} for details. If \code{NA}, log entries will not be translated.}
    }
}

\section{Creating Objects}{
    \code{simple_logger} instances can be created through \code{simple_logger()}, respectively.
}

\section{Methods}{
    Standard generic methods:
    \describe{
        \item{\code{$flush():}}{Flush buffered log entries to the log file.}

        \item{\code{$append(entry):}}{Append a raw entry to the log.}

        \item{\code{$append_session():}}{Append the \code{sessionInfo()} to the log.}

        \item{\code{$log(\dots, echo = FALSE):}}{Record a log entry with a timestamp, optionally signaling a \code{message} condition if \code{echo=TRUE}.}

        \item{\code{$message(\dots):}}{Record a log entry with a timestamp and signal a \code{message} condition.}

        \item{\code{$warning(\dots):}}{Record a log entry with a timestamp and signal a \code{warning} condition.}

        \item{\code{$stop(\dots):}}{Record a log entry with a timestamp and signal an \code{error} condition.}

        \item{\code{$move(file):}}{Move the log file to a new location.}

        \item{\code{$close(file):}}{Flush buffered log entries (including \code{sessionInfo()}) and detach the log file. This is called automatically at the end of an R session. (Note that log files in the temporary directory will be deleted after the R session ends if they are not \code{$move()}ed to a persistent directory before quitting.)}

        \item{\code{path(x), path(x) <- value:}}{Get or set the path to the log file. The replacement method is the same as \code{$move()}.}
    }
}

\value{
    An object derived from class \code{simple_logger}.
}

\author{Kylie A. Bemis}

\examples{
sl <- simple_logger()

sl$log("This is a basic log entry that doesn't signal a condition.")
sl$message("This log entry will signal a message condition")
sl$flush()

readLines(path(sl))
}

\keyword{utilities}
\keyword{debugging}
