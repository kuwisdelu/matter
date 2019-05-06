
# streaming statistical summaries

s_range <- function(x, ..., na.rm = FALSE) {
	if ( length(list(...)) > 0L ) {
		x <- s_range(x, na.rm=na.rm)
		return(stat_c(x, ...))
	}
	if ( !is.stream_stat(x) ) {
		structure(range(x, na.rm=na.rm),
			class=c("stream_range", "stream_stat"),
			na.rm=na.rm,
			nobs=length(x))
	} else {
		x
	}
}

s_min <- function(x, ..., na.rm = FALSE) {
	if ( length(list(...)) > 0L ) {
		x <- s_min(x, na.rm=na.rm)
		return(stat_c(x, ...))
	}
	if ( !is.stream_stat(x) ) {
		structure(min(x, na.rm=na.rm),
			class=c("stream_min", "stream_stat"),
			na.rm=na.rm,
			nobs=length(x))
	} else {
		x
	}
}

s_max <- function(x, ..., na.rm = FALSE) {
	if ( length(list(...)) > 0L ) {
		x <- s_max(x, na.rm=na.rm)
		return(stat_c(x, ...))
	}
	if ( !is.stream_stat(x) ) {
		structure(max(x, na.rm=na.rm),
			class=c("stream_max", "stream_stat"),
			na.rm=na.rm,
			nobs=length(x))
	} else {
		x
	}
}

s_prod <- function(x, ..., na.rm = FALSE) {
	if ( length(list(...)) > 0L ) {
		x <- s_prod(x, na.rm=na.rm)
		return(stat_c(x, ...))
	}
	if ( !is.stream_stat(x) ) {
		structure(prod(x, na.rm=na.rm),
			class=c("stream_prod", "stream_stat"),
			na.rm=na.rm,
			nobs=length(x))
	} else {
		x
	}
}

s_sum <- function(x, ..., na.rm = FALSE) {
	if ( length(list(...)) > 0L ) {
		x <- s_sum(x, na.rm=na.rm)
		return(stat_c(x, ...))
	}
	if ( !is.stream_stat(x) ) {
		structure(sum(x, na.rm=na.rm),
			class=c("stream_sum", "stream_stat"),
			na.rm=na.rm,
			nobs=length(x))
	} else {
		x
	}
}

s_mean <- function(x, ..., na.rm = FALSE) {
	if ( length(list(...)) > 0L ) {
		x <- s_mean(x, na.rm=na.rm)
		return(stat_c(x, ...))
	}
	if ( !is.stream_stat(x) ) {
		structure(mean(x, na.rm=na.rm),
			class=c("stream_mean", "stream_stat"),
			na.rm=na.rm,
			nobs=length(x))
	} else {
		x
	}
}

s_var <- function(x, ..., na.rm = FALSE) {
	if ( length(list(...)) > 0L ) {
		x <- s_var(x, na.rm=na.rm)
		return(stat_c(x, ...))
	}
	if ( !is.stream_stat(x) ) {
		structure(var(x, na.rm=na.rm),
			class=c("stream_var", "stream_stat"),
			mean=mean(x, na.rm=na.rm),
			na.rm=na.rm,
			nobs=length(x))
	} else {
		x
	}
}

s_sd <- function(x, ..., na.rm = FALSE) {
	if ( length(list(...)) > 0L ) {
		x <- s_sd(x, na.rm=na.rm)
		return(stat_c(x, ...))
	}
	if ( !is.stream_stat(x) ) {
		structure(sd(x, na.rm=na.rm),
			class=c("stream_sd", "stream_stat"),
			mean=mean(x, na.rm=na.rm),
			na.rm=na.rm,
			nobs=length(x))
	} else {
		x
	}
}

s_any <- function(x, ..., na.rm = FALSE) {
	if ( length(list(...)) > 0L ) {
		x <- s_any(x, na.rm=na.rm)
		return(stat_c(x, ...))
	}
	if ( !is.stream_stat(x) ) {
		structure(any(x, na.rm=na.rm),
			class=c("stream_any", "stream_stat"),
			na.rm=na.rm,
			nobs=length(x))
	} else {
		x
	}
}

s_all <- function(x, ..., na.rm = FALSE) {
	if ( length(list(...)) > 0L ) {
		x <- s_all(x, na.rm=na.rm)
		return(stat_c(x, ...))
	}
	if ( !is.stream_stat(x) ) {
		structure(all(x, na.rm=na.rm),
			class=c("stream_all", "stream_stat"),
			na.rm=na.rm,
			nobs=length(x))
	} else {
		x
	}
}

# register for S4 methods

setOldClass(c("stream_range", "stream_stat"))

setOldClass(c("stream_min", "stream_stat"))

setOldClass(c("stream_max", "stream_stat"))

setOldClass(c("stream_prod", "stream_stat"))

setOldClass(c("stream_sum", "stream_stat"))

setOldClass(c("stream_mean", "stream_stat"))

setOldClass(c("stream_var", "stream_stat"))

setOldClass(c("stream_sd", "stream_stat"))

setOldClass(c("stream_any", "stream_stat"))

setOldClass(c("stream_all", "stream_stat"))

# streaming statistics methods

is.stream_stat <- function(x) is(x, "stream_stat")

print.stream_stat <- function(x, ...) {
	cat(class(x)[1L], "with n =", nobs(x), "\n")
	print(c(x), ...)
}

nobs.stream_stat <- function(object, ...) {
	attr(object, "nobs")
}

setMethod("nobs", "stream_stat", nobs.stream_stat)

na_rm <- function(object, ...) UseMethod("na_rm")

na_rm.default <- function(object, ...) attr(object, "na.rm")

stat_c <- function(x, y, ...) {
	if ( length(list(...)) > 0 ) {
		stat_c(x, do.call(stat_c, list(y, ...)))
	} else {
		UseMethod("stat_c")
	}
}

setMethod("combine", "stream_stat",
	function(x, y, ...) stat_c(x, y, ...))

stream_stat_attr <- function(value, x, y) {
	structure(value,
		class=class(x),
		na.rm=na_rm(x) && na_rm(y),
		nobs=nobs(x) + nobs(y))
}

# combine statistics

stat_c.default <- function(x, y, ...) {
	if ( is.stream_stat(y) ) {
		stat_c(y, x)
	} else {
		c(x, y)
	}
}

stat_c.stream_range <- function(x, y, ...) {
	if ( !inherits(y, class(x)) )
		y <- s_range(y, na.rm=na_rm(x))
	val <- range(c(x, y), na.rm=FALSE)
	stream_stat_attr(val, x, y)
}

stat_c.stream_min <- function(x, y, ...) {
	if ( !inherits(y, class(x)) )
		y <- s_min(y, na.rm=na_rm(x))
	val <- min(c(x, y), na.rm=FALSE)
	stream_stat_attr(val, x, y)
}

stat_c.stream_max <- function(x, y, ...) {
	if ( !inherits(y, class(x)) )
		y <- s_max(y, na.rm=na_rm(x))
	val <- max(c(x, y), na.rm=FALSE)
	stream_stat_attr(val, x, y)
}

stat_c.stream_prod <- function(x, y, ...) {
	if ( !inherits(y, class(x)) )
		y <- s_prod(y, na.rm=na_rm(x))
	val <- prod(c(x, y), na.rm=FALSE)
	stream_stat_attr(val, x, y)
}

stat_c.stream_sum <- function(x, y, ...) {
	if ( !inherits(y, class(x)) )
		y <- s_sum(y, na.rm=na_rm(x))
	val <- sum(c(x, y), na.rm=FALSE)
	stream_stat_attr(val, x, y)
}

stat_c.stream_mean <- function(x, y, ...) {
	if ( !inherits(y, class(x)) )
		y <- s_mean(y, na.rm=na_rm(x))
	nx <- nobs(x)
	ny <- nobs(y)
	val <- (nx * x + ny * y) / (nx + ny)
	stream_stat_attr(val, x, y)
}

stat_c.stream_var <- function(x, y, ...) {
	if ( !inherits(y, class(x)) )
		y <- s_var(y, na.rm=na_rm(x))
	nx <- nobs(x)
	ny <- nobs(y)
	mx <- attr(x, "mean")
	my <- attr(y, "mean")
	m <- (nx * mx + ny * my) / (nx + ny)
	if ( nx == 1 && ny > 1 )
		return(stat_c(y, x))
	if ( nx >= 1 && ny == 1 ) {
		if ( nx == 1 ) {
			ss1 <- 0
		} else {
			ss1 <- ((nx - 1) * x) 
		}
		ss2 <- ss1 + (my - mx) * (my - m)
		val <- ss2 / (nx + ny - 1)
	} else {
		num1 <- ((nx - 1) * x) + ((ny - 1) * y)
		num2 <- (nx * ny / (nx + ny)) * (mx - my)^2
		val <- (num1 + num2) / (nx + ny - 1)
	}
	ret <- stream_stat_attr(val, x, y)
	attr(ret, "mean") <- m
	ret
}

stat_c.stream_sd <- function(x, y, ...) {
	if ( !inherits(y, class(x)) )
		y <- s_sd(y, na.rm=na_rm(x))
	nx <- nobs(x)
	ny <- nobs(y)
	mx <- attr(x, "mean")
	my <- attr(y, "mean")
	m <- (nx * mx + ny * my) / (nx + ny)
	if ( nx == 1 && ny > 1 )
		return(stat_c(y, x))
	if ( nx >= 1 && ny == 1 ) {
		if ( nx == 1 ) {
			ss1 <- 0
		} else {
			ss1 <- ((nx - 1) * x^2) 
		}
		ss2 <- ss1 + (my - mx) * (my - m)
		val <- sqrt(ss2 / (nx + ny - 1))
	} else {
		num1 <- ((nx - 1) * x^2) + ((ny - 1) * y^2)
		num2 <- (nx * ny / (nx + ny)) * (mx - my)^2
		val <- sqrt((num1 + num2) / (nx + ny - 1))
	}
	ret <- stream_stat_attr(val, x, y)
	attr(ret, "mean") <- m
	ret
}

stat_c.stream_any <- function(x, y, ...) {
	if ( !inherits(y, class(x)) )
		y <- s_any(y, na.rm=na_rm(x))
	val <- any(c(x, y), na.rm=FALSE)
	stream_stat_attr(val, x, y)
}

stat_c.stream_all <- function(x, y, ...) {
	if ( !inherits(y, class(x)) )
		y <- s_all(y, na.rm=na_rm(x))
	val <- all(c(x, y), na.rm=FALSE)
	stream_stat_attr(val, x, y)
}

