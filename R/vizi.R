
#### Define visual encoding classes for 'vizi' ####
## ------------------------------------------------

vizi <- function(data, ..., encoding = NULL, params = NULL)
{
	encoding <- merge_encoding(encoding, as_encoding(...))
	props <- compute_properties(encoding, data=data)
	encoding <- props$encoding
	channels <- props$channels
	coord <- list(xlim=NULL, ylim=NULL, log="", asp=NA, grid=TRUE)
	structure(list(encoding=encoding, channels=channels,
		marks=list(), coord=coord, params=params), class="vizi_plot")
}

setOldClass("vizi_plot")

add_mark <- function(plot, mark,
	encoding = NULL, data = NULL, ..., trans = NULL)
{
	if ( !is(plot, "vizi_plot") )
		stop("'plot' must inherit from 'vizi_plot'")
	cls <- paste0("vizi_", mark)
	if ( !existsMethod("plot", cls) )
		stop("no known plot() method for class: ", sQuote(cls))
	if ( !is.null(encoding) ) {
		props <- compute_properties(normalize_encoding(encoding), data=data)
		plot$channels <- merge_channels(props$channels, plot$channels)
		encoding <- props$encoding
	}
	params <- normalize_encoding(as_encoding(...))
	mk <- list(encoding=encoding, params=params, trans=trans)
	mk <- setNames(list(structure(mk, class=cls)), mark)
	plot$marks <- c(plot$marks, mk)
	plot
}

# facet <- function(plot, encoding = NULL, data = NULL,
# 	nrow = NA, ncol = NA, labels = NULL, drop = TRUE)
# {
# 	if ( !inherits(plot, c("vizi_plot", "vizi_facet")) )
# 		stop("'plot' must inherit from 'vizi_plot' or 'vizi_facet")
# 	if ( is(encoding, "AsIs") )
# 		encoding <- list(encoding)
# 	if ( is(encoding, "formula") ) {
# 		encoding <- parse_formula(encoding)
# 		encoding <- c(encoding$lhs, encoding$rhs)
# 	}
# 	encoding <- compute_variables(encoding, data=data)
# 	dim <- as.integer(c(nrow, ncol))
# 	if ( any(!is.na(dim)) ) {
# 		nshingles <- max(dim, na.rm=TRUE)
# 	} else {
# 		nshingles <- 6
# 	}
# 	subscripts <- compute_subscripts(encoding, nshingles)
# 	plot$facets <- structure(list(index=index, dim=dim,
# 		labels=labels, drop=drop), class="vizi_facet")
# 	plot
# }

#### Plotting methods for 'vizi' ####
## ----------------------------------

preplot.vizi_plot <- function(object, ...)
{
	par(vizi_par())
	xlim <- get_xlim(object)
	ylim <- get_ylim(object)
	plot.new()
	localWindow <- function(..., col, bg, pch, cex, lty, lwd) plot.window(...)
	args <- list(xlim=xlim, ylim=ylim,
		log=object$coord$log,
		asp=object$coord$asp)
	args <- c(args, object$params, list(...))
	do.call(localWindow, args)
	if ( isTRUE(object$coord$grid) )
		grid()
}

plot.vizi_plot <- function(x, ..., add = FALSE)
{
	localAxis <- function(..., col, bg, pch, cex, lty, lwd) Axis(...)
	localTitle <- function(..., col, bg, pch, cex, lty, lwd) title(...)
	localBox <- function(..., col, bg, pch, cex, lty, lwd) box(...)
	if ( !add ) {
		dev.hold()
		on.exit(dev.flush())
		preplot(x)
	}
	for ( m in x$marks )
		plot(m, plot=x, ..., add=TRUE)
	if ( !add ) {
		localAxis(x$encoding$x, side=1, ...)
		localAxis(x$encoding$y, side=2, ...)
		localTitle(xlab=x$channels$x$label,
			ylab=x$channels$y$label, outer=TRUE, ...)
	}
	localBox(...)
}

setMethod("plot", "vizi_plot", plot.vizi_plot)

plot_xy <- function(mark, plot = NULL, ..., add = FALSE)
{
	encoding <- merge_encoding(plot$encoding, mark$encoding)
	x <- encode("x", encoding, plot$channels)
	y <- encode("y", encoding, plot$channels)
	stopifnot(!is.null(x), !is.null(y))
	t <- mark$trans
	if ( !is.null(t$nmax) && t$nmax < length(x) ) {
		if ( !is.null(t$sampler) ) {
			y <- downsample(y, n=t$nmax, domain=x, method=t$sampler)
		} else {
			y <- downsample(y, n=t$nmax, domain=x)
		}
		i <- attr(y, "index")
		x <- x[i]
	} else {
		i <- NULL
	}
	p <- c("pch", "col", "bg", "cex", "lwd", "lty")
	p <- lapply(setNames(p, p), encode, encoding=encoding,
		channels=plot$channels, params=mark$params, subset=i)
	if ( !add ) {
		plot.new()
		plot.window(xlim=range(x), ylim=range(y))
	}
	plot.xy(xy.coords(x, y), pch=p$pch, col=p$col, bg=p$bg,
		cex=p$cex, lwd=p$lwd, lty=p$lty, ...)
}

plot.vizi_points <- function(x, plot = NULL, ..., add = FALSE)
{
	plot_xy(mark=x, plot=plot, type="p", ..., add=add)
}

plot.vizi_lines <- function(x, plot = NULL, ..., add = FALSE)
{
	plot_xy(mark=x, plot=plot, type="l", ..., add=add)
}

plot.vizi_peaks <- function(x, plot = NULL, ..., add = FALSE)
{
	plot_xy(mark=x, plot=plot, type="h", ..., add=add)
}

setOldClass("vizi_points")
setOldClass("vizi_lines")
setOldClass("vizi_peaks")

setMethod("plot", "vizi_points", plot.vizi_points)
setMethod("plot", "vizi_lines", plot.vizi_lines)
setMethod("plot", "vizi_peaks", plot.vizi_peaks)

#### Internal functions for vizi ####
## ----------------------------------

as_encoding <- function(x, y, ..., env = NULL)
{
	args <- list(...)
	if ( !missing(y) )
		args <- c(list(y=y), args)
	if ( !missing(x) )
		args <- c(list(x=x), args)
	encoding <- lapply(args, function(e)
	{
		if ( is(e, "formula") && !is.null(env) )
			environment(e) <- env
		e
	})
	normalize_encoding(encoding)
}

normalize_encoding <- function(e)
{
	if ( length(e) == 0L )
		return(NULL)
	nms <- names(e)
	nms <- vapply(nms, function(nm)
	{
		switch(nm,
			shape = "pch",
			color = "col",
			colour = "col",
			fill = "bg",
			size = "cex",
			linewidth = "lwd",
			linetype = "lty",
			lineend = "lend",
			linejoin = "ljoin",
			linemitre = "lmitre",
			nm)
	}, character(1))
	names(e) <- nms
	e
}

merge_encoding <- function(e1, e2, ...)
{
	if ( ...length() > 0L )
		e2 <- do.call(merge_encoding, list(e2, ...))
	if ( !is.null(e1) && !is.null(e2) ) {
		e <- e1
		for ( nm in names(e2) ) {
			# merge e2 into e1 => e2 takes priority
			e[[nm]] <- e2[[nm]]
		}
	} else if ( is.null(e1) && !is.null(e2) ) {
		e <- e2
	} else if ( !is.null(e1) && is.null(e2) ) {
		e <- e1
	} else {
		e <- NULL
	}
	e
}

merge_channels <- function(c1, c2, ...)
{
	if ( ...length() > 0L )
		c2 <- do.call(merge_channels, list(c2, ...))
	chs <- list()
	nms <- union(names(c1), names(c2))
	for ( nm in nms ) {
		if ( nm %in% names(c1) && !nm %in% names(c2) ) {
			chs[[nm]] <- c1[[nm]]
		} else if ( !nm %in% names(c1) && nm %in% names(c2) ) {
			chs[[nm]] <- c2[[nm]]
		} else {
			ch <- c1[[nm]]
			for ( m in names(c2[[nm]]) ) {
				# merge c2 into c1 => c2 takes priority
				ch[[m]] <- c2[[nm]][[m]]
			}
			ch$limits <- merge_limits(c1[[nm]]$limits, c2[[nm]]$limits)
			chs[[nm]] <- ch
		}
	}
	chs
}

merge_limits <- function(l1, l2, ...)
{
	if ( ...length() > 0L )
		l2 <- do.call(merge_limits, list(l2, ...))
	if ( is_discrete(l1) && is_discrete(l2) ) {
		union(l1, l2)
	} else if ( !is_discrete(l1) && !is_discrete(l2) ) {
		range(l1, l2)
	} else {
		stop("can't merge continuous and discrete channels")
	}
}

compute_variables <- function(encoding, data = NULL)
{
	e <- lapply(encoding, get_var, data=data)
	ns <- lengths(e)
	if ( length(unique(ns)) != 1L )
		e <- lapply(e, rep_len, length.out=max(ns))
	e
}

compute_properties <- function(encoding, data = NULL)
{
	e <- compute_variables(encoding, data=data)
	channels <- lapply(names(e), function(nm)
	{
		xx <- encoding[[nm]]
		if ( is(xx, "formula") ) {
			lab <- deparse1(xx[[2L]])
		} else if ( is.character(xx) && !is.null(data) ) {
			lab <- xx[1L]
		} else {
			lab <- nm
		}
		lim <- get_limits(e[[nm]])
		sch <- get_scheme(nm, e[[nm]])
		list(label=lab, limits=lim, scheme=sch)
	})
	names(channels) <- names(e)
	list(encoding=e, channels=channels)
}

compute_subscripts <- function(encoding, nshingles = 6L)
{
	lapply(encoding, function(e) {
		if ( is_discrete(e) ) {
			e <- as.factor(e)
			nms <- levels(e)
			e <- lapply(levels(e), function(l) which(e == l))
			setNames(e, nms)
		} else {
			shingles(e, breaks=nshingles)
		}
	})
}

encode <- function(name, encoding, channels, params = NULL, subset = NULL)
{
	e <- encoding[[name]]
	if ( is.null(e) ) {
		# search non-data graphical parameters
		e <- params[[name]]
		if ( is.null(e) )
			e <- vizi_par(name)
		if ( is.null(e) )
			e <- par(name)
		if ( is.null(e) )
			stop("couldn't find encoding for ", sQuote(name))
	} else {
		# apply limits
		lim <- channels[[name]]$limits
		if ( is_discrete(e) ) {
			e <- factor(as.factor(e), levels=lim)
		} else {
			e <- ifelse(lim[1L] <= e | e <= lim[2L], e, NA)
		}
		# apply scheme
		sch <- channels[[name]]$scheme
		if ( !is.null(sch) ) {
			if ( !is_discrete(e) )
				e <- cut.default(e, length(sch))
			e <- as.integer(as.factor(e))
			e <- sch[e]
		}
		# apply subset
		if ( !is.null(subset) )
			e <- e[subset]
	}
	e
}

get_var <- function(x, data)
{
	if ( is(x, "formula") ) {
		if ( length(x) != 2L )
			stop("formula can only have rhs")
		eval(x[[2L]], envir=data, enclos=environment(x))
	} else if ( is(x, "character") && !is.null(data) ) {
		if ( length(x) != 1L )
			stop("string must be length-1")
		get(x, envir=as.environment(data))
	} else {
		force(x)
	}
}

get_limits <- function(x)
{
	if ( is_discrete(x) ) {
		levels(as.factor(x))
	} else {
		range(x, na.rm=TRUE)
	}
}

get_xlim <- function(plot)
{
	if ( !is.null(plot$coord$xlim) ) {
		plot$coord$xlim
	} else {
		plot$channels$x$limits
	}
}

get_ylim <- function(plot)
{
	if ( !is.null(plot$coord$ylim) ) {
		plot$coord$ylim
	} else {
		plot$channels$y$limits
	}
}

get_scheme <- function(channel, x)
{
	n <- min(length(unique(x)), 256L)
	if ( is_discrete(x) ) {
		switch(channel,
			x = NULL,
			y = NULL,
			pch = seq_len(min(n, 14L)),
			col = palette.colors(n),
			bg = palette.colors(n),
			cex = stop("can't make discrete scheme for 'cex'"),
			lwd = stop("can't make discrete scheme for 'lwd'"),
			lty = seq_len(min(n, 6L)),
			stop("can't make scheme for channel: ", sQuote(channel)))
	} else {
		switch(channel,
			x = NULL,
			y = NULL,
			pch = stop("can't make continuous scheme for 'pch'"),
			col = hcl.colors(n),
			bg = hcl.colors(n),
			cex = seq(from=0.5, to=5.5, length.out=n),
			lwd = seq(from=0.5, to=5.5, length.out=n),
			lty = stop("can't make continuous scheme for 'lty'"),
			stop("can't make scheme for channel: ", sQuote(channel)))
	}
}

squash_continuous <- function(x, limits)
{
	if ( valid(limits) ) {
		l <- limits[1L]
		u <- limits[2L]
		x[x < l | u < x] <- NA
	}
	x
}

squash_discrete <- function(x, limits)
{
	if ( valid(limits) ) {
		if ( is.factor(x) ) {
			x <- factor(x, levels=limits)
		} else {
			x[!x %in% limits] <- NA
		}
	}
	x
}
