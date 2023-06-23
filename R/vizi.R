
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

# register for S4 methods

setOldClass("vizi_plot")
setOldClass("vizi_points")
setOldClass("vizi_lines")
setOldClass("vizi_peaks")
setOldClass("vizi_key")
setOldClass("vizi_colorkey")

#### Plotting methods for 'vizi' ####
## ----------------------------------

preplot.vizi_plot <- function(object, ...)
{
	p <- vizi_par()
	w <- needs_legends(object)
	if ( w > 0L ) {
		padj <- 5/6 # adjust chr width ~=> # of lines
		p <- par_pad(p, "right", w^padj, outer=TRUE)
	}
	par(p)
	xlim <- object$coord$xlim
	ylim <- object$coord$ylim
	if ( is.null(xlim) )
		xlim <- object$channels$x$limits
	if ( is.null(ylim) )
		ylim <- object$channels$y$limits
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
	keys <- lapply(x$marks, plot, plot=x, add=TRUE, ...)
	keys <- merge_legends(keys)
	if ( !add ) {
		localAxis(x$encoding$x, side=1L, ...)
		localAxis(x$encoding$y, side=2L, ...)
		localTitle(xlab=x$channels$x$label,
			ylab=x$channels$y$label, outer=TRUE, ...)
	}
	localBox(...)
	if ( !add && length(keys) > 0L ) {
		p <- panel_side("right", split=length(keys), p=c(1, 1))
		for (k in keys)
			plot(k, ...)
		panel_restore(p)
	}
	invisible(keys)
}

setMethod("plot", "vizi_plot", plot.vizi_plot)

plot_xy <- function(mark, plot = NULL, ..., type = "p", add = FALSE)
{
	encoding <- merge_encoding(plot$encoding, mark$encoding)
	x <- encode_var("x", encoding, plot$channels)
	y <- encode_var("y", encoding, plot$channels)
	stopifnot(!is.null(x), !is.null(y))
	t <- mark$trans
	if ( !is.null(t$nmax) && t$nmax < length(x) ) {
		if ( !is.null(t$sampler) ) {
			y <- downsample(y, n=t$nmax, domain=x, method=t$sampler)
		} else {
			y <- downsample(y, n=t$nmax, domain=x)
		}
		i <- attr(y, "sample")
		x <- x[i]
	} else {
		i <- NULL
	}
	p <- c("shape", "color", "fill", "size", "linewidth", "linetype")
	p <- lapply(setNames(p, p), encode_var, encoding=encoding,
		channels=plot$channels, params=mark$params, subset=i)
	if ( !add ) {
		plot.new()
		plot.window(xlim=range(x), ylim=range(y))
	}
	plot.xy(xy.coords(x, y), pch=p$shape, col=p$color, bg=p$fill,
		cex=p$size, lwd=p$linewidth, lty=p$linetype, type=type, ...)
	p <- p[!names(p) %in% names(encoding)]
	invisible(encode_legends(plot$channels, p, type))
}

plot.vizi_points <- function(x, plot = NULL, ..., add = FALSE)
{
	invisible(plot_xy(mark=x, plot=plot, type="p", ..., add=add))
}

plot.vizi_lines <- function(x, plot = NULL, ..., add = FALSE)
{
	invisible(plot_xy(mark=x, plot=plot, type="l", ..., add=add))
}

plot.vizi_peaks <- function(x, plot = NULL, ..., add = FALSE)
{
	invisible(plot_xy(mark=x, plot=plot, type="h", ..., add=add))
}

setMethod("plot", "vizi_points", plot.vizi_points)
setMethod("plot", "vizi_lines", plot.vizi_lines)
setMethod("plot", "vizi_peaks", plot.vizi_peaks)

plot.vizi_key <- function(x, ...)
{
	plot.new()
	args <- list(x="left", bty="n", title.adj=0)
	do.call(legend, c(args, x))
}

plot.vizi_colorkey <- function(x, ..., p = c(1/3, 2/3))
{
	plt <- par("plt")
	p <- rep_len(p, 2L)
	dp <- (1 - p[2]) / 2
	pltnew <- c(0, p[1], dp, 1 - dp)
	par(plt=pltnew)
	image(1, x$values, t(x$values), col=x$color,
		xaxt="n", yaxt="n", xlab="", ylab="")
	mtext(x$title, side=3L, outer=FALSE)
	Axis(x$values, side=4L, las=1L)
	par(plt=plt)
}

setMethod("plot", "vizi_key", plot.vizi_key)
setMethod("plot", "vizi_colorkey", plot.vizi_colorkey)

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
	if ( length(e) > 0L ) {
		setNames(e, to_vizi_name(names(e)))
	} else {
		NULL
	}
}

to_par_name <- function(ch)
{
	channels <- c(shape = "pch", size = "cex",
		color = "col", colour = "col", fill = "bg",
		linewidth = "lwd", linetype = "lty",
		lineend = "lend", linejoin = "ljoin",
		linemitre = "lmitre")
	ifelse(ch %in% names(channels), channels[ch], ch)
}

to_vizi_name <- function(ch)
{
	channels <- c(pch = "shape", cex = "size",
		col = "color", colour = "color", fg = "color", bg = "fill",
		lwd = "linewidth", lty = "linetype",
		lend = "lineend", ljoin = "linejoin",
		lmitre = "linemitre")
	ifelse(ch %in% names(channels), channels[ch], ch)
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
	normalize_lengths(e)
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
	normalize_lengths(e)
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
		list(label=lab, limits=lim)
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

encode_var <- function(name, encoding = NULL,
	channels = NULL, params = NULL, subset = NULL)
{
	e <- encoding[[name]]
	if ( is.null(e) ) {
		# search non-data graphical parameters
		e <- params[[name]]
		if ( is.null(e) )
			e <- vizi_par(to_par_name(name))
		if ( is.null(e) )
			e <- par(to_par_name(name))
		if ( is.null(e) )
			stop("couldn't find encoding for ", sQuote(name))
	} else {
		# encode limits
		lim <- channels[[name]]$limits
		if ( is.null(lim) )
			lim <- get_limits(e)
		e <- encode_limits(e, lim)
		# encode scheme
		sch <- channels[[name]]$scheme
		if ( is.null(sch) )
			sch <- get_scheme(name, e)
		e <- encode_scheme(e, sch, lim)
		# subset
		if ( !is.null(subset) )
			e <- e[subset]
	}
	e
}

get_limits <- function(x)
{
	if ( is_discrete(x) ) {
		levels(as.factor(x))
	} else {
		range(x, na.rm=TRUE)
	}
}

encode_limits <- function(x, limits)
{
	if ( is.null(limits) )
		return(x)
	if ( is_discrete(x) ) {
		factor(as.factor(x), levels=limits)
	} else {
		ifelse(limits[1L] <= x & x <= limits[2L], x, NA)
	}
}

get_scheme <- function(channel, x)
{
	if ( is_discrete(x) ) {
		msg <- paste0("can't make discrete scheme for ", sQuote(channel))
		switch(channel,
			x =, y =, z = NULL,
			shape = function(n) seq_len(min(n, 14L)),
			color =, fill = palette.colors,
			size =, linewidth = stop(msg),
			linetype = function(n) seq_len(min(n, 6L)),
			stop(msg))
	} else {
		switch(channel,
			x =, y =, z = NULL,
			shape = stop(msg),
			color =, fill = hcl.colors,
			size =, linewidth = function(n) seq.int(1, 6, length.out=n),
			linetype = stop(msg),
			stop(msg))
	}
}

encode_scheme <- function(x, scheme, limits)
{
	if ( is.null(scheme) )
		return(x)
	if ( is.function(scheme) ) {
		f <- scheme
		if ( is_discrete(x) ) {
			n <- length(limits)
		} else {
			n <- length(unique(x))
		}
		n <- max(1L, min(n, 256L))
		scheme <- f(n)
	}
	n <- length(scheme)
	if ( is_discrete(x) ) {
		x <- as.factor(x)
		scheme <- rep_len(scheme, nlevels(x))
	} else {
		dx <- diff(limits)
        if ( dx == 0 ) {
			x <- rep.int(1L, length(x))
		} else {
			breaks <- seq.int(limits[1L], limits[2L], length.out=n + 1L)
			breaks[1L] <- limits[1L] - (dx / 1000)
			breaks[n + 1L] <- limits[2L] + (dx / 1000)
			x <- cut.default(x, breaks=breaks)
		}
	}
	setNames(scheme[as.integer(x)], x)
}

encode_legends <- function(channels, params, type = NULL)
{
	keys <- list()
	for ( nm in setdiff(names(channels), c("x", "y", "z")) )
	{
		key <- params[!names(params) %in% nm]
		names(key) <- to_par_name(names(key))
		lab <- channels[[nm]]$label
		key$title <- lab
		# get limits
		lim <- channels[[nm]]$limits
		# get scheme
		sch <- channels[[nm]]$scheme
		if ( is.null(sch) )
			sch <- get_scheme(nm, lim)
		# make legend
		pnm <- to_par_name(nm)
		if ( is_discrete(lim) ) {
			key$legend <- lim
			key[[pnm]] <- encode_scheme(lim, sch, lim)
			class(key) <- "vizi_key"
		} else {
			if ( nm %in% c("color", "fill") ) {
				key$values <- seq.int(lim[1L], lim[2L], length.out=256L)
				key$color <- encode_scheme(key$values, sch, lim)
				class(key) <- "vizi_colorkey"
			} else {
				x <- seq.int(lim[1L], lim[2L], length.out=3L)
				key[[pnm]] <- encode_scheme(x, sch, lim)
				key$legend <- format(x)
				class(key) <- "vizi_key"
			}
		}
		if ( !is.null(type) )
		{
			if ( type %in% c("p", "b", "o") ) {
				if ( "bg" %in% names(key) )
					key$pt.bg <- key$bg
				if ( "cex" %in% names(key) )
					key$pt.cex <- key$cex
				if ( "lwd" %in% names(key) )
					key$pt.lwd <- key$lwd
				if ( type == "p" )
					key[c("bg", "cex", "lwd", "lty")] <- NULL
				if ( !"pch" %in% names(key) )
					key$pch <- encode_var("pch")
			}
			if ( type %in% c("l", "h") ) {
				key$pch <- NULL
				if ( !"lty" %in% names(key) )
					key$lty <- encode_var("lty")
				if ( !"lwd" %in% names(key) )
					key$lwd <- encode_var("lwd")
			}
		}
		if ( lab %in% names(keys) ) {
			nnm <- setdiff(names(key), names(keys[[lab]]))
			keys[[lab]][nnm] <- key[nnm]
		} else {
			keys[[lab]] <- key
		}
	}
	keys
}

merge_legends <- function(keys)
{
	ks <- list()
	keys <- do.call(c, unname(keys))
	nms <- unique(names(keys))
	for ( nm in nms )
	{
		ks[[nm]] <- structure(list(), class=class(keys[[nm]]))
		for ( k in keys[names(keys) %in% nm] )
		{
			for ( p in names(k) )
			{
				if ( !p %in% names(ks[[nm]]) )
					ks[[nm]][[p]] <- k[[p]]
			}
		}
	}
	ks
}

needs_legends <- function(plot)
{
	chs <- names(plot$channels)
	chs <- setdiff(chs, c("x", "y", "z"))
	chs <- plot$channels[chs]
	if ( length(chs) > 0L ) {
		lens <- vapply(chs, function(ch) max(nchar(ch$limits)),
			numeric(1L))
		max(lens)
	} else {
		FALSE
	}
}
