
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
	if ( is(plot, "vizi_facets") )
		stop("can't add more marks to 'vizi_facets")
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

facet <- function(plot, by = NULL, data = NULL,
	nrow = NA, ncol = NA, labels = NULL, drop = TRUE)
{
	if ( !inherits(plot, c("vizi_plot", "vizi_facets")) ) {
		if ( is.list(plot) ) {
			plot <- as_facets(plot, drop)
		} else {
			stop("'plot' must inherit from 'vizi_plot' or 'vizi_facets")
		}
	}
	if ( is(by, "formula") ) {
		by <- parse_formula(by)
		by <- c(by$lhs, by$rhs)
	} else {
		by <- compute_variables(by, data)
	}
	if ( any(!is.na(c(nrow, ncol))) ) {
		nshingles <- max(c(nrow, ncol), na.rm=TRUE)
	} else {
		nshingles <- 6
	}
	facets <- compute_facets(plot, by, nshingles)
	if ( !is.null(labels) )
		facets$labels <- labels
	n <- prod(facets$dim)
	if ( !is.na(nrow) && !is.na(ncol) ) {
		facets$dim[1L] <- nrow
		facets$dim[2L] <- ncol
	} else if ( !is.na(nrow) ) {
		facets$dim[1L] <- nrow
		facets$dim[2L] <- ceiling(n / nrow)
	} else if ( !is.na(ncol) ) {
		facets$dim[1L] <- ceiling(n / ncol)
		facets$dim[2L] <- ncol
	}
	facets$drop <- drop
	structure(facets, class="vizi_facets")
}

# register for S4 methods

setOldClass("vizi_plot")
setOldClass("vizi_facets")
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
	vizi_panel(dim=c(1L,1L), params=p)
	xlim <- object$coord$xlim
	ylim <- object$coord$ylim
	log <- object$coord$log
	asp <- object$coord$asp
	if ( is.null(xlim) )
		xlim <- object$channels$x$limits
	if ( is.null(ylim) )
		ylim <- object$channels$y$limits
	if ( is.null(log) )
		log <- ""
	if ( is.null(asp) )
		asp <- NA
	plot.new()
	localWindow <- function(..., col, bg, pch, cex, lty, lwd) plot.window(...)
	args <- list(xlim=xlim, ylim=ylim, log=log, asp=asp)
	args <- c(args, object$params, list(...))
	do.call(localWindow, args)
	if ( isTRUE(object$coord$grid) )
		grid()
}

preplot.vizi_facets <- function(object, ...)
{
	p <- vizi_par()
	w <- needs_legends(object)
	if ( w > 0L ) {
		padj <- 5/6 # adjust chr width ~=> # of lines
		p <- par_pad(p, "right", w^padj, outer=TRUE)
	}
	n <- max(nlines(object$labels))
	p <- par_pad(p, "top", n - 1)
	vizi_panel(dim=object$dim, params=p)
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
	keys <- list()
	for ( i in seq_along(x$marks) )
	{
		mark <- x$marks[[i]]
		keys[[i]] <- plot(mark, plot=x, add=TRUE, ...)
	}
	keys <- merge_legends(keys)
	if ( !add ) {
		localAxis(x$channels$x$limits, side=1L, ...)
		localAxis(x$channels$y$limits, side=2L, ...)
		localTitle(xlab=x$channels$x$label,
			ylab=x$channels$y$label, outer=TRUE, ...)
	}
	localBox(...)
	if ( !add && length(keys) > 0L ) {
		p <- panel_side("right", split=length(keys), p=c(1, 1))
		for (k in keys)
			plot(k, cex=p$cex, ...)
		panel_restore(p)
	}
	x$keys <- keys
	invisible(x)
}

plot.vizi_facets <- function(x, ..., add = FALSE)
{
	localAxis <- function(..., col, bg, pch, cex, lty, lwd) Axis(...)
	localTitle <- function(..., col, bg, pch, cex, lty, lwd) title(...)
	if ( !add ) {
		dev.hold()
		on.exit(dev.flush())
		preplot(x)
	} else {
		panel_set(1)
	}
	keys <- list()
	n <- length(x$plots)
	for ( i in seq_len(n) )
	{
		plot <- x$plots[[i]]
		plot$channels <- x$channels
		if ( !add )
		{
			xlim <- x$coord$xlim
			ylim <- x$coord$ylim
			log <- x$coord$log
			asp <- x$coord$asp
			if ( is.null(xlim) )
				xlim <- x$channels$x$limits
			if ( is.null(ylim) )
				ylim <- x$channels$y$limits
			if ( is.null(log) )
				log <- ""
			if ( is.null(asp) )
				asp <- NA
			plot.new()
			localWindow <- function(..., col, bg, pch, cex, lty, lwd) plot.window(...)
			args <- list(xlim=xlim, ylim=ylim, log=log, asp=asp)
			args <- c(args, x$params, list(...))
			do.call(localWindow, args)
			if ( isTRUE(x$coord$grid) )
				grid()
		}
		keys[[i]] <- plot(plot, add=TRUE, ...)$keys
		if ( !add )
		{
			mtext(x$labels[i], cex=par("cex"), col=par("col.lab"))
			if ( is_left_panel() )
				localAxis(x$channels$y$limits, side=2L, ...)
			if ( is_bottom_panel(n) )
				localAxis(x$channels$x$limits, side=1L, ...)
		} else {
			panel_next()
		}
	}
	keys <- merge_legends(keys)
	if ( !add ) {
		localTitle(xlab=x$channels$x$label,
			ylab=x$channels$y$label, outer=TRUE, ...)
		if ( length(keys) > 0L ) {
			p <- panel_side("right", split=length(keys), p=c(1, 1))
			for (k in keys)
				plot(k, cex=p$cex, ...)
			panel_restore(p)
		}
		panel_set(1)
	}
	x$keys <- keys
	invisible(x)
}

setMethod("plot", "vizi_plot", plot.vizi_plot)
setMethod("plot", "vizi_facets", plot.vizi_facets)

plot_xy <- function(mark, plot = NULL, ..., type = "p", add = FALSE)
{
	encoding <- merge_encoding(plot$encoding, mark$encoding)
	x <- encode_var("x", encoding, plot$channels)
	y <- encode_var("y", encoding, plot$channels)
	if ( length(x) == 0L || length(y) == 0L )
		return()
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

plot.vizi_key <- function(x, cex = 1, ...)
{
	plot.new()
	args <- list(x="left", bty="n", title.adj=0, cex=cex, ...)
	do.call(legend, c(args, x))
}

plot.vizi_colorkey <- function(x, cex = 1, ..., p = c(1/3, 2/3))
{
	plt <- par("plt")
	p <- rep_len(p, 2L)
	dp <- (1 - p[2]) / 2
	pltnew <- c(0, p[1], dp, 1 - dp)
	par(plt=pltnew)
	image(1, x$values, t(x$values), col=x$color, cex=cex,
		xaxt="n", yaxt="n", xlab="", ylab="", ...)
	mtext(x$title, side=3L, cex=cex, ...)
	Axis(x$values, side=4L, las=1L, cex.axis=cex, ...)
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
	f <- function(x) {
		x <- get_var(x, data)
		if ( is_discrete(x) ) {
			as.factor(x)
		} else {
			x
		}
	}
	e <- lapply(encoding, f)
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

compute_subscripts <- function(by, nshingles = 6L)
{
	lapply(by, function(v) {
		if ( is_discrete(v) ) {
			v <- as.factor(v)
			nms <- levels(v)
			v <- lapply(levels(v), function(lvl) which(v == lvl))
			setNames(v, nms)
		} else {
			shingles(v, breaks=nshingles)
		}
	})
}

merge_subscripts <- function(subscripts, ...)
{
	if ( ...length() > 0L )
		subscripts <- list(subscripts, ...)
	ij <- expand.grid(lapply(subscripts, seq_along))
	fsub <- function(i) {
		i <- Map(function(F, j) F[[j]], subscripts, i)
		Reduce(intersect, i)
	}
	apply(ij, 1L, fsub, simplify=FALSE)
}

as_facets <- function(plotlist, drop = TRUE)
{
	plots <- lapply(plotlist, function(plot) {
		if ( !is(plot, "vizi_plot") )
			stop("'plot' must inherit from 'vizi_plot'")
		structure(list(encoding=plot$encoding,
			marks=plot$marks, params=plot$params), class="vizi_plot")
	})
	channels <- lapply(plotlist, function(plot) plot$channels)
	channels <- merge_channels(channels)
	subscripts <- lapply(plotlist,
		function(plot) max(lengths(plot$encoding)))
	n <- length(plots)
	structure(list(plots=plots, channels=channels,
		coord=plotlist[[1L]]$coord, subscripts=subscripts,
		labels=character(n), dim=panel_dim_n(n), drop=drop), class="vizi_facets")
}

compute_facets <- function(plot, by, nshingles = 6L)
{
	subscripts <- compute_subscripts(by, nshingles)
	if ( length(subscripts) == 1L ) {
		dim <- panel_dim_n(prod(lengths(subscripts)))
	} else {
		dim <- c(lengths(subscripts)[c(2, 1)])
	}
	levels <- expand.grid(lapply(subscripts, names))
	labels <- apply(levels, 1L, paste0, collapse="\n")
	ffac <- function(v, p) {
		fsub <- function(x) x[v]
		e <- lapply(p$encoding, fsub)
		mks <- lapply(p$marks, function(mk) {
			mk$encoding <- lapply(mk$encoding, fsub)
			mk
		})
		structure(list(encoding=e, marks=mks,
			params=p$params), class="vizi_plot")
	}
	subscripts <- merge_subscripts(subscripts)
	if ( is(plot, "vizi_facets") ) {
		plots <- lapply(plot$plots, function(p) lapply(subscripts, ffac, p=p))
		plots <- unlist(plots, recursive=FALSE)
		dim <- c(length(subscripts), length(plot$subscripts))
		subscripts <- merge_subscripts(plot$subscripts, subscripts)
		labels <- expand.grid(plot$labels, labels)
		labels <- apply(labels, 1L, paste0, collapse="\n")
	} else {
		plots <- lapply(subscripts, ffac, p=plot)
	}
	list(plots=plots, channels=plot$channels, coord=plot$coord,
		subscripts=subscripts, labels=labels, dim=dim)
}

get_var <- function(x, data)
{
	if ( is(x, "formula") ) {
		if ( length(x) != 2L )
			stop("formula encodings can only have rhs")
		eval(x[[2L]], envir=data, enclos=environment(x))
	} else if ( is(x, "character") && !is.null(data) ) {
		if ( length(x) != 1L )
			stop("string encodings must be length-1")
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
		get_discrete_scheme(channel)
	} else {
		get_continuous_scheme(channel)
	}
}

get_discrete_scheme <- function(channel)
{
	msg <- paste0("can't make discrete scheme for ", sQuote(channel))
	switch(channel,
		x =, y =, z = NULL,
		shape = seq_fun(14),
		color = discrete_colors,
		fill = discrete_colors,
		size = stop(msg),
		linewidth = stop(msg),
		linetype = seq_fun(6),
		stop(msg))
}

get_continuous_scheme <- function(channel)
{
	msg <- paste0("can't make continuous scheme for ", sQuote(channel))
	switch(channel,
		x =, y =, z = NULL,
		shape = stop(msg),
		color = continuous_colors,
		fill = continuous_colors,
		size = range_fun(1, 6),
		linewidth = range_fun(1, 6),
		linetype = stop(msg),
		stop(msg))
}

discrete_colors <- function(n)
{
	palette.colors(n, getOption("matter.vizi.palette"))
}

continuous_colors <- function(n)
{
	hcl.colors(n, getOption("matter.vizi.hcl"))
}

encode_scheme <- function(x, scheme, limits)
{
	if ( is.null(scheme) )
		return(x)
	if ( is.function(scheme) ) {
		fx <- scheme
		if ( is_discrete(x) ) {
			n <- length(limits)
		} else {
			n <- length(unique(x))
		}
		n <- max(1L, min(n, 256L))
		scheme <- fx(n)
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
			keys[[lab]] <- merge_legends(keys[[lab]], key)[[1L]]
		} else {
			keys[[lab]] <- key
		}
	}
	keys
}

merge_legends <- function(keys, ...)
{
	if ( ...length() > 0L ) {
		keys <- list(keys, ...)
		names(keys) <- rep.int(".", length(keys))
	} else {
		keys <- do.call(c, unname(keys))
	}
	ks <- list()
	nms <- unique(names(keys))
	for ( nm in nms )
	{
		ks[[nm]] <- structure(list(), class=class(keys[[nm]]))
		for ( k in keys[names(keys) %in% nm] )
		{
			for ( p in names(k) )
			{
				replace <- length(k[[p]]) > length(ks[[nm]][[p]])
				if ( !p %in% names(ks[[nm]]) || replace )
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
		lens1 <- vapply(chs, function(ch) max(0, nchar(ch$limits)),
			numeric(1L))
		lens2 <- vapply(chs, function(ch) max(0, nchar(ch$label)),
			numeric(1L))
		max(lens1, lens2)
	} else {
		FALSE
	}
}
