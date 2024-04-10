

#### Plot a list of signals ####
## -----------------------------

plot_signal <- function(x, y, by = names(y), group = NULL,
	xlim = NULL, ylim = NULL, col = NULL, byrow = FALSE,
	xlab = NULL, ylab = NULL, layout = NULL, free = "",
	n = Inf, downsampler = "lttb", key = TRUE,
	grid = TRUE, isPeaks = FALSE, annPeaks = 0, ...)
{
	if ( is.array(x) ) {
		if ( length(dim(x)) > 2L )
			stop("'x' must have at most 2 dimensions")
		if ( byrow ) {
			y <- apply(x, 1L, identity, simplify=FALSE)
		} else {
			y <- apply(x, 2L, identity, simplify=FALSE)
		}
		x <- seq_along(y[[1L]])
	} else if ( missing(y) ) {
		y <- x
		if ( is.list(y) ) {
			x <- lapply(y, seq_along)
		} else {
			x <- seq_along(y)
		}
	} else if ( is.array(y) ) {
		if ( length(dim(y)) > 2L )
			stop("'y' must have at most 2 dimensions")
		if ( byrow ) {
			y <- apply(y, 1L, identity, simplify=FALSE)
		} else {
			y <- apply(y, 2L, identity, simplify=FALSE)
		}
	}
	if ( !is.list(y) )
		y <- list(y)
	if ( !is.list(x) )
		x <- rep_len(list(x), length(y))
	if ( !is.null(by) )
		by <- rep_len(factor(by, levels=unique(by)), length(y))
	if ( !is.null(group) )
		group <- rep_len(factor(group, levels=unique(group)), length(y))
	if ( length(annPeaks) != 1L || (!is.numeric(annPeaks) && !is.character(annPeaks)) )
		stop("'annPeaks' must be a scalar string or integer")
	isPeaks <- rep_len(isPeaks, length(y))
	mark <- ifelse(isPeaks, "peaks", "lines")
	if ( any(isPeaks) ) {
		for ( i in which(isPeaks) ) {
			nz <- which(y[[i]] != 0)
			if ( length(nz) > 0L ) {
				x[[i]] <- x[[i]][nz]
				y[[i]] <- y[[i]][nz]
			}
		}
	}
	if ( is.null(by) ) {
		plot <- vizi()
		for ( i in seq_along(y) ) {
			plot <- add_mark(plot, mark[[i]],
				x=x[[i]], y=y[[i]], color=group[[i]],
				trans=list(n=n, downsampler=downsampler))
			if ( isPeaks[[i]] ) {
				if ( is.character(annPeaks) ) {
					pch <- shape_pal()
					pch <- pch[[match.arg(annPeaks, names(pch))]]
					plot <- add_mark(plot, "points",
						x=x[[i]], y=y[[i]], color=group[[i]],
						trans=list(n=n, downsampler=downsampler),
						params=list(shape=pch))
				} else if ( annPeaks > 0 ) {
					j <- tail(order(abs(y[[i]])), n=annPeaks)
					labs <- as.character(round(x[[i]][j], digits=4L))
					plot <- add_mark(plot, "text",
						x=x[[i]][j], y=y[[i]][j], text=labs,
						params=list(pos=3L, offset=0.1))
				}
			}
		}
	} else {
		plot <- lapply(levels(by), function(lvl)
		{
			p <- vizi()
			for ( i in which(by == lvl) ) {
				p <- add_mark(p, mark[[i]],
					x=x[[i]], y=y[[i]], color=group[[i]],
					trans=list(n=n, downsampler=downsampler))
				if ( isPeaks[[i]] ) {
					if ( is.character(annPeaks) ) {
						pch <- shape_pal()
						pch <- pch[[match.arg(annPeaks, names(pch))]]
						p <- add_mark(p, "points",
							x=x[[i]], y=y[[i]], color=group[[i]],
							trans=list(n=n, downsampler=downsampler),
							params=list(shape=pch))
					} else if ( annPeaks > 0 ) {
						j <- tail(order(abs(y[[i]])), n=annPeaks)
						labs <- as.character(round(x[[i]][j], digits=4L))
						p <- add_mark(p, "text",
							x=x[[i]][j], y=y[[i]][j], text=labs,
							params=list(pos=3L, offset=0.1))
					}
				}
			}
			p
		})
		if ( is.null(layout) ) {
			plot <- as_facets(plot, labels=levels(by), free=free)
		} else {
			plot <- as_facets(plot, labels=levels(by),
				nrow=layout[1L], ncol=layout[2L], free=free)
		}
	}
	plot <- set_coord(plot, xlim=xlim, ylim=ylim, grid=grid)
	plot <- set_channel(plot, "x", label=xlab)
	plot <- set_channel(plot, "y", label=ylab)
	if ( !is.null(group) ) {
		plot <- set_channel(plot, "color", label="\n", scheme=col, key=key)
	} else {
		plot <- set_par(plot, col=col)
	}
	plot <- set_par(plot, ...)
	plot
}


#### Plot a list of images ####
## -----------------------------

plot_image <- function(x, y, vals, by = names(vals), group = NULL,
	zlim = NULL, xlim = NULL, ylim = NULL, col = NULL, byrow = FALSE,
	zlab = NULL, xlab = NULL, ylab = NULL, layout = NULL, free = "",
	enhance = NULL, smooth = NULL, scale = NULL, key = TRUE,
	grid = TRUE, asp = 1, useRaster = TRUE, ...)
{
	if ( is.array(x) || (missing(vals) && is.list(x) && is.matrix(x[[1L]])) )
	{
		if ( is.array(x) && length(dim(x)) > 3L )
			stop("'x' must have at most 2 dimensions")
		if ( is.array(x) && length(dim(x)) > 2L ) {
			vals <- apply(x, 3L, identity, simplify=FALSE)
		} else if ( !is.list(x) ) {
			vals <- list(x)
		} else {
			vals <- x
		}
		pos <- lapply(vals, function(v) expand.grid(x=1:dim(v)[1L], y=1:dim(v)[2L]))
		x <- lapply(pos, function(p) p$x)
		y <- lapply(pos, function(p) p$y)
	} else if ( is.matrix(vals) ) {
		if ( byrow ) {
			vals <- apply(vals, 1L, identity, simplify=FALSE)
		} else {
			vals <- apply(vals, 2L, identity, simplify=FALSE)
		}
	}
	if ( !is.list(vals) )
		vals <- list(vals)
	if ( !is.list(x) )
		x <- rep_len(list(x), length(vals))
	if ( !is.list(y) )
		y <- rep_len(list(y), length(vals))
	if ( !is.null(by) )
		by <- rep_len(factor(by, levels=unique(by)), length(vals))
	if ( !is.null(group) )
		group <- rep_len(factor(group, levels=unique(group)), length(vals))
	vals <- lapply(vals, function(v) if (is.factor(v)) v else as.vector(v))
	if ( is.null(by) ) {
		plot <- vizi()
		for ( i in seq_along(vals) ) {
			if ( is.null(group) ) {
				plot <- add_mark(plot, "pixels",
					x=x[[i]], y=y[[i]], color=vals[[i]],
					trans=list(enhance=enhance, smooth=smooth, scale=scale),
					params=list(useRaster=useRaster))
			} else {
				plot <- add_mark(plot, "pixels",
					x=x[[i]], y=y[[i]], alpha=vals[[i]], color=group[[i]],
					trans=list(enhance=enhance, smooth=smooth, scale=scale),
					params=list(useRaster=useRaster))
			}
		}
	} else {
		plot <- lapply(levels(by), function(lvl)
		{
			p <- vizi()
			for ( i in which(by == lvl) ) {
				if ( is.null(group) ) {
					p <- add_mark(p, "pixels",
						x=x[[i]], y=y[[i]], color=vals[[i]],
						trans=list(enhance=enhance, smooth=smooth, scale=scale),
						params=list(useRaster=useRaster))
				} else {
					p <- add_mark(p, "pixels",
						x=x[[i]], y=y[[i]], alpha=vals[[i]], color=group[[i]],
						trans=list(enhance=enhance, smooth=smooth, scale=scale),
						params=list(useRaster=useRaster))
				}
			}
			p
		})
		if ( is.null(layout) ) {
			plot <- as_facets(plot, labels=levels(by), free=free)
		} else {
			plot <- as_facets(plot, labels=levels(by),
				nrow=layout[1L], ncol=layout[2L], free=free)
		}
	}
	plot <- set_coord(plot, xlim=xlim, ylim=ylim, grid=grid, asp=asp, rev="y")
	plot <- set_channel(plot, "x", label=xlab)
	plot <- set_channel(plot, "y", label=ylab)
	if ( is.null(group) ) {
		clab <- if(isTRUE(scale)) "%" else "\n"
		plot <- set_channel(plot, "color", label=clab, limits=zlim, scheme=col, key=key)
	} else {
		plot <- set_channel(plot, "color", label="\n", scheme=col, key=key)
		plot <- set_channel(plot, "alpha", limits=zlim, key=FALSE)
	}
	plot <- set_par(plot, ...)
	plot
}


#### Color palettes and transparency ####
## ----------------------------------------

# discrete palette
dpal <- function(palette = "Tableau 10") {
	function(n) palette.colors(n, palette)
}

# continuous palette
cpal <- function(palette = "Viridis") {
	function(n) hcl.colors(n, palette)
}

# continuous palette
add_alpha <- function(colors, alpha = 1, exp = 2) {
	dm <- dim(colors)
	if ( is.null(dm) && !is.null(dim(alpha)) )
		dm <- dim(alpha)
	alpha <- ifelse(alpha < 0 | alpha > 1, 0, alpha)
	n <- max(length(colors), length(alpha))
	if ( length(alpha) != n )
		alpha <- rep_len(alpha, n)
	if ( length(colors) != n )
		colors <- rep_len(colors, n)
	na <- is.na(colors) | is.na(alpha)
	alpha <- ifelse(is.na(alpha), 0, alpha)
	colors <- col2rgb(colors, alpha=TRUE)
	colors <- rgb(colors[1L,], colors[2L,], colors[3L,],
		alpha=255 * alpha^exp, maxColorValue=255)
	colors[na] <- NA_character_
	dim(colors) <- dm
	colors
}

#### Plotting methods for 'vizi' marks ####
## ----------------------------------------

plot_mark_xy <- function(mark, plot = NULL, ...,
	n = Inf, downsampler = "lttb", jitter = "",
	type = "p", add = FALSE)
{
	# encode position channels
	encoding <- merge_encoding(plot$encoding, mark$encoding)
	x <- encode_var("x", encoding, plot$channels)
	y <- encode_var("y", encoding, plot$channels)
	if ( !is2d(plot) )
		z <- encode_var("z", encoding, plot$channels)
	if ( length(x) == 0L || length(y) == 0L )
		return()
	# decode positions if discrete
	if ( is_discrete(x) )
		x <- match(x, plot$channels$x$limits)
	if ( is_discrete(y) )
		y <- match(y, plot$channels$y$limits)
	# perform transformations
	if ( is2d(plot) ) {
		# downsample
		t <- mark$trans
		if ( !is.null(t$n) )
			n <- t$n
		if ( !is.null(t$downsampler) )
			downsampler <- t$downsampler
		if ( !is.null(t$jitter) )
			jitter <- t$jitter
		if ( n < length(y) ) {
			y <- downsample(y, n=n, domain=x, method=downsampler)
			i <- attr(y, "sample")
			x <- x[i]
		} else {
			i <- NULL
		}
	} else {
		# project 3d points
		pmat <- trans3d_get()
		t <- trans3d(x, y, z, pmat)
		i <- trans3d_sort(x, y, z, pmat)
		x <- t$x[i]
		y <- t$y[i]
	}
	if ( jitter %in% c("x", "xy", "yx") )
		x <- jitter(x)
	if ( jitter %in% c("y", "xy", "yx") )
		y <- jitter(y)
	# encode non-required channels
	params <- merge_encoding(plot$params, mark$params, as_encoding(...))
	params <- normalize_encoding(params)
	p <- c("shape", "color", "fill", "alpha", "size", "linewidth", "linetype")
	p <- setNames(p, p)
	p <- lapply(p, encode_var, encoding=encoding,
		channels=plot$channels, params=params, subscripts=i)
	p$color <- add_alpha(p$color, p$alpha)
	if ( !add )
		plot_init(plot)
	plot.xy(xy.coords(x, y), pch=p$shape, col=p$color, bg=p$fill,
		cex=p$size, lwd=p$linewidth, lty=p$linetype, type=type)
	# encode legends
	params <- p[!names(p) %in% names(encoding)]
	invisible(encode_legends(plot$channels, params, type))
}

plot.vizi_points <- function(x, plot = NULL, add = FALSE, ...,
	n = Inf, downsampler = "lttb", jitter = "")
{
	invisible(plot_mark_xy(mark=x, plot=plot, type="p", add=add, ...,
		n=n, downsampler=downsampler, jitter=jitter))
}

plot.vizi_lines <- function(x, plot = NULL, add = FALSE, ...,
	n = Inf, downsampler = "lttb", jitter = "")
{
	invisible(plot_mark_xy(mark=x, plot=plot, type="l", add=add, ...,
		n=n, downsampler=downsampler, jitter=jitter))
}

plot.vizi_peaks <- function(x, plot = NULL, add = FALSE, ...,
	n = Inf, downsampler = "lttb", jitter = "")
{
	invisible(plot_mark_xy(mark=x, plot=plot, type="h", add=add, ...,
		n=n, downsampler=downsampler, jitter=jitter))
}

plot_mark_text <- function(mark, plot = NULL, ...,
	adj = NULL, pos = NULL, offset = 0.5, add = FALSE)
{
	# encode position + label channels
	encoding <- merge_encoding(plot$encoding, mark$encoding)
	x <- encode_var("x", encoding, plot$channels)
	y <- encode_var("y", encoding, plot$channels)
	text <- encode_var("text", encoding, plot$channels)
	if ( !is2d(plot) )
		z <- encode_var("z", encoding, plot$channels)
	if ( length(x) == 0L || length(y) == 0L )
		return()
	# decode positions if discrete
	if ( is_discrete(x) )
		x <- match(x, plot$channels$x$limits)
	if ( is_discrete(y) )
		y <- match(y, plot$channels$y$limits)
	# get parameters
	if ( !is.null(mark$params$adj) )
		adj <- mark$params$adj
	if ( !is.null(mark$params$pos) )
		pos <- mark$params$pos
	if ( !is.null(mark$params$offset) )
		offset <- mark$params$offset
	# perform transformations
	if ( !is2d(plot) ) {
		# project 3d points
		pmat <- trans3d_get()
		t <- trans3d(x, y, z, pmat)
		i <- trans3d_sort(x, y, z, pmat)
		x <- t$x[i]
		y <- t$y[i]
	} else {
		i <- NULL
	}
	# encode non-required channels
	params <- merge_encoding(plot$params, mark$params, as_encoding(...))
	params <- normalize_encoding(params)
	p <- c("color", "alpha", "size")
	p <- setNames(p, p)
	p <- lapply(p, encode_var, encoding=encoding,
		channels=plot$channels, params=params, subscripts=i)
	p$color <- add_alpha(p$color, p$alpha)
	if ( !add )
		plot_init(plot)
	text.default(x, y, labels=text, adj=adj, pos=pos,
		offset=offset, cex=p$size, col=p$color)
	# encode legends
	invisible(encode_legends(plot$channels, list()))
}

plot.vizi_text <- function(x, plot = NULL, add = FALSE, ...,
	adj = NULL, pos = NULL, offset = 0.5)
{
	invisible(plot_mark_text(mark=x, plot=plot, add=add, ...,
		adj=adj, pos=pos, offset=offset))
}

plot_mark_intervals <- function(mark, plot = NULL, ...,
	length = 0.25, angle = 90, add = FALSE)
{
	# encode position channels
	encoding <- merge_encoding(plot$encoding, mark$encoding)
	x <- encode_var("x", encoding, plot$channels)
	xmin <- encode_var("xmin", encoding, plot$channels)
	xmax <- encode_var("xmax", encoding, plot$channels)
	y <- encode_var("y", encoding, plot$channels)
	ymin <- encode_var("ymin", encoding, plot$channels)
	ymax <- encode_var("ymax", encoding, plot$channels)
	if ( length(x) == 0L && length(y) == 0L )
		return()
	if ( is.null(c(xmin, xmax, ymin, ymax)) )
		return()
	# decode positions if discrete
	if ( is_discrete(x) )
		x <- match(x, plot$channels$x$limits)
	if ( is_discrete(y) )
		y <- match(y, plot$channels$y$limits)
	# get parameters
	if ( !is.null(mark$params$length) )
		length <- mark$params$length
	if ( !is.null(mark$params$angle) )
		angle <- mark$params$angle
	# encode non-required channels
	params <- merge_encoding(plot$params, mark$params, as_encoding(...))
	params <- normalize_encoding(params)
	p <- c("color", "alpha", "linewidth", "linetype")
	p <- setNames(p, p)
	p <- lapply(p, encode_var, encoding=encoding,
		channels=plot$channels, params=params)
	p$color <- add_alpha(p$color, p$alpha)
	if ( !add )
		plot_init(plot)
	if ( !is.null(xmin) && !is.null(xmax) )
		arrows(xmin, y, xmax, y, length=length, angle=angle,
			code=3L, col=p$color, lty=p$linetype, lwd=p$linewidth)
	if ( !is.null(ymin) && !is.null(ymax) )
		arrows(x, ymin, x, ymax, length=length, angle=angle,
			code=3L, col=p$color, lty=p$linetype, lwd=p$linewidth)
	# encode legends
	params <- p[!names(p) %in% names(encoding)]
	invisible(encode_legends(plot$channels, params, "l"))
}

plot.vizi_intervals <- function(x, plot = NULL, add = FALSE, ...,
	length = 0.25, angle = 90)
{
	invisible(plot_mark_intervals(mark=x, plot=plot, add=add, ...,
		length=length, angle=angle))
}

plot_mark_boxplot <- function(mark, plot = NULL, ...,
	range = 1.5, notch = FALSE, pars = NULL, add = FALSE)
{
	# encode position channels
	encoding <- merge_encoding(plot$encoding, mark$encoding)
	x <- encode_var("x", encoding, plot$channels)
	y <- encode_var("y", encoding, plot$channels)
	if ( length(x) == 0L || length(y) == 0L )
		return()
	# get parameters
	if ( !is.null(mark$params$range) )
		range <- mark$params$range
	if ( !is.null(mark$params$notch) )
		notch <- mark$params$notch
	if ( !is.null(mark$params$points) )
		points <- mark$params$points
	if ( !is.null(mark$params$pars) )
		pars <- mark$params$pars
	# encode non-required channels
	params <- merge_encoding(plot$params, mark$params, as_encoding(...))
	params <- normalize_encoding(params)
	p <- c("color", "fill", "alpha")
	p <- setNames(p, p)
	p <- lapply(p, encode_var, encoding=encoding,
		channels=plot$channels, params=params)
	p$color <- add_alpha(p$color, p$alpha)
	p$fill <- add_alpha(p$fill, p$alpha)
	f <- function(pj, i) {
		if ( length(pj) == length(i) ) {
			tapply(pj, i, unique)
		} else {
			pj
		}
	}
	if ( !add )
		plot_init(plot)
	if ( is_discrete(x) && is_discrete(y) ) {
		stop("one of 'x' or 'y' must be numeric")
	} else if ( is_discrete(x) ) {
		vals <- tapply(y, x, identity, simplify=FALSE)
		p <- lapply(p, f, x)
		horiz <- FALSE
	} else if ( is_discrete(y) ) {
		vals <- tapply(x, y, identity, simplify=FALSE)
		p <- lapply(p, f, y)
		horiz <- TRUE
	} else {
		stop("one of 'x' or 'y' must be discrete")
	}
	boxplot.default(vals, range=range, notch=notch,
		border=p$color, col=p$fill, pars=pars,
		horizontal=horiz, axes=FALSE, add=TRUE)
	# encode legends
	invisible(encode_legends(plot$channels, list()))
}

plot.vizi_boxplot <- function(x, plot = NULL, add = FALSE, ...,
	range = 1.5, notch = FALSE, pars = NULL)
{
	invisible(plot_mark_boxplot(mark=x, plot=plot, add=add, ...,
		range=range, notch=notch, pars=pars))
}

compute_raster <- function(mark, plot = NULL, ...,
	enhance = FALSE, smooth = FALSE, scale = FALSE,
	slice = NULL, tol = 1e-6)
{
	# encode position channels
	encoding <- merge_encoding(plot$encoding, mark$encoding)
	x <- encode_var("x", encoding, plot$channels)
	y <- encode_var("y", encoding, plot$channels)
	if ( !is2d(plot) )
		z <- encode_var("z", encoding, plot$channels)
	# encode alpha (allow setting via ...)
	params <- merge_encoding(plot$params, mark$params, as_encoding(...))
	params <- normalize_encoding(params)
	alpha <- encode_var("alpha", encoding, plot$channels, params)
	if ( length(x) == 0L || length(y) == 0L )
		return()
	# get name of color channel
	if ( "color" %in% names(plot$channels) ) {
		cname <- "color"
	} else if ( "fill" %in% names(plot$channels) ) {
		cname <- "fill"
	} else {
		stop("couldn't find encoding for 'color' or 'fill'")
	}
	# get color variable (w/out encoding)
	color <- encoding[[cname]]
	if ( length(color) == 0L || all(is.na(color)) )
		return()
	if ( length(alpha) == 0L || all(is.na(alpha)) )
		return()
	n <- max(length(color), length(alpha))
	if ( length(color) != n )
		color <- rep_len(color, n)
	if ( length(alpha) != n )
		alpha <- rep_len(alpha, n)
	# encode color limits
	clim <- plot$channels[[cname]]$limits
	if ( is.null(clim) )
		clim <- get_limits(color)
	color <- encode_limits(color, clim)
	# encode discrete values
	if ( is.factor(color) )
		color <- as.character(color)
	if ( is.factor(alpha) )
		alpha <- as.character(alpha)
	# rasterize color/alpha
	if ( !is2d(plot) && !is.null(slice) ) {
		# 3d to 2d slice
		ortho <- names(slice)
		if ( length(slice) != 1L || is.null(ortho) )
			stop("slice must be a named scalar")
		if ( !ortho %in% c("x", "y", "z") )
			stop("slice must be named x, y, or z")
		subset <- switch(ortho,
			x=(x >= slice - tol & x <= slice + tol),
			y=(y >= slice - tol & y <= slice + tol),
			z=(z >= slice - tol & z <= slice + tol))
		x <- x[subset]
		y <- y[subset]
		z <- z[subset]
		color <- color[subset]
		alpha <- alpha[subset]
		if ( ortho == "z" ) {
			i <- x
			j <- y
		} else if ( ortho == "y" ) {
			i <- x
			j <- z
		} else if ( ortho == "x" ) {
			i <- y
			j <- z
		}
	} else {
		# 2d image
		ortho <- "z"
		i <- x
		j <- y
	}
	rc <- to_raster(i, j, color)
	ra <- to_raster(i, j, alpha)
	dm <- dim(rc)
	# perform transformations
	t <- mark$trans
	if ( !is.null(t$enhance) )
		enhance <- t$enhance
	if ( !is.null(t$smooth) )
		smooth <- t$smooth
	if ( !is.null(t$scale) )
		scale <- t$scale
	const_color <- n_unique(color) == 1L
	const_alpha <- n_unique(alpha) == 1L
	if ( is.character(enhance) || isTRUE(enhance) ) {
		# contrast enhancement
		fn <- enhance_fun(enhance)
		if ( !const_color && is.numeric(rc) ) {
			rc <- fn(rc)
			clim <- range(rc, na.rm=TRUE)
		}
		if ( !const_alpha && is.numeric(ra) ) {
			ra <- fn(ra)
			ra <- rescale_range(ra, c(0, 1))
		}
	}
	if ( is.character(smooth) || isTRUE(smooth) ) {
		# smoothing
		fn <- filt2_fun(smooth)
		if ( !const_color && is.numeric(rc) ) {
			rc <- fn(rc)
			clim <- range(rc, na.rm=TRUE)
		}
		if ( !const_alpha && is.numeric(ra) ) {
			ra <- fn(ra)
			ra <- rescale_range(ra, c(0, 1))
		}
	}
	if ( (is.numeric(rc) || is.numeric(ra)) && isTRUE(scale) ) {
		# scaling
		if ( !const_color && is.numeric(rc) ) {
			clim <- c(0, 100)
			rc <- rescale_range(rc, clim)
		}
		if ( !const_alpha && is.numeric(ra) )
			ra <- rescale_range(ra, c(0, 1))
	}
	# encode color scheme
	csch <- plot$channels[[cname]]$scheme
	if ( is.null(csch) )
		csch <- get_scheme(cname, rc)
	rc <- encode_scheme(rc, csch, clim)
	rc <- add_alpha(rc, ra)
	dim(rc) <- dm
	# return raster
	if ( is2d(plot) ) {
		list(raster=rc, channel=cname,
			limits=clim, ortho=ortho,
			i=range(i, na.rm=TRUE),
			j=range(j, na.rm=TRUE),
			x=x, y=y)
	} else {
		list(raster=rc, channel=cname,
			limits=clim, ortho=ortho,
			i=range(i, na.rm=TRUE),
			j=range(j, na.rm=TRUE),
			x=x, y=y, z=z)
	}
}

plot_mark_pixels <- function(mark, plot = NULL, ...,
	enhance = FALSE, smooth = FALSE, scale = FALSE,
	useRaster = TRUE, add = FALSE)
{
	# compute the image
	rs <- compute_raster(mark, plot,
		enhance=enhance, smooth=smooth, scale=scale, ...)
	rc <- rs$raster
	if ( is.null(rc) )
		return()
	# flip x axis?
	if ( par("usr")[1L] > par("usr")[2L] )
		rc <- rc[nrow(rc):1L,,drop=FALSE]
	# flip y axis?
	if ( par("usr")[3L] < par("usr")[4L] )
		rc <- rc[,ncol(rc):1L,drop=FALSE]
	# plot the image
	if ( !add )
		plot_init(plot)
	hasRaster <- dev.capabilities("rasterImage")$rasterImage
	if ( !is2d(plot) || hasRaster != "yes" )
		useRaster <- FALSE
	if ( !is.null(mark$params$useRaster) )
		useRaster <- useRaster && isTRUE(mark$params$useRaster)
	if ( useRaster ) {
		# plot raster
		rc <- t(rc)
		di <- 0.5 * diff(rs$i) / (dim(rc)[1L] - 1)
		dj <- 0.5 * diff(rs$j) / (dim(rc)[2L] - 1)
		rasterImage(as.raster(rc),
			xleft=rs$i[1L] - di, ybottom=rs$j[1L] - dj,
			xright=rs$i[2L] + di, ytop=rs$j[2L] + dj,
			interpolate=FALSE)
	} else {
		# plot polygons
		p <- pix2poly(rs$i, rs$j, dim(rc))
		if ( !is2d(plot) ) {
			pmat <- trans3d_get()
			i <- as.vector(p$x)
			j <- as.vector(p$y)
			if ( rs$ortho == "z" ) {
				p <- trans3d(i, j, rs$z, pmat)
			} else if ( rs$ortho == "y" ) {
				p <- trans3d(i, rs$y, j, pmat)
			} else if ( rs$ortho == "x" ) {
				p <- trans3d(rs$x, i, j, pmat)
			}
		}
		polygon(p$x, p$y, col=rc, border=rc)
	}
	# encode legends
	plot$channels[[rs$channel]]$limits <- rs$limits
	invisible(encode_legends(plot$channels, list()))
}

plot_mark_voxels <- function(mark, plot = NULL, ...,
	enhance = FALSE, smooth = FALSE, scale = FALSE,
	xslice = NULL, yslice = NULL, zslice = NULL, add = FALSE)
{
	# encode position channels
	encoding <- merge_encoding(plot$encoding, mark$encoding)
	x <- encode_var("x", encoding, plot$channels)
	y <- encode_var("y", encoding, plot$channels)
	z <- encode_var("z", encoding, plot$channels)
	# compute slices
	xslice <- if (is.null(xslice)) integer() else xslice
	yslice <- if (is.null(yslice)) integer() else yslice
	zslice <- if (is.null(zslice)) integer() else zslice
	slices <- c(
		setNames(xslice, rep.int("x", length(xslice))),
		setNames(yslice, rep.int("y", length(yslice))),
		setNames(zslice, rep.int("z", length(zslice))))
	if ( length(slices) == 0L ) {
		slices <- sort(unique(z))
		names(slices) <- rep.int("z", length(slices))
	}
	n <- length(slices)
	slices <- lapply(seq_len(n), function(k) slices[k])
	# compute rasters
	px <- list()
	py <- list()
	colors <- list()
	depths <- list()
	for ( slice in slices ) {
		# compute raster slice
		rs <- compute_raster(mark, plot,
			enhance=enhance, smooth=smooth, scale=scale,
			slice=slice, ...)
		rc <- rs$raster
		# project raster to 3d polygons
		p <- pix2poly(rs$i, rs$j, dim(rc))
		pmat <- trans3d_get()
		i <- as.vector(p$x)
		j <- as.vector(p$y)
		if ( rs$ortho == "z" ) {
			p <- trans3d(i, j, rs$z, pmat)
			d <- trans3d_depth(i, j, rs$z, pmat)
		} else if ( rs$ortho == "y" ) {
			p <- trans3d(i, rs$y, j, pmat)
			d <- trans3d_depth(i, rs$y, j, pmat)
		} else if ( rs$ortho == "x" ) {
			p <- trans3d(rs$x, i, j, pmat)
			d <- trans3d_depth(rs$x, i, j, pmat)
		}
		# compute polygon depth
		dim(d) <- c(5L, length(rc))
		d <- colMeans(d, na.rm=TRUE)
		# re-structure
		dim(rc) <- NULL
		dim(p$x) <- c(5L, length(rc))
		dim(p$y) <- c(5L, length(rc))
		# assign polygons to list
		px <- c(px, list(p$x))
		py <- c(py, list(p$y))
		colors <- c(colors, list(rc))
		depths <- c(depths, list(d))
	}
	# sort polygons by depth
	px <- do.call(cbind, px)
	py <- do.call(cbind, py)
	colors <- do.call(c, colors)
	depths <- do.call(c, depths)
	i <- sort.list(depths)
	px <- px[,i,drop=FALSE]
	py <- py[,i,drop=FALSE]
	colors <- colors[i]
	# plot the polygons
	if ( !add )
		plot_init(plot)
	polygon(px, py, col=colors, border=colors)
	# encode legends
	plot$channels[[rs$channel]]$limits <- rs$limits
	invisible(encode_legends(plot$channels, list()))
}

pix2poly <- function(xlim, ylim, dim)
{
	dx <- 0.5 * (xlim[2L] - xlim[1L]) / (dim[1L] - 1)
	dy <- 0.5 * (ylim[2L] - ylim[1L]) / (dim[2L] - 1)
	px <- seq(xlim[1L], xlim[2L], length.out=dim[1L])
	py <- seq(ylim[1L], ylim[2L], length.out=dim[2L])
	p <- expand.grid(x=px, y=py)
	x <- rbind(
		bottomleft=p$x - dx, topleft=p$x - dx,
		topright=p$x + dx, bottomright=p$x + dx, NA_real_)
	y <- rbind(
		bottomleft=p$y - dy, topleft=p$y + dy,
		topright=p$y + dy, bottomright=p$y - dy, NA_real_)
	list(x=x, y=y)
}

plot.vizi_pixels <- function(x, plot = NULL, add = FALSE, ...,
	enhance = FALSE, smooth = FALSE, scale = FALSE,
	useRaster = TRUE)
{
	invisible(plot_mark_pixels(mark=x, plot=plot, add=add, ...,
		enhance=enhance, smooth=smooth, scale=scale,
		useRaster=useRaster))
}

plot.vizi_voxels <- function(x, plot = NULL, add = FALSE, ...,
	xslice = NULL, yslice = NULL, zslice = NULL)
{
	invisible(plot_mark_voxels(mark=x, plot=plot, add=add, ...,
		xslice=xslice, yslice=yslice, zslice=zslice))
}

setOldClass("vizi_points")
setOldClass("vizi_lines")
setOldClass("vizi_peaks")
setOldClass("vizi_text")
setOldClass("vizi_intervals")
setOldClass("vizi_boxplot")
setOldClass("vizi_pixels")
setOldClass("vizi_voxels")

setMethod("plot", "vizi_points", plot.vizi_points)
setMethod("plot", "vizi_lines", plot.vizi_lines)
setMethod("plot", "vizi_peaks", plot.vizi_peaks)
setMethod("plot", "vizi_text", plot.vizi_text)
setMethod("plot", "vizi_intervals", plot.vizi_intervals)
setMethod("plot", "vizi_boxplot", plot.vizi_boxplot)
setMethod("plot", "vizi_pixels", plot.vizi_pixels)
setMethod("plot", "vizi_voxels", plot.vizi_voxels)

#### Set graphical parameters ####
## -------------------------------

par_update <- function(params, ..., more = list())
{
	update <- c(list(...), more)
	for ( nm in names(update) )
		params[[nm]] <- update[[nm]]
	params
}

par_style <- function(style = c("light", "dark", "classic"), ...)
{
	switch(match.arg(style),
		light = par_style_light(...),
		dark = par_style_dark(...),
		classic = par_style_classic(...))
}

par_style_new <- function(params = list(), ...)
{
	p <- list(
		bty = "n",
		mar = c(0.5, 0.5, 1, 1),	# inner margins
		oma = c(3, 3, 1, 1),		# outer margins
		mgp = c(1.5, 0.5, 0))		# adjust axes
	par_update(p, ..., more=params)
}

par_style_classic <- function(params = list(), ..., new = FALSE)
{
	if ( new )
		params <- par_update(par_style_new(), more=params)
	p <- list(
		fg="black",
		bg="transparent",
		col="black",
		col.axis="black",
		col.lab="black",
		col.main="black",
		col.sub="black")
	par_update(p, ..., more=params)
}

par_style_light <- function(params = list(), ..., new = FALSE)
{
	if ( new )
		params <- par_update(par_style_new(), more=params)
	p <- list(
		fg="black",
		bg="white",
		col="black",
		col.axis="black",
		col.lab="black",
		col.main="black",
		col.sub="black")
	par_update(p, ..., more=params)
}

par_style_dark <- function(params = list(), ..., new = FALSE)
{
	if ( new )
		params <- par_update(par_style_new(), more=params)
	p <- list(
		fg="white",
		bg="black",
		col="white",
		col.axis="white",
		col.lab="white",
		col.main="white",
		col.sub="white")
	par_update(p, ..., more=params)
}

par_pad <- function(params, side, adj = 0, outer = FALSE)
{
	if ( outer ) {
		pname <- "oma"
	} else {
		pname <- "mar"
	}
	margins <- params[[pname]]
	names(margins) <- c("bottom", "left", "top", "right")
	margins[side] <- margins[side] + adj
	params[[pname]] <- margins
	params
}

#### Panel and layout navigation ####
## -----------------------------------

panel_grid <- function(dim = c(1, 1),
	byrow = TRUE, ..., params = vizi_par())
{
	if ( missing(dim) )
		return(getOption("matter.vizi.panelgrid"))
	if ( is.function(params) ) {
		p <- params(...)
	} else {
		p <- c(list(...), params)
	}
	if ( !is.numeric(dim) || length(dim) != 2L )
		stop("dim must be a length-2 numeric vector")
	if ( byrow ) {
		p$mfrow <- dim
	} else {
		p$mfcol <- dim
	}
	op <- par(p)
	mat <- matrix(seq_len(prod(dim)),
		nrow=dim[1L], ncol=dim[2L], byrow=byrow)
	pgrid <- list(mat=mat, byrow=byrow, par=op)
	options(matter.vizi.panelgrid=pgrid)
	invisible(pgrid)
}

panel_dim_n <- function(n)
{
	ncol <- ceiling(sqrt(n))
	nrow <- ceiling(n / ncol)
	c(nrow, ncol)
}

panel_row <- function() par("mfg")[1L]

panel_col <- function() par("mfg")[2L]

panel_dim <- function() par("mfg")[c(3L, 4L)]

panel_save <- function()
{
	pgrid <- getOption("matter.vizi.panelgrid")
	if ( dev.cur() == 1L )
		stop("no graphics device open")
	if ( is.null(pgrid) )
		pgrid <- list()
	params <- par(no.readonly=TRUE)
	pgrid$par <- params
	options(matter.vizi.panelgrid=pgrid)
	invisible(params)
}

panel_restore <- function(params = NULL, pgrid = NULL, new = FALSE)
{
	if ( dev.cur() == 1L )
		stop("no graphics device open")
	if ( is.null(pgrid) )
		pgrid <- getOption("matter.vizi.panelgrid")
	if ( is.null(params) )
		params <- pgrid$par
	if ( is.null(params) )
		stop("nothing to restore; has panel_save() been called?")
	p <- par(params)
	if ( !is.null(pgrid$mat) ) {
		if ( isTRUE(pgrid$byrow) ) {
			par(mfrow=dim(pgrid$mat))
		} else {
			par(mfrow=dim(pgrid$mat))
		}
	}
	mfg <- par("mfg")
	par(mfg=mfg, new=new)
	invisible(p)
}

panel_get <- function(pgrid = NULL, arr.ind = FALSE)
{
	if ( dev.cur() == 1L )
		stop("no graphics device open")
	if ( is.null(pgrid) )
		pgrid <- getOption("matter.vizi.panelgrid")
	if ( is.null(pgrid) )
		return(NA_integer_)
	mfg <- par("mfg")
	i <- pgrid$mat[mfg[1], mfg[2]]
	if ( arr.ind ) {
		structure(mfg[c(1,2)], index=i)
	} else {
		structure(i, arr.ind=mfg[c(1,2)])
	}
}

panel_set <- function(which = -1, pgrid = NULL, new = NULL)
{
	if ( dev.cur() == 1 )
		stop("no graphics device open")
	if ( is.null(pgrid) )
		pgrid <- getOption("matter.vizi.panelgrid")
	if ( is.null(pgrid) )
		stop("no panel grid found")
	mfg <- par("mfg")
	if ( length(which) == 1L ) {
		if ( which < 1 )
			which <- length(pgrid$mat) + which + 1
		nxt <- which(pgrid$mat == which, arr.ind=TRUE)
	} else if ( length(which) == 2L ) {
		nxt <- which
	} else {
		stop("which must be length 1 or 2")
	}
	mfg[c(1, 2)] <- nxt
	par(mfg=mfg)
	if ( !is.null(new) )
		par(new=new)
	invisible(pgrid)
}

panel_next <- function(pgrid = NULL)
{
	if ( is.null(pgrid) )
		pgrid <- getOption("matter.vizi.panelgrid")
	i <- panel_get(pgrid)
	imax <- length(pgrid$mat)
	panel_set((i %% imax) + 1, pgrid=pgrid)
}

panel_prev <- function(pgrid = NULL)
{
	if ( is.null(pgrid) )
		pgrid <- getOption("matter.vizi.panelgrid")
	i <- panel_get(pgrid)
	imax <- length(pgrid$mat)
	panel_set((i %% imax) - 1, pgrid=pgrid)
}

panel_side <- function(side = "right", split = 1, p = c(5/6, 5/6))
{
	if ( dev.cur() == 1L )
		stop("no graphics device open")
	side <- match.arg(side, c("right", "left", "bottom", "top"))
	saved <- panel_save()
	paruser <- saved[grep("^col|^cex", names(saved))]
	omd <- saved$omd
	if ( side %in% c("right", "left") ) {
		y <- c(omd[3], omd[4])
	} else if ( side %in% c("bottom", "top") ) {
		x <- c(omd[1], omd[2])
	}
	if ( side == "right" ) {
		x <- c(omd[2], 1)
	} else if ( side == "left" ) {
		x <- c(0, omd[2])
	} else if ( side == "bottom" ) {
		y <- c(0, omd[3])
	} else if ( side == "top" ) {
		y <- c(omd[4], 1)
	}
	if ( side %in% c("left", "right") ) {
		split <- list(mfrow=c(split, 1))
	} else if ( side %in% c("bottom", "top") ) {
		split <- list(mfcol=c(1, split))
	}
	omd <- c(x, y)
	p <- rep_len(p, 2L)
	dp <- (1 - p[2]) / 2
	plt <- c(0, p[1], dp, 1 - dp)
	parnew <- list(new=TRUE, pty="m", omd=omd, plt=plt)
	parnew <- c(paruser, parnew)
	par(parnew)
	par(split)
	mfg <- par("mfg")
	mfg[c(1, 2)] <- c(1, 1)
	par(mfg=mfg)
	invisible(saved)
}

is_top_panel <- function(pgrid = NULL)
{
	if ( is.null(pgrid) )
		pgrid <- getOption("matter.vizi.panelgrid")
	if ( is.null(pgrid) )
		return(TRUE)
	panel_get(pgrid, arr.ind=TRUE)[2L] == 1L
}

is_left_panel <- function(pgrid = NULL)
{
	if ( is.null(pgrid) )
		pgrid <- getOption("matter.vizi.panelgrid")
	if ( is.null(pgrid) )
		return(TRUE)
	panel_get(pgrid, arr.ind=TRUE)[2L] == 1L
}

is_bottom_panel <- function(n, pgrid = NULL)
{
	if ( is.null(pgrid) )
		pgrid <- getOption("matter.vizi.panelgrid")
	if ( is.null(pgrid) )
		return(TRUE)
	ind <- panel_get(pgrid, arr.ind=TRUE)
	test <- ind[1L] == nrow(pgrid$mat)
	if ( test || missing(n) )
		return(test)
	nind <- rep.int(NA_integer_, length(pgrid$mat))
	nind[seq_len(n)] <- seq_len(n)
	mat <- matrix(nind, byrow=pgrid$byrow,
		nrow=nrow(pgrid$mat), ncol=ncol(pgrid$mat))
	is.na(mat[ind[1L] + 1L, ind[2L]])
}

is_right_panel <- function(n, pgrid = NULL)
{
	if ( is.null(pgrid) )
		pgrid <- getOption("matter.vizi.panelgrid")
	if ( is.null(pgrid) )
		return(TRUE)
	ind <- panel_get(pgrid, arr.ind=TRUE)
	test <- ind[2L] == ncol(pgrid$mat)
	if ( test || missing(n) )
		return(test)
	nind <- rep.int(NA_integer_, length(pgrid$mat))
	nind[seq_len(n)] <- seq_len(n)
	mat <- matrix(nind, byrow=pgrid$byrow,
		nrow=nrow(pgrid$mat), ncol=ncol(pgrid$mat))
	is.na(mat[ind[1L], ind[2L] + 1L])
}

is_last_panel <- function(pgrid = NULL)
{
	if ( is.null(pgrid) )
		pgrid <- getOption("matter.vizi.panelgrid")
	if ( is.null(pgrid) )
		return(TRUE)
	panel_get(pgrid) == length(pgrid$mat)
}

#### 3D to 2D transformations ####
## -------------------------------

trans3d_get <- function(i, pgrid = NULL)
{
	plist <- getOption("matter.vizi.trans3d")
	if ( missing(i) )
		i <- panel_get(pgrid)
	if ( !is.null(plist) ) {
		plist[[i]]
	} else {
		NULL
	}
}

trans3d_set <- function(pmat, i, pgrid = NULL)
{
	plist <- getOption("matter.vizi.trans3d")
	if ( missing(i) )
		i <- panel_get(pgrid)
	if ( is.na(i) )
		i <- 1L
	if ( is.null(plist) )
		plist <- list()
	plist[[i]] <- pmat
	options(matter.vizi.trans3d=plist)
	pmat
}

trans3d_depth <- function(x, y, z, pmat)
{
	tr <- cbind(x, y, z, 1, deparse.level = 0L) %*% pmat
	-tr[,4L,drop=TRUE]
}

trans3d_sort <- function(x, y, z, pmat)
{
	depth <- trans3d_depth(x, y, z, pmat)
	sort.list(depth)
}
