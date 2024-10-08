
#### Plot a list of signals ####
## -----------------------------

plot_signal <- function(x, y, z, by, group = NULL, byrow = FALSE,
	xlim = NULL, ylim = NULL, col = NULL, alphapow = 1,
	xlab = NULL, ylab = NULL, layout = NULL, free = "",
	n = Inf, downsampler = "lttb", key = TRUE, grid = TRUE,
	isPeaks = FALSE, annPeaks = 0, engine = NULL, ...)
{
	if ( is.array(x) ) {
		if ( length(dim(x)) > 2L )
			matter_error("'x' must have at most 2 dimensions")
		if ( byrow ) {
			y <- array2list(x, 1L)
		} else {
			y <- array2list(x, 2L)
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
			matter_error("'y' must have at most 2 dimensions")
		if ( byrow ) {
			y <- array2list(y, 1L)
		} else {
			y <- array2list(y, 2L)
		}
	}
	if ( missing(z) )
		z <- list()
	if ( !is.list(y) )
		y <- list(y)
	if ( !is.list(x) )
		x <- list(x)
	if ( !is.list(z) )
		z <- list(z)
	len <- max(length(x), length(y), length(z))
	x <- rep_len(x, len)
	y <- rep_len(y, len)
	z <- rep_len(z, len)
	is2d <- any(lengths(z) > 0L)
	if ( missing(by) ) {
		if ( is2d ) {
			if ( is.null(names(z)) && length(z) > 1L ) {
				by <- seq_along(z)
			} else {
				by <- names(z)
			}
		} else {
			if ( is.null(names(y)) && length(y) > 1L ) {
				by <- seq_along(y)
			} else {
				by <- names(y)
			}
		}
	}
	if ( !is.null(by) )
		by <- rep_len(factor(by, levels=unique(by)), length(y))
	if ( !is.null(group) )
		group <- rep_len(factor(group, levels=unique(group)), length(y))
	if ( length(annPeaks) != 1L || (!is.numeric(annPeaks) && !is.character(annPeaks)) )
		matter_error("'annPeaks' must be a scalar string or integer")
	if ( is2d ) {
		isPeaks <- rep.int(FALSE, len)
		mark <- rep.int("points", len)
		trans <- list()
		params <- list(shape=16L, size=0.25)
	} else {
		isPeaks <- rep_len(isPeaks, len)
		mark <- ifelse(isPeaks, "peaks", "lines")
		trans <- list(n=n, downsampler=downsampler)
		params <- list()
	}
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
				x=x[[i]], y=y[[i]],
				alpha=z[[i]], color=group[[i]],
				trans=trans, params=params)
			if ( isPeaks[[i]] )
			{
				if ( is.character(annPeaks) ) {
					pch <- shape_pal()
					pch <- pch[[match.arg(annPeaks, names(pch))]]
					plot <- add_mark(plot, "points",
						x=x[[i]], y=y[[i]], color=group[[i]],
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
					x=x[[i]], y=y[[i]],
					alpha=z[[i]], color=group[[i]],
					trans=trans, params=params)
				if ( isPeaks[[i]] )
				{
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
	asch <- range_fun(0, 1, alphapow)
	plot <- set_coord(plot, xlim=xlim, ylim=ylim, grid=grid)
	plot <- set_channel(plot, "x", label=xlab)
	plot <- set_channel(plot, "y", label=ylab)
	if ( is2d )
		plot <- set_channel(plot, "alpha", scheme=asch, key=FALSE)
	if ( !is.null(group) ) {
		plot <- set_channel(plot, "color", label="\n", scheme=col, key=key)
	} else {
		plot <- set_par(plot, col=col)
	}
	if ( !is.null(engine) )
		plot <- set_engine(plot, engine)
	plot <- set_par(plot, ...)
	plot
}


#### Plot a list of images ####
## -----------------------------

plot_image <- function(x, y, z, vals, by, group = NULL, byrow = FALSE,
	zlim = NULL, xlim = NULL, ylim = NULL, col = NULL, alphapow = 1,
	zlab = NULL, xlab = NULL, ylab = NULL, layout = NULL, free = "",
	enhance = NULL, smooth = NULL, scale = NULL, key = TRUE,
	rasterImages = NULL, rasterParams = NULL, useRaster = !is3d,
	grid = TRUE, asp = 1, engine = NULL, ...)
{
	if ( missing(vals) && !missing(z) )
	{
		vals <- z
		z <- list()
	}
	if ( is.array(x) || (missing(vals) && is.list(x) && is.array(x[[1L]])) )
	{
		if ( is.list(x) ) {
			vals <- x
		} else {
			vals <- list(x)
		}
		lens <- lengths(lapply(vals, dim))
		if ( any(lens < 2L) )
			matter_error("images must have at least 2 dimensions")
		if ( any(lens > 3L) )
			matter_error("images must have at most 3 dimensions")
		if ( n_unique(lens) > 1L )
			matter_error("can't mix 2D and 3D images")
		if ( lens[1L] > 2L ) {
			pos <- lapply(vals,
				function(v) expand.grid(
					x=1:dim(v)[1L], y=1:dim(v)[2L], z=1:dim(v)[3L]))
			x <- lapply(pos, function(p) p$x)
			y <- lapply(pos, function(p) p$y)
			z <- lapply(pos, function(p) p$z)
		} else {
			pos <- lapply(vals,
				function(v) expand.grid(x=1:dim(v)[1L], y=1:dim(v)[2L]))
			x <- lapply(pos, function(p) p$x)
			y <- lapply(pos, function(p) p$y)
			z <- list()
		}
	} else if ( is.matrix(vals) ) {
		if ( length(vals) != length(x) || length(vals) != length(y) )
		{
			if ( byrow ) {
				vals <- array2list(vals, 1L)
			} else {
				vals <- array2list(vals, 2L)
			}
		}
	}
	if ( missing(z) )
		z <- list()
	if ( !is.list(vals) )
		vals <- list(vals)
	if ( !is.list(x) )
		x <- list(x)
	if ( !is.list(y) )
		y <- list(y)
	if ( !is.list(z) )
		z <- list(z)
	x <- rep_len(x, length(vals))
	y <- rep_len(y, length(vals))
	z <- rep_len(z, length(vals))
	is3d <- any(lengths(z) > 0L)
	if ( missing(by) ) {
		if ( is.null(names(vals)) && length(vals) > 1L ) {
			by <- seq_along(vals)
		} else {
			by <- names(vals)
		}
	}
	if ( !is.null(by) )
		by <- rep_len(factor(by, levels=unique(by)), length(vals))
	if ( !is.null(group) )
		group <- rep_len(factor(group, levels=unique(group)), length(vals))
	if ( !is.null(rasterImages) && !is.list(rasterImages) )
		rasterImages <- list(rasterImages)
	vals <- lapply(vals, function(v) if (is.factor(v)) v else as.vector(v))
	if ( is3d ) {
		if ( useRaster ) {
			mark <- "voxels"
			alpha <- vals
			trans <- list(
				enhance=enhance,
				smooth=smooth,
				scale=scale)
			params <- list()
		} else {
			mark <- "points"
			alpha <- rep_len(list(), length(vals))
			trans <- list()
			params <- list(shape=16L)
		}
	} else {
		mark <- "pixels"
		if ( is.null(group) ) {
			alpha <- rep_len(list(), length(vals))
		} else {
			alpha <- vals
		}
		trans <- list(
			enhance=enhance,
			smooth=smooth,
			scale=scale)
		params <- list(useRaster=useRaster)
	}
	if ( is.null(group) ) {
		color <- vals
	} else {
		color <- group
	}
	if ( is.null(by) ) {
		plot <- vizi()
		if ( length(rasterImages) > 0L )
		{
			plot <- add_mark(plot, "image", image=rasterImages,
				xmin=rasterParams$xmin, xmax=rasterParams$xmax,
				ymin=rasterParams$ymin, ymax=rasterParams$ymax)
		}
		for ( i in seq_along(vals) ) {
			plot <- add_mark(plot, mark,
				x=x[[i]], y=y[[i]], z=z[[i]],
				alpha=alpha[[i]], color=color[[i]],
				trans=trans, params=params)
		}
	} else {
		plot <- lapply(levels(by), function(lvl)
		{
			p <- vizi()
			if ( length(rasterImages) > 0L )
			{
				if ( is.null(names(rasterImages)) ) {
					k <- seq_along(rasterImages)
				} else {
					k <- which(names(rasterImages) %in% lvl)
				}
				rs <- rasterImages[k]
				rp <- subset_list(rasterParams, k)
				p <- add_mark(p, "image", image=rs,
					xmin=rp$xmin, xmax=rp$xmax,
					ymin=rp$ymin, ymax=rp$ymax)
			}
			for ( i in which(by == lvl) ) {
				p <- add_mark(p, mark,
					x=x[[i]], y=y[[i]], z=z[[i]],
					alpha=alpha[[i]], color=color[[i]],
					trans=trans, params=params)
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
	asch <- range_fun(0, 1, alphapow)
	plot <- set_coord(plot, xlim=xlim, ylim=ylim, grid=grid, asp=asp, rev="y")
	plot <- set_channel(plot, "x", label=xlab)
	plot <- set_channel(plot, "y", label=ylab)
	if ( is3d ) {
		plot <- set_channel(plot, "z", label=zlab)
		plot <- set_channel(plot, "color", label="\n", scheme=col, key=key)
		plot <- set_channel(plot, "alpha", scheme=asch, key=FALSE)
		plot <- set_coord(plot, zlim=zlim)
		plot <- set_par(plot, scale=FALSE)
	} else {
		if ( is.null(group) ) {
			if ( is.null(zlab) )
				zlab <- if (isTRUE(scale)) "%" else "\n"
			plot <- set_channel(plot, "color", label=zlab, limits=zlim, scheme=col, key=key)
		} else {
			plot <- set_channel(plot, "color", label="\n", scheme=col, key=key)
			plot <- set_channel(plot, "alpha", limits=zlim, scheme=asch, key=FALSE)
		}
	}
	if ( !is.null(engine) )
		plot <- set_engine(plot, engine)
	plot <- set_par(plot, ...)
	plot
}


#### Color palettes and transparency ####
## ----------------------------------------

# discrete palette
dpal <- function(palette = "Tableau 10") {	
	test <- try(palette.colors(1, palette), silent=TRUE)
	if ( inherits(test, "try-error") )
	{
		matter_error("'palette' must be one of: ",
			paste0(sQuote(palette.pals()), collapse=", "))
	}
	function(n) palette.colors(n, palette)
}

dpals <- function() palette.pals()

# continuous palette
cpal <- function(palette = "Viridis") {
	test <- try(hcl.colors(1, palette), silent=TRUE)
	if ( inherits(test, "try-error") )
	{
		matter_error("'palette' must be one of: ",
			paste0(sQuote(hcl.pals()), collapse=", "))
	}
	function(n) hcl.colors(n, palette)
}

cpals <- function() hcl.pals()

# continuous palette
add_alpha <- function(colors, alpha = 1, pow = 1) {
	dm <- dim(colors)
	if ( is.null(dm) && !is.null(dim(alpha)) )
		dm <- dim(alpha)
	raster <- inherits(colors, "raster")
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
		alpha=255 * alpha^pow, maxColorValue=255)
	colors[na] <- NA_character_
	if ( raster ) {
		colors <- matrix(colors, byrow=TRUE, nrow=dm[1L], ncol=dm[2L])
	} else {
		dim(colors) <- dm
	}
	colors
}

#### Plotting methods for 'vizi' marks ####
## ----------------------------------------

plot_mark_xy <- function(mark, plot = NULL, ...,
	n = Inf, downsampler = "lttb", jitter = "",
	sort = is.finite(n), type = "p")
{
	# encode position channels
	encoding <- merge_encoding(plot$encoding, mark$encoding)
	x <- encode_var("x", encoding, plot$channels)
	y <- encode_var("y", encoding, plot$channels)
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
		if ( !is.null(t$sort) )
			sort <- t$sort
		if ( sort ) {
			i <- order(x)
			x <- x[i]
			y <- y[i]
		} else {
			i <- NULL
		}
		if ( n < length(y) ) {
			y <- downsample(y, n=n, domain=x, method=downsampler)
			x <- x[attr(y, "sample")]
			if ( sort ) {
				i <- i[attr(y, "sample")]
			} else {
				i <- attr(y, "sample")
			}
		}
	} else {
		if ( plot$engine$name == "base" ) {
			# project 3d points
			pmat <- trans3d_get()
			t <- trans3d(x, y, z, pmat)
			i <- trans3d_sort(x, y, z, pmat)
			x <- t$x[i]
			y <- t$y[i]
		} else {
			i <- NULL
		}
	}
	if ( jitter %in% c("x", "xy", "yx") )
		x <- jitter(x)
	if ( jitter %in% c("y", "xy", "yx") )
		y <- jitter(y)
	# encode non-required channels
	params <- merge_encoding(plot$params, mark$params, as_encoding(...))
	params <- normalize_encoding(params)
	more <- c("shape", "color", "fill", "alpha", "size", "linewidth", "linetype")
	more <- set_names(more, more)
	more <- lapply(more, encode_var, encoding=encoding,
		channels=plot$channels, params=params, subscripts=i)
	more$color <- add_alpha(more$color, more$alpha)
	# find non-required channels that encode groups
	groups <- compute_groups(plot, encoding, names(more))
	if ( is.null(i) ) {
		group_encoding <- encoding[names(groups)]
	} else {
		group_encoding <- lapply(encoding[names(groups)], `[`, i)
	}
	ngroups <- max(1, nrow(groups))
	# iterate over groups
	e <- plot$engine
	for ( j in seq_len(ngroups) )
	{
		# get encodings
		p <- c(list(x=x, y=y, z=z), more)
		# subset the group
		if ( length(groups) > 0L )
		{
			group <- groups[j,,drop=FALSE]
			label <- paste0(unlist(group), collapse=",")
			is_group <- Reduce(`&`, Map(`%in%`, group_encoding, group))
			p <- subset_list(p, is_group)
		} else {
			label <- NULL
		}
		if ( length(p$x) == 0L || length(p$y) == 0L )
			next
		# plot the group
		if ( e$name == "base" ) {
			plot.xy(xy.coords(p$x, p$y), pch=p$shape,
				cex=p$size, col=p$color, bg=p$fill,
				lwd=p$linewidth, lty=p$linetype, type=type)
		} else if ( e$name == "plotly" ) {
			if ( type == "p" ) {
				if ( !is.null(p$size) )
					p$size <- 20 * p$size
				e$plotly <- plotly::add_markers(e$plotly,
					x=p$x, y=p$y, z=p$z, color=I(p$color),
					size=I(p$size), symbol=I(p$shape), name=label)
			}
			if ( type == "l" ) {
				if ( sort || !is2d(plot) ) {
					e$plotly <- plotly::add_lines(e$plotly,
						x=p$x, y=p$y, z=p$z, color=I(p$color),
						linetype=I(p$linetype), name=label)
				} else {
					if ( length(p$color) > 1L )
						p$color <- p$color[-length(p$color)]
					if ( length(p$linetype) > 1L )
						p$linetype <- p$linetype[-length(p$linetype)]
					e$plotly <- plotly::add_segments(e$plotly,
						x=p$x[-length(p$x)], xend=p$x[-1L],
						y=p$y[-length(p$x)], yend=p$y[-1L],
						linetype=I(p$linetype), color=I(p$color),
						name=label)
				}
			}
			if ( type == "h" ) {
				e$plotly <- plotly::add_segments(e$plotly,
					x=p$x, y=p$y, z=p$z, xend=x, yend=0,
					color=I(p$color), name=label)
			}
		} else {
			matter_error("unsupported plot engine: ", sQuote(e$name))
		}
	}
	# encode legends
	static_params <- more[setdiff(names(more), names(encoding))]
	invisible(encode_legends(plot$channels, static_params, type))
}

plot.vizi_points <- function(x, plot = NULL, ...,
	n = Inf, downsampler = "lttb", jitter = "",
	sort = is.finite(n))
{
	invisible(plot_mark_xy(x, plot=plot, type="p", ...,
		n=n, downsampler=downsampler, jitter=jitter,
		sort=sort))
}

plot.vizi_lines <- function(x, plot = NULL, ...,
	n = Inf, downsampler = "lttb", jitter = "",
	sort = is.finite(n))
{
	invisible(plot_mark_xy(x, plot=plot, type="l", ...,
		n=n, downsampler=downsampler, jitter=jitter,
		sort=sort))
}

plot.vizi_peaks <- function(x, plot = NULL, ...,
	n = Inf, downsampler = "lttb", jitter = "",
	sort = is.finite(n))
{
	invisible(plot_mark_xy(x, plot=plot, type="h", ...,
		n=n, downsampler=downsampler, jitter=jitter,
		sort=sort))
}

plot_mark_text <- function(mark, plot = NULL, ...,
	adj = NULL, pos = NULL, offset = 0.5)
{
	# encode position + label channels
	encoding <- merge_encoding(plot$encoding, mark$encoding)
	x <- encode_var("x", encoding, plot$channels)
	y <- encode_var("y", encoding, plot$channels)
	z <- encode_var("z", encoding, plot$channels)
	text <- encode_var("text", encoding, plot$channels)
	text <- as.character(text)
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
	if ( !is2d(plot) && plot$engine$name == "base" )
	{
		# project 3d points
		pmat <- trans3d_get()
		t <- trans3d(x, y, z, pmat)
		i <- trans3d_sort(x, y, z, pmat)
		x <- t$x[i]
		y <- t$y[i]
		text <- text[i]
	} else {
		i <- NULL
	}
	# encode non-required channels
	params <- merge_encoding(plot$params, mark$params, as_encoding(...))
	params <- normalize_encoding(params)
	more <- c("color", "alpha", "size")
	more <- set_names(more, more)
	more <- lapply(more, encode_var, encoding=encoding,
		channels=plot$channels, params=params, subscripts=i)
	more$color <- add_alpha(more$color, more$alpha)
	# find non-required channels that encode groups
	groups <- compute_groups(plot, encoding, names(more))
	if ( is.null(i) ) {
		group_encoding <- encoding[names(groups)]
	} else {
		group_encoding <- lapply(encoding[names(groups)], `[`, i)
	}
	ngroups <- max(1, nrow(groups))
	# iterate over groups
	e <- plot$engine
	for ( j in seq_len(ngroups) )
	{
		# get encodings
		p <- c(list(x=x, y=y, z=z, text=text), more)
		# subset the group
		if ( length(groups) > 0L )
		{
			group <- groups[j,,drop=FALSE]
			label <- paste0(unlist(group), collapse=",")
			is_group <- Reduce(`&`, Map(`%in%`, group_encoding, group))
			p <- subset_list(p, is_group)
		} else {
			label <- NULL
		}
		if ( length(p$x) == 0L || length(p$y) == 0L )
			next
		# plot the group
		if ( e$name == "base" ) {
			text.default(p$x, p$y, labels=p$text, adj=adj, pos=pos,
				offset=offset, cex=p$size, col=p$color)
		} else if ( e$name == "plotly" ) {
			if ( !is.null(p$size) )
				p$size <- 20 * p$size
			e$plotly <- plotly::add_text(e$plotly, x=p$x, y=p$y, z=p$z,
				text=p$text, color=I(p$color), size=I(p$size), name=label)
		} else {
			matter_error("unsupported plot engine: ", sQuote(e$name))
		}
	}
	# encode legends
	invisible(encode_legends(plot$channels, list()))
}

plot.vizi_text <- function(x, plot = NULL, ...,
	adj = NULL, pos = NULL, offset = 0.5)
{
	invisible(plot_mark_text(x, plot=plot, ...,
		adj=adj, pos=pos, offset=offset))
}

plot_mark_intervals <- function(mark, plot = NULL, ...,
	length = 0.25, angle = 90)
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
	more <- c("shape", "color", "alpha", "size", "linewidth", "linetype")
	more <- set_names(more, more)
	more <- lapply(more, encode_var, encoding=encoding,
		channels=plot$channels, params=params)
	more$color <- add_alpha(more$color, more$alpha)
	# find non-required channels that encode groups
	groups <- compute_groups(plot, encoding, names(more))
	group_encoding <- encoding[names(groups)]
	ngroups <- max(1, nrow(groups))
	# iterate over groups
	e <- plot$engine
	for ( j in seq_len(ngroups) )
	{
		# get encodings
		p <- c(list(
			x=x, xmin=xmin, xmax=xmax,
			y=y, ymin=ymin, ymax=ymax), more)
		# subset the group
		if ( length(groups) > 0L )
		{
			group <- groups[j,,drop=FALSE]
			label <- paste0(unlist(group), collapse=",")
			is_group <- Reduce(`&`, Map(`%in%`, group_encoding, group))
			p <- subset_list(p, is_group)
		} else {
			label <- NULL
		}
		if ( length(p$x) == 0L && length(p$y) == 0L )
			next
		# plot the group
		if ( e$name == "base" ) {
			if ( !is.null(p$xmin) && !is.null(p$xmax) ) {
				arrows(p$xmin, p$y, p$xmax, p$y, length=length, angle=angle,
					col=p$color, lty=p$linetype, lwd=p$linewidth, code=3L)
				if ( !is.null(x) )
					points(x, y, col=p$color, pch=p$shape, cex=p$size)
			}
			if ( !is.null(p$ymin) && !is.null(p$ymax) ) {
				arrows(p$x, p$ymin, p$x, p$ymax, length=length, angle=angle,
					col=p$color, lty=p$linetype, lwd=p$linewidth, code=3L)
				if ( !is.null(y) )
					points(x, y, col=p$color, pch=p$shape, cex=p$size)
			}
		} else if ( e$name == "plotly" ) {
			if ( !is.null(p$size) )
				p$size <- 20 * p$size
			if ( !is.null(p$xmin) && !is.null(p$xmax) ) {
				if ( is.null(p$x) )
					p$x <- (p$xmin + p$xmax) / 2
				xplus <- abs(p$x - p$xmax)
				xminus <- abs(p$x - p$xmin)
				xerr <- list(array=xplus, arrayminus=xminus, color=I(p$color),
					thickness=p$linewidth, symmetric=FALSE)
				e$plotly <- plotly::add_markers(e$plotly, x=p$x, y=p$y,
					color=I(p$color), size=I(p$size), symbol=I(p$shape),
					error_x=xerr, name=label)
			}
			if ( !is.null(p$ymin) && !is.null(p$ymax) ) {
				if ( is.null(p$y) )
					p$y <- (p$ymin + p$ymax) / 2
				yplus <- abs(p$y - p$ymax)
				yminus <- abs(p$y - p$ymin)
				yerr <- list(array=yplus, arrayminus=yminus, color=I(p$color),
					thickness=p$linewidth, symmetric=FALSE)
				e$plotly <- plotly::add_markers(e$plotly, x=p$x, y=p$y,
					color=I(p$color), size=I(p$size), symbol=I(p$shape),
					error_y=yerr, name=label)
			}
		} else {
			matter_error("unsupported plot engine: ", sQuote(e$name))
		}
	}
	# encode legends
	static_params <- more[setdiff(names(more), names(encoding))]
	invisible(encode_legends(plot$channels, static_params, "l"))
}

plot.vizi_intervals <- function(x, plot = NULL, ...,
	length = 0.25, angle = 90)
{
	invisible(plot_mark_intervals(x, plot=plot, ...,
		length=length, angle=angle))
}

plot_mark_rules <- function(mark, plot = NULL, ...)
{
	# encode position channels
	encoding <- merge_encoding(plot$encoding, mark$encoding)
	x <- encode_var("x", encoding, plot$channels)
	y <- encode_var("y", encoding, plot$channels)
	if ( (length(x) == 0L && length(y) == 0L) )
		return()
	# decode positions if discrete
	if ( is_discrete(x) )
		x <- match(x, plot$channels$x$limits)
	if ( is_discrete(y) )
		y <- match(y, plot$channels$y$limits)
	# encode non-required channels
	params <- merge_encoding(plot$params, mark$params, as_encoding(...))
	params <- normalize_encoding(params)
	more <- c("color", "alpha", "linewidth", "linetype")
	more <- set_names(more, more)
	more <- lapply(more, encode_var, encoding=encoding,
		channels=plot$channels, params=params)
	more$color <- add_alpha(more$color, more$alpha)
	# find non-required channels that encode groups
	groups <- compute_groups(plot, encoding, names(more))
	group_encoding <- encoding[names(groups)]
	ngroups <- max(1, nrow(groups))
	rules <- list()
	# iterate over groups
	e <- plot$engine
	for ( j in seq_len(ngroups) )
	{
		# get encodings
		p <- c(list(x=x, y=y), more)
		# subset the group
		if ( length(groups) > 0L )
		{
			group <- groups[j,,drop=FALSE]
			label <- paste0(unlist(group), collapse=",")
			is_group <- Reduce(`&`, Map(`%in%`, group_encoding, group))
			p <- subset_list(p, is_group)
		} else {
			label <- NULL
		}
		if ( length(p$x) == 0L && length(p$y) == 0L )
			next
		# plot the group
		if ( e$name == "base" ) {
			if ( length(p$x) > 0L ) {
				abline(v=p$x, col=p$color,
					lwd=p$linewidth, lty=p$linetype)
			}
			if ( length(p$y) > 0L ) {
				abline(h=p$y, col=p$color,
					lwd=p$linewidth, lty=p$linetype)
			}
		} else if ( e$name == "plotly" ) {
			if ( length(p$x) > 0L ) {
				vlines <- lapply(p$x, function(xi)
					{
						list(x0=xi, x1=xi, y0=0, y1=1,
							type="line", yref="paper",
							line=list(color=p$color,
								width=p$linewidth))
					})
				rules <- c(rules, vlines)
			}
			if ( length(p$y) > 0L ) {
				hlines <- lapply(p$y, function(yi)
					{
						list(x0=0, x1=1, y0=yi, y1=yi,
							type="line", xref="paper",
							line=list(color=p$color,
								width=p$linewidth))
					})
				rules <- c(rules, hlines)
			}
		} else {
			matter_error("unsupported plot engine: ", sQuote(e$name))
		}
	}
	if ( e$name == "plotly" )
		e$plotly <- plotly::layout(e$plotly, shapes=rules)
	# encode legends
	static_params <- more[setdiff(names(more), names(encoding))]
	invisible(encode_legends(plot$channels, static_params, "l"))
}

plot.vizi_rules <- function(x, plot = NULL, ...)
{
	invisible(plot_mark_rules(x, plot=plot, ...))
}

plot_mark_bars <- function(mark, plot = NULL, ...,
	width = 1, stack = FALSE)
{
	# encode position channels
	encoding <- merge_encoding(plot$encoding, mark$encoding)
	x <- encode_var("x", encoding, plot$channels)
	y <- encode_var("y", encoding, plot$channels)
	if ( length(x) == 0L || length(y) == 0L )
		return()
	# get parameters
	if ( !is.null(mark$params$width) )
		width <- mark$params$width
	if ( !is.null(mark$params$stack) )
		stack <- mark$params$stack
	# encode non-required channels
	params <- merge_encoding(plot$params, mark$params, as_encoding(...))
	params <- normalize_encoding(params)
	more <- c("color", "fill", "alpha", "linewidth")
	more <- set_names(more, more)
	more <- lapply(more, encode_var, encoding=encoding,
		channels=plot$channels, params=params)
	more$color <- add_alpha(more$color, more$alpha)
	more$fill <- add_alpha(more$fill, more$alpha)
	# find non-required channels that encode groups
	groups <- compute_groups(plot, encoding, names(more))
	group_encoding <- encoding[names(groups)]
	ngroups <- max(1, nrow(groups))
	if ( length(groups) > 1L )
		matter_error("multiple group encodings not allowed for mark 'bars': ",
			paste0(names(groups), collapse=", "))
	# determine orientation
	if ( is_discrete(x) && is_discrete(y) ) {
		matter_error("one of 'x' or 'y' must be numeric")
	} else if ( is_discrete(x) ) {
		horizontal <- FALSE
	} else if ( is_discrete(y) ) {
		horizontal <- TRUE
	} else {
		matter_error("one of 'x' or 'y' must be discrete")
	}
	# sum counts for each bar
	if ( length(groups) > 0L ) {
		bar_encoding <- data.frame(x=x, y=y,
			group=group_encoding[[1L]], more[names(groups)])
	} else {
		bar_encoding <- data.frame(x=x, y=y)
	}
	if ( horizontal ) {
		bar_encoding <- aggregate(bar_encoding, x ~ ., sum)
	} else {
		bar_encoding <- aggregate(bar_encoding, y ~ ., sum)
	}
	# re-encode the summarization
	x <- bar_encoding[["x"]]
	y <- bar_encoding[["y"]]
	if ( length(groups) > 0L ) {
		group_encoding <- set_names(bar_encoding["group"], names(groups))
		more[names(groups)] <- bar_encoding[names(groups)]
	}
	# make the plot
	e <- plot$engine
	if ( e$name == "base" ) {
		# create grouping vector
		if ( length(groups) > 0L ) {
			group_levels <- plot$channels[[names(groups)]]$limits
			groups <- match(group_encoding[[1L]], group_levels)
		}
		# create plotting function
		if ( horizontal ) {
			centers <- match(y, plot$channels$y$limits)
			heights <- x
			bars <- function(sides, heights, ...)
			{
				rect(heights[,1L], sides[,1L],
					heights[,2L], sides[,2L], ...)
			}
		} else {
			centers <- match(x, plot$channels$x$limits)
			heights <- y
			bars <- function(sides, heights, ...)
			{
				rect(sides[,1L], heights[,1L],
					sides[,2L], heights[,2L], ...)
			}
		}
		# calculate the bars
		left <- centers - 0.5 * width
		right <- centers + 0.5 * width
		floors <- rep.int(0, length(centers))
		if ( anyDuplicated(centers) )
		{
			if ( stack ) {
				for ( uc in unique(centers) ) {
					i <- which(centers %in% uc)
					if ( length(groups) > 0L )
						i <- i[order(groups[i])]
					ustack <- cumsum(c(0, heights[i]))
					floors[i] <- ustack[seq_along(i)]
				}
				heights <- floors + heights
			} else {
				for ( uc in unique(centers) ) {
					i <- which(centers %in% uc)
					if ( length(groups) > 0L )
						i <- i[order(groups[i])]
					n <- length(i) + 1L
					udodge <- seq(left[i][1L], right[i][1L], length.out=n)
					left[i] <- udodge[-length(udodge)]
					right[i] <- udodge[-1L]
				}
			}
		}
		# plot the bars
		bars(sides=cbind(left, right), heights=cbind(floors, heights),
			col=more$fill, border=more$color, lwd=more$linewidth)
	} else if ( e$name == "plotly" ) {
		# calculate bar width
		if ( !stack )
			width <- width / ngroups
		# iterate over groups
		for ( j in seq_len(ngroups) )
		{
			# get encodings
			p <- c(list(x=x, y=y), more)
			# subset the group
			if ( length(groups) > 0L )
			{
				group <- groups[j,,drop=FALSE]
				label <- paste0(unlist(group), collapse=",")
				is_group <- Reduce(`&`, Map(`%in%`, group_encoding, group))
				p <- subset_list(p, is_group)
			} else {
				label <- NULL
			}
			if ( length(p$x) == 0L || length(p$y) == 0L )
				next
			# make the plot
			e$plotly <- plotly::add_bars(e$plotly, x=p$x, y=p$y,
				marker=list(line=list(color=I(p$color), width=p$linewidth)),
				color=I(p$fill), width=width, name=label)
		}
		if ( stack ) {
			e$plotly <- plotly::layout(e$plotly, barmode="stack")
		} else {
			e$plotly <- plotly::layout(e$plotly, barmode="group")
		}
	} else {
		matter_error("unsupported plot engine: ", sQuote(e$name))
	}
	# encode legends
	invisible(encode_legends(plot$channels, list()))
}

plot.vizi_bars <- function(x, plot = NULL, ...,
	width = 1, stack = FALSE)
{
	invisible(plot_mark_bars(mark=x, plot=plot, ...,
		width=width, stack=stack))
}

plot_mark_boxplot <- function(mark, plot = NULL, ...,
	range = 1.5, notch = FALSE, width = 0.8)
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
	if ( !is.null(mark$params$width) )
		width <- mark$params$width
	# encode non-required channels
	params <- merge_encoding(plot$params, mark$params, as_encoding(...))
	params <- normalize_encoding(params)
	more <- c("color", "fill", "alpha")
	more <- set_names(more, more)
	more <- lapply(more, encode_var, encoding=encoding,
		channels=plot$channels, params=params)
	more$color <- add_alpha(more$color, more$alpha)
	more$fill <- add_alpha(more$fill, more$alpha)
	# find non-required channels that encode groups
	groups <- compute_groups(plot, encoding, names(more))
	group_encoding <- encoding[names(groups)]
	ngroups <- max(1, nrow(groups))
	if ( length(groups) > 1L )
		matter_error("multiple group encodings not allowed for mark 'boxplot': ",
			paste0(names(groups), collapse=", "))
	# calculate box positions
	if ( ngroups <= 1L ||
		is_discrete(x) && setequal(groups[[1L]], x) ||
		is_discrete(y) && setequal(groups[[1L]], y) )
	{
		grouped <- FALSE
		adj <- rep.int(0, ngroups)
	} else {
		grouped <- TRUE
		adj <- seq(-0.5, 0.5, length.out=ngroups + 2L)
		adj <- adj[-c(1L, length(adj))]
	}
	# iterate over groups
	e <- plot$engine
	for ( j in seq_len(ngroups) )
	{
		# get encodings
		p <- c(list(x=x, y=y), more)
		# subset the group
		if ( length(groups) > 0L )
		{
			group <- groups[j,,drop=FALSE]
			label <- paste0(unlist(group), collapse=",")
			is_group <- Reduce(`&`, Map(`%in%`, group_encoding, group))
			p <- subset_list(p, is_group)
		} else {
			label <- NULL
		}
		if ( length(p$x) == 0L || length(p$y) == 0L )
			next
		if ( e$name == "base" ) {
			# prepare parameters
			box_params <- function(param, index) {
				if ( length(param) == length(index) ) {
					tapply(param, index, unique)
				} else {
					param
				}
			}
			if ( is_discrete(p$x) && is_discrete(p$y) ) {
				matter_error("one of 'x' or 'y' must be numeric")
			} else if ( is_discrete(p$x) ) {
				horiz <- FALSE
				vals <- tapply(p$y, p$x, identity, simplify=FALSE)
				p <- lapply(p[-match("y", names(p))], box_params, index=p$x)
				at <- as.integer(p$x) + adj[j]
			} else if ( is_discrete(p$y) ) {
				horiz <- TRUE
				vals <- tapply(p$x, p$y, identity, simplify=FALSE)
				p <- lapply(p[-match("x", names(p))], box_params, index=p$y)
				at <- as.integer(p$y) + adj[j]
			} else {
				matter_error("one of 'x' or 'y' must be discrete")
			}
			nz <- lengths(vals) > 0L
			vals <- vals[nz]
			p <- subset_list(p, nz)
			at <- at[nz]
			# plot the group
			if ( grouped ) {
				boxwex <- width / (ngroups + 1L)
			} else {
				boxwex <- width
			}
			relwidth <- rep.int(1, length(vals))
			boxplot.default(vals, range=range, width=relwidth, notch=notch,
				at=at, border=p$color, col=p$fill, horizontal=horiz,
				pars=list(boxwex=boxwex), axes=FALSE, add=TRUE)
		} else if ( e$name == "plotly" ) {
			e$plotly <- plotly::add_boxplot(e$plotly, x=p$x, y=p$y,
				color=I(p$color), fillcolor=p$fill,
				notched=notch, name=label)
			if ( grouped )
				e$plotly <- plotly::layout(e$plotly, boxmode="group")
		} else {
			matter_error("unsupported plot engine: ", sQuote(e$name))
		}
	}
	# encode legends
	invisible(encode_legends(plot$channels, list()))
}

plot.vizi_boxplot <- function(x, plot = NULL, ...,
	range = 1.5, notch = FALSE, width = 0.8)
{
	invisible(plot_mark_boxplot(mark=x, plot=plot, ...,
		range=range, notch=notch, width=width))
}

plot_mark_image <- function(mark, plot = NULL, ...,
	alpha = NA, interpolate = TRUE, maxColorValue = 1)
{
	# encode position channels
	encoding <- merge_encoding(plot$encoding, mark$encoding)
	xmin <- encode_var("xmin", encoding, plot$channels)
	xmax <- encode_var("xmax", encoding, plot$channels)
	ymin <- encode_var("ymin", encoding, plot$channels)
	ymax <- encode_var("ymax", encoding, plot$channels)
	if ( length(xmin) == 0L )
		xmin <- min(plot$channels[["x"]]$limits)
	if ( length(xmax) == 0L )
		xmax <- max(plot$channels[["x"]]$limits)
	if ( length(ymin) == 0L )
		ymin <- min(plot$channels[["y"]]$limits)
	if ( length(ymax) == 0L )
		ymax <- max(plot$channels[["y"]]$limits)
	# encode images
	image <- encoding[["image"]]
	if ( !is.list(image) )
		matter_error("'image' must be a list of images")
	# get parameters
	if ( !is.null(mark$params$alpha) )
		alpha <- mark$params$alpha
	if ( !is.null(mark$params$interpolate) )
		interpolate <- mark$params$interpolate
	if ( !is.null(mark$params$maxColorValue) )
		maxColorValue <- mark$params$maxColorValue
	# plot images
	e <- plot$engine
	for ( i in seq_along(image) )
	{
		label <- names(image)[i]
		rc <- as.raster(image[[i]], max=maxColorValue)
		if ( !is.na(alpha) && alpha >= 0 && alpha <= 1 )
			rc <- add_alpha(rc, alpha)
		dxi <- 0.5 * (xmax[i] - xmin[i]) / (ncol(rc) - 1)
		dyi <- 0.5 * (ymax[i] - ymin[i]) / (nrow(rc) - 1)
		rev <- if (is.null(plot$coord$rev)) "" else plot$coord$rev
		if ( e$name == "base" ) {
			hasRaster <- dev.capabilities("rasterImage")$rasterImage
			if ( hasRaster == "no" )
				matter_error("device does not have raster capabilities")
			# flip x axis?
			if ( rev %in% c("x", "xy", "yx") ) {
				xleft <- xmax[i] + dxi
				xright <- xmin[i] - dxi
			} else {
				xleft <- xmin[i] - dxi
				xright <- xmax[i] + dxi
			}
			# flip y axis?
			if ( rev %in% c("y", "xy", "yx") ) {
				ytop <- ymin[i] - dyi
				ybottom <- ymax[i] + dyi
			} else {
				ytop <- ymax[i] + dyi
				ybottom <- ymin[i] - dyi
			}
			rasterImage(rc,
				xleft=xleft, ybottom=ybottom,
				xright=xright, ytop=ytop,
				interpolate=interpolate)
		} else if ( e$name == "plotly" ) {
			# flip x axis?
			if ( rev %in% c("x", "xy", "yx") ) {
				x0 <- xmax[i]
				pxi <- -2 * dxi
			} else {
				x0 <- xmin[i]
				pxi <- 2 * dxi
			}
			# flip y axis?
			if ( rev %in% c("y", "xy", "yx") ) {
				y0 <- ymin[i]
				pyi <- 2 * dyi
			} else {
				y0 <- ymax[i]
				pyi <- -2 * dyi
			}
			e$plotly <- plotly::add_image(e$plotly, z=rc,
				x0=x0, y0=y0, dx=pxi, dy=pyi, name=label)
		} else {
			matter_error("unsupported plot engine: ", sQuote(e$name))
		}
	}
	list()
}

plot.vizi_image <- function(x, plot = NULL, ...,
	alpha = NA, interpolate = TRUE, maxColorValue = 1)
{
	invisible(plot_mark_image(mark=x, plot=plot, ...,
		alpha=alpha, interpolate=interpolate, maxColorValue=maxColorValue))
}

compute_raster <- function(mark, plot = NULL, ...,
	enhance = FALSE, smooth = FALSE, scale = FALSE,
	slice = NULL, tol = 1e-6, asis = FALSE)
{
	# encode position channels
	encoding <- merge_encoding(plot$encoding, mark$encoding)
	x <- encode_var("x", encoding, plot$channels)
	y <- encode_var("y", encoding, plot$channels)
	z <- encode_var("z", encoding, plot$channels)
	# get alpha (w/out encoding -- allow setting via ...)
	params <- merge_encoding(plot$params, mark$params, as_encoding(...))
	params <- normalize_encoding(params)
	if ( has_alpha(plot) ) {
		alpha <- encoding[["alpha"]]
	} else if ( "alpha" %in% names(params) ) {
		alpha <- params[["alpha"]]
	} else {
		alpha <- 1
	}
	if ( length(x) == 0L || length(y) == 0L )
		return()
	# get name of color channel
	if ( "color" %in% names(plot$channels) ) {
		cname <- "color"
	} else if ( "fill" %in% names(plot$channels) ) {
		cname <- "fill"
	} else {
		matter_error("couldn't find encoding for 'color' or 'fill'")
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
	# get alpha limits
	alim <- plot$channels[["alpha"]]$limits
	if ( is.null(alim) )
		alim <- get_limits(alpha)
	alpha <- encode_limits(alpha, alim)
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
			matter_error("slice must be a named scalar")
		if ( !ortho %in% c("x", "y", "z") )
			matter_error("slice must be named x, y, or z")
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
			alim <- range(ra, na.rm=TRUE)
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
			alim <- range(ra, na.rm=TRUE)
		}
	}
	if ( (is.numeric(rc) || is.numeric(ra)) && isTRUE(scale) ) {
		# scaling
		if ( !const_color && is.numeric(rc) ) {
			clim <- c(0, 100)
			rc <- rescale_range(rc, clim)
		}
		if ( !const_alpha && is.numeric(ra) ) {
			alim <- c(0, 100)
			ra <- rescale_range(ra, alim)
		}
	}
	# encode color scheme
	csch <- plot$channels[[cname]]$scheme
	if ( is.null(csch) )
		csch <- get_scheme(cname, rc)
	if ( asis ) {
		if ( is.function(csch) ) {
			fx <- csch
			if ( is_discrete(rc) ) {
				n <- length(clim)
			} else {
				n <- 256L
			}
			csch <- fx(n)
		}
		if ( is_discrete(rc) )
		{
			csch <- csch[clim %in% rc]
			clim <- clim[clim %in% rc]
			if ( const_color ) {
				rc <- ra
			} else {
				rc <- array(match(rc, clim), dim=dim(rc))
			}
		}
		if ( const_alpha ) {
			asch <- 1L
		} else {
			asch <- seq(0, 1, length.out=256L)
		}
		csch <- add_alpha(csch, asch)
	} else {
		if ( const_alpha ) {
			ra <- pmax(ra, 0)
			ra <- pmin(ra, 1)
		} else {
			ra <- rescale_range(ra, c(0, 1))
		}
		rc <- encode_scheme(rc, csch, clim)
		rc <- add_alpha(rc, ra)
		dim(rc) <- dm
	}
	# get raster label/limits
	if ( const_color ) {
		label <- color[1L]
		if ( !const_alpha )
		{
			cname <- "alpha"
			clim <- alim
		}
	} else if ( const_alpha ) {
		label <- alpha[1L]
	} else {
		label <- NULL
	}
	# return raster
	list(raster=rc, channel=cname, label=label,
		limits=clim, scheme=csch, ortho=ortho,
		i=range(i, na.rm=TRUE),
		j=range(j, na.rm=TRUE),
		x=x, y=y, z=z)
}

plot_mark_pixels <- function(mark, plot = NULL, ...,
	enhance = FALSE, smooth = FALSE, scale = FALSE,
	useRaster = TRUE)
{
	# compute raster
	e <- plot$engine
	asis <- e$name != "base"
	rs <- compute_raster(mark, plot, asis=asis,
		enhance=enhance, smooth=smooth, scale=scale, ...)
	rc <- rs$raster
	if ( is.null(rc) )
		return()
	# plot the image
	if ( e$name == "base" ) {
		rev <- if (is.null(plot$coord$rev)) "" else plot$coord$rev
		hasRaster <- dev.capabilities("rasterImage")$rasterImage
		if ( !is2d(plot) || hasRaster != "yes" )
			useRaster <- FALSE
		if ( !is.null(mark$params$useRaster) )
			useRaster <- useRaster && isTRUE(mark$params$useRaster)
		if ( useRaster ) {
			# plot raster
			rc <- t(rc)
			di <- 0.5 * diff(rs$i) / (ncol(rc) - 1)
			dj <- 0.5 * diff(rs$j) / (nrow(rc) - 1)
			# flip x axis?
			if ( rev %in% c("x", "xy", "yx") ) {
				xleft <- rs$i[2L] + di
				xright <- rs$i[1L] - di
			} else {
				xleft <- rs$i[1L] - di
				xright <- rs$i[2L] + di
			}
			# flip y axis?
			if ( rev %in% c("y", "xy", "yx") ) {
				ytop <- rs$j[1L] - dj
				ybottom <- rs$j[2L] + dj
			} else {
				ytop <- rs$j[2L] + dj
				ybottom <- rs$j[1L] - dj
			}
			rasterImage(as.raster(rc),
				xleft=xleft, ybottom=ybottom,
				xright=xright, ytop=ytop,
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
	} else if ( e$name == "plotly" ) {
		# plot heatmap
		if ( !is2d(plot) )
			matter_error("'pixels' must be 2d for engine 'plotly'; use 'voxels'")
		x <- seq(rs$i[1L], rs$i[2L], length.out=nrow(rc))
		y <- seq(rs$j[1L], rs$j[2L], length.out=ncol(rc))
		if ( has_alpha(plot) ) {
			n <- length(rs$scheme)
			if ( n > 1L )
				rs$scheme <- Map(list, seq(0, 1, length.out=n), rs$scheme)
			e$plotly <- plotly::add_heatmap(e$plotly,
				x=x, y=y, z=t(rc), colorscale=rs$scheme,
				colorbar=list(title=list(text=rs$label)), name=rs$label)
		} else {
			e$plotly <- plotly::add_heatmap(e$plotly,
				x=x, y=y, z=t(rc), colors=rs$scheme, name=rs$label)
		}
	} else {
		matter_error("unsupported plot engine: ", sQuote(e$name))
	}
	# encode legends
	if ( rs$channel %in% names(plot$channels) )
		plot$channels[[rs$channel]]$limits <- rs$limits
	invisible(encode_legends(plot$channels, list()))
}

plot_mark_voxels <- function(mark, plot = NULL, ...,
	enhance = FALSE, smooth = FALSE, scale = FALSE,
	xslice = NULL, yslice = NULL, zslice = NULL)
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
		set_names(xslice, rep.int("x", length(xslice))),
		set_names(yslice, rep.int("y", length(yslice))),
		set_names(zslice, rep.int("z", length(zslice))))
	if ( length(slices) == 0L ) {
		slices <- sort(unique(z))
		names(slices) <- rep.int("z", length(slices))
	}
	n <- length(slices)
	slices <- lapply(seq_len(n), function(k) slices[k])
	# compute rasters
	e <- plot$engine
	asis <- e$name != "base"
	rss <- lapply(slices, function(slice) {
			compute_raster(mark, plot, asis=asis,
				enhance=enhance, smooth=smooth, scale=scale,
				slice=slice, ...)
		})
	# plot the volumes
	if ( e$name == "base" ) {
		px <- list()
		py <- list()
		colors <- list()
		depths <- list()
		for ( i in seq_along(rss) ) {
			# get raster slice
			rs <- rss[[i]]
			rc <- rs$raster
			# project raster to 3d polygons
			p <- pix2poly(rs$i, rs$j, dim(rc))
			pmat <- trans3d_get()
			i <- as.vector(p$x)
			j <- as.vector(p$y)
			if ( rs$ortho == "z" ) {
				p <- trans3d(i, j, rs$z[1L], pmat)
				d <- trans3d_depth(i, j, rs$z[1L], pmat)
			} else if ( rs$ortho == "y" ) {
				p <- trans3d(i, rs$y[1L], j, pmat)
				d <- trans3d_depth(i, rs$y[1L], j, pmat)
			} else if ( rs$ortho == "x" ) {
				p <- trans3d(rs$x[1L], i, j, pmat)
				d <- trans3d_depth(rs$x[1L], i, j, pmat)
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
		polygon(px, py, col=colors, border=colors)
	} else if ( e$name == "plotly" ) {
		FUN <- function(rs, d) {
			rc <- rs$raster
			co <- expand.grid(
				i=seq(rs$i[1L], rs$i[2L], length.out=nrow(rc)),
				j=seq(rs$j[1L], rs$j[2L], length.out=ncol(rc)))
			ns <- length(rc)
			if ( rs$ortho == "z" ) {
				switch(d, x=co$i, y=co$j, z=rep_len(rs$z, ns))
			} else if ( rs$ortho == "y" ) {
				switch(d, x=co$i, y=rep_len(rs$y, ns), z=rs$z[1L])
			} else if ( rs$ortho == "x" ) {
				switch(d, x=rep_len(rs$x, ns), y=co$i, z=co$j)
			}
		}
		vx <- unlist(lapply(rss, FUN, "x"))
		vy <- unlist(lapply(rss, FUN, "y"))
		vz <- unlist(lapply(rss, FUN, "z"))
		vals <- unlist(lapply(rss, `[[`, "raster"))
		csch <- rss[[1L]]$scheme
		n <- length(csch)
		if ( n > 1L )
			csch <- Map(list, seq(0, 1, length.out=n), csch)
		if ( has_alpha(plot) ) {
			asch <- "max"
		} else {
			asch <- "uniform"
		}
		e$plotly <- plotly::add_trace(e$plotly,
			x=vx, y=vy, z=vz, value=vals,
			surface=list(count=2L * length(slices)),
			colorscale=csch, opacityscale=asch, type="volume")
	} else {
		matter_error("unsupported plot engine: ", sQuote(e$name))
	}
	# encode legends
	cname <- rss[[1L]]$channel
	clim <- do.call(merge_limits, lapply(rss, `[[`, "limits"))
	if ( cname %in% names(plot$channels) )
		plot$channels[[cname]]$limits <- clim
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

plot.vizi_pixels <- function(x, plot = NULL, ...,
	enhance = FALSE, smooth = FALSE, scale = FALSE,
	useRaster = TRUE)
{
	invisible(plot_mark_pixels(mark=x, plot=plot, ...,
		enhance=enhance, smooth=smooth, scale=scale,
		useRaster=useRaster))
}

plot.vizi_voxels <- function(x, plot = NULL, ...,
	xslice = NULL, yslice = NULL, zslice = NULL)
{
	invisible(plot_mark_voxels(mark=x, plot=plot, ...,
		xslice=xslice, yslice=yslice, zslice=zslice))
}

setOldClass("vizi_points")
setOldClass("vizi_lines")
setOldClass("vizi_peaks")
setOldClass("vizi_text")
setOldClass("vizi_intervals")
setOldClass("vizi_rules")
setOldClass("vizi_bars")
setOldClass("vizi_boxplot")
setOldClass("vizi_image")
setOldClass("vizi_pixels")
setOldClass("vizi_voxels")

setMethod("plot", "vizi_points", plot.vizi_points)
setMethod("plot", "vizi_lines", plot.vizi_lines)
setMethod("plot", "vizi_peaks", plot.vizi_peaks)
setMethod("plot", "vizi_text", plot.vizi_text)
setMethod("plot", "vizi_intervals", plot.vizi_intervals)
setMethod("plot", "vizi_rules", plot.vizi_rules)
setMethod("plot", "vizi_bars", plot.vizi_bars)
setMethod("plot", "vizi_boxplot", plot.vizi_boxplot)
setMethod("plot", "vizi_image", plot.vizi_image)
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

par_style_options <- function()
{
	list(
		"light" = par_style_light,
		"dark" = par_style_dark,
		"voidlight" = par_style_voidlight,
		"voiddark" = par_style_voiddark,
		"transparent" = par_style_transparent)
}

par_style_fun <- function(method)
{
	options <- par_style_options()
	if ( is.character(method) )
		method <- tolower(method)
	options[[match.arg(method, names(options))]]
}

par_style <- function(style, ...)
{
	par_style_fun(style)(...)
}

par_style_new <- function(params = list(), ...)
{
	p <- list(
		bty = "n",
		mar = c(0.5, 0.5, 1, 1),	# inner margins
		oma = c(3, 3, 1, 1),		# outer margins
		mgp = c(1.5, 0.5, 0),		# adjust axes
		xaxt = "s",
		yaxt = "s")
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

par_style_voidlight <- function(params = list(), ..., new = FALSE)
{
	if ( new ) {
		params <- par_update(par_style_new(), more=params)
		params <- par_update(params,
			mar = c(0, 0, 0, 0),
			oma = c(0, 0, 1, 0))
	}
	p <- list(
		fg="black",
		bg="white",
		col="black",
		col.axis="transparent",
		col.lab="transparent",
		col.main="transparent",
		col.sub="transparent",
		xaxt="n",
		yaxt="n")
	par_update(p, ..., more=params)
}

par_style_voiddark <- function(params = list(), ..., new = FALSE)
{
	if ( new ) {
		params <- par_update(par_style_new(), more=params)
		params <- par_update(params,
			mar = c(0, 0, 0, 0),
			oma = c(0, 0, 1, 0))
	}
	p <- list(
		fg="white",
		bg="black",
		col="white",
		col.axis="transparent",
		col.lab="transparent",
		col.main="transparent",
		col.sub="transparent",
		xaxt="n",
		yaxt="n")
	par_update(p, ..., more=params)
}

par_style_transparent <- function(params = list(), ..., new = FALSE)
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
		matter_error("dim must be a length-2 numeric vector")
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
		matter_error("no graphics device open")
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
		matter_error("no graphics device open")
	if ( is.null(pgrid) )
		pgrid <- getOption("matter.vizi.panelgrid")
	if ( is.null(params) )
		params <- pgrid$par
	if ( is.null(params) )
		matter_error("nothing to restore; has panel_save() been called?")
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
		matter_error("no graphics device open")
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

panel_set <- function(which = -1L, pgrid = NULL, new = NULL)
{
	if ( dev.cur() == 1L )
		matter_error("no graphics device open")
	if ( is.null(pgrid) )
		pgrid <- getOption("matter.vizi.panelgrid")
	if ( is.null(pgrid) )
		matter_error("no panel grid found")
	mfg <- par("mfg")
	if ( length(which) == 1L ) {
		if ( which < 1 )
			which <- length(pgrid$mat) + which + 1
		nxt <- which(pgrid$mat == which, arr.ind=TRUE)
	} else if ( length(which) == 2L ) {
		nxt <- which
	} else {
		matter_error("which must be length 1 or 2")
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
		matter_error("no graphics device open")
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
