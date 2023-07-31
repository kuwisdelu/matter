
#### Plotting methods for 'vizi' marks ####
## ----------------------------------------

plot_mark_xy <- function(mark, plot = NULL, ...,
	n = Inf, downsampler = "lttb", type = "p", add = FALSE)
{
	# encode required channels
	encoding <- merge_encoding(plot$encoding, mark$encoding)
	x <- encode_var("x", encoding, plot$channels)
	y <- encode_var("y", encoding, plot$channels)
	if ( length(x) == 0L || length(y) == 0L )
		return()
	# perform transformations
	t <- mark$trans
	if ( !is.null(t$n) )
		n <- t$n
	if ( !is.null(t$downsampler) )
		downsampler <- t$downsampler
	if ( n < length(y) ) {
		y <- downsample(y, n=n, domain=x, method=downsampler)
		i <- attr(y, "sample")
		x <- x[i]
	} else {
		i <- NULL
	}
	# encode non-required channels
	params <- merge_encoding(mark$params, as_encoding(...))
	p <- c("shape", "color", "fill", "alpha", "size", "linewidth", "linetype")
	p <- lapply(setNames(p, p), encode_var, encoding=encoding,
		channels=plot$channels, params=params, subset=i)
	p$color <- add_alpha(p$color, p$alpha)
	if ( !add ) {
		xr <- range(x, na.rm=TRUE)
		yr <- range(y, na.rm=TRUE)
		plot.new()
		plot.window(xlim=xr, ylim=yr)
	}
	plot.xy(xy.coords(x, y), pch=p$shape, col=p$color, bg=p$fill,
		cex=p$size, lwd=p$linewidth, lty=p$linetype, type=type)
	params <- p[!names(p) %in% names(encoding)]
	invisible(encode_legends(plot$channels, params, type))
}

plot.vizi_points <- function(x, plot = NULL, add = FALSE, ...,
	n = Inf, downsampler = "lttb")
{
	invisible(plot_mark_xy(mark=x, plot=plot, type="p", add=add, ...,
		n = Inf, downsampler = "lttb"))
}

plot.vizi_lines <- function(x, plot = NULL, add = FALSE, ...,
	n = Inf, downsampler = "lttb")
{
	invisible(plot_mark_xy(mark=x, plot=plot, type="l", add=add, ...,
		n = Inf, downsampler = "lttb"))
}

plot.vizi_peaks <- function(x, plot = NULL, add = FALSE, ...,
	n = Inf, downsampler = "lttb")
{
	invisible(plot_mark_xy(mark=x, plot=plot, type="h", add=add, ...,
		n = Inf, downsampler = "lttb"))
}

plot_mark_pixels <- function(mark, plot = NULL, ...,
	enhance = FALSE, smooth = FALSE, useRaster = TRUE,
	add = FALSE)
{
	# encode required channels (except color)
	encoding <- merge_encoding(plot$encoding, mark$encoding)
	x <- encode_var("x", encoding, plot$channels)
	y <- encode_var("y", encoding, plot$channels)
	alpha <- encode_var("alpha", encoding, plot$channels)
	if ( length(x) == 0L || length(y) == 0L )
		return()
	# get color variable (w/out encoding)
	color <- encoding$color
	if ( is.null(color) )
		color <- encoding$fill
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
	zlim <- plot$channels[["color"]]$limits
	if ( is.null(zlim) )
		zlim <- get_limits(color)
	color <- encode_limits(color, zlim)
	# rasterize color/alpha
	if ( is.factor(color) )
		color <- as.character(color)
	if ( is.factor(alpha) )
		alpha <- as.character(alpha)
	zc <- to_raster(x, y, color)
	za <- to_raster(x, y, alpha)
	dm <- dim(zc)
	# perform transformations
	t <- mark$trans
	if ( !is.null(t$enhance) )
		enhance <- t$enhance
	if ( !is.null(t$smooth) )
		smooth <- t$smooth
	const_color <- n_unique(color) == 1L
	const_alpha <- n_unique(alpha) == 1L
	if ( is.character(enhance) || isTRUE(enhance) ) {
		fn <- enhance_fun(enhance)
		if ( !const_color && is.numeric(zc) ) {
			zc <- fn(zc)
			zlim <- range(zc, na.rm=TRUE)
		}
		if ( !const_alpha && is.numeric(za) ) {
			za <- fn(za)
			za <- za - min(za, na.rm=TRUE)
			za <- za / max(za, na.rm=TRUE)
		}
	}
	if ( is.character(smooth) || isTRUE(smooth) ) {
		fn <- filt2_fun(smooth)
		if ( !const_color && is.numeric(zc) ) {
			zc <- fn(zc)
			zlim <- range(zc, na.rm=TRUE)
		}
		if ( !const_alpha && is.numeric(za) ) {
			za <- fn(za)
			za <- za - min(za, na.rm=TRUE)
			za <- za / max(za, na.rm=TRUE)
		}
	}
	# encode color scheme
	zcol <- plot$channels[["color"]]$scheme
	if ( is.null(zcol) )
		zcol <- get_scheme("color", zc)
	zc <- encode_scheme(zc, zcol, zlim)
	zc <- add_alpha(zc, za)
	dim(zc) <- dm
	# plot the image
	xr <- range(x, na.rm=TRUE)
	yr <- range(y, na.rm=TRUE)
	if ( !add ) {
		plot.new()
		plot.window(xlim=xr, ylim=yr)
	}
	ras <- dev.capabilities("rasterImage")$rasterImage
	if ( useRaster && ras != "yes" )
		useRaster <- FALSE
	if ( useRaster ) {
		zc <- t(zc)[ncol(zc):1L,,drop=FALSE]
		dx <- 0.5 * (xr[2L] - xr[1L]) / (dim(zc)[1L] - 1)
		dy <- 0.5 * (yr[2L] - yr[1L]) / (dim(zc)[2L] - 1)
		rasterImage(as.raster(zc),
			xleft=xr[1L] - dx, ybottom=yr[1L] - dy,
			xright=xr[2L] + dx, ytop=yr[2L] + dy,
			interpolate=FALSE)
	} else {
		p <- pix2poly(xr, yr, dim(zc))
		polygon(p$x, p$y, col=zc, border=zc)
	}
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
	enhance = FALSE, smooth = FALSE, useRaster = TRUE)
{
	invisible(plot_mark_pixels(mark=x, plot=plot, add=add, ...,
		enhance=enhance, smooth=smooth, useRaster=useRaster))
}

setOldClass("vizi_points")
setOldClass("vizi_lines")
setOldClass("vizi_peaks")
setOldClass("vizi_pixels")

setMethod("plot", "vizi_points", plot.vizi_points)
setMethod("plot", "vizi_lines", plot.vizi_lines)
setMethod("plot", "vizi_peaks", plot.vizi_peaks)
setMethod("plot", "vizi_pixels", plot.vizi_pixels)

#### Set graphical parameters ####
## -------------------------------

par_update <- function(params, ..., more = list())
{
	update <- c(list(...), more)
	for ( nm in names(update) )
		params[[nm]] <- update[[nm]]
	params
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

par_style_light <- function(params = list(), ..., new = TRUE)
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

par_style_dark <- function(params = list(), ..., new = TRUE)
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
	panel_get(pgrid, arr.ind=TRUE)[2L] == 1L
}

is_left_panel <- function(pgrid = NULL)
{
	if ( is.null(pgrid) )
		pgrid <- getOption("matter.vizi.panelgrid")
	panel_get(pgrid, arr.ind=TRUE)[2L] == 1L
}

is_bottom_panel <- function(n, pgrid = NULL)
{
	if ( is.null(pgrid) )
		pgrid <- getOption("matter.vizi.panelgrid")
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
	panel_get(pgrid) == length(pgrid$mat)
}

