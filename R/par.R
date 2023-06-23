
#### Set graphical parameters ####
## -------------------------------

vizi_par <- function(..., style = getOption("matter.vizi.style"))
{
	params <- getOption("matter.vizi.par")
	args <- list(...)
	if ( length(args) > 0L ) {
		if ( length(args) == 1L ) {
			if ( is.list(args[[1L]]) && is.null(names(args)) ) {
				args <- args[[1L]]
			} else if ( is.null(args[[1L]]) ) {
				args <- par_style_new()
				params <- list()
			}
		}
		if ( !is.null(names(args)) ) {
			params <- par_update(params, more=args)
			options(matter.vizi.par=params)
			return(invisible(params))
		}
		args <- as.character(unlist(args))
	} else {
		args < names(params)
	}
	if ( !is.null(style) ) {
		p <- get(paste0("par_style_", style))
		params <- par_update(p(), more=params)
	}
	if ( length(args) > 0L )
		params <- params[args]
	if ( length(params) == 1L )
		params <- params[[1L]]
	params
}

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

vizi_panel <- function(dim = c(1, 1),
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

is_bottom_panel <- function(pgrid = NULL)
{
	if ( is.null(pgrid) )
		pgrid <- getOption("matter.vizi.panelgrid")
	panel_get(pgrid, arr.ind=TRUE)[1L] == nrow(pgrid$mat)
}

is_right_panel <- function(pgrid = NULL)
{
	if ( is.null(pgrid) )
		pgrid <- getOption("matter.vizi.panelgrid")
	panel_get(pgrid, arr.ind=TRUE)[2L] == ncol(pgrid$mat)
}

is_last_panel <- function(pgrid = NULL)
{
	if ( is.null(pgrid) )
		pgrid <- getOption("matter.vizi.panelgrid")
	panel_get(pgrid) == length(pgrid$mat)
}

