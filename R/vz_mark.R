
#### Define visual mark classes for 'vizi' ####
## --------------------------------------------

setClass("vz_mark", contains = "vz_")

add_mark <- function(plot, mark, encoding = NULL,
	data = NULL, transform = NULL, params = NULL)
{
	mark <- new(paste0("vz_mark_", mark), encoding=encoding,
		data=data, transform=transform, params=params)
	plot@layers <- c(plot@layers, list(mark))
	plot
}

# points

setClass("vz_mark_point", contains = "vz_mark")

setMethod("points", "vz_graphic",
	function(x, encoding = NULL, data = NULL, transform = NULL,
		stroke = "solid", ...)
{
	params <- list(stroke=stroke, ...)
	add_mark(x, "point", encoding=encoding, data=data,
		transform=transform, params=params)
})

# lines

setClass("vz_mark_line", contains = "vz_mark")

setMethod("lines", "vz_graphic",
	function(x, encoding = NULL, data = NULL, transform = NULL,
		lineend = "round", linejoin = "round", linemitre = 10,
		ordered = TRUE, ...)
{
	params <- list(lineend=lineend, linejoin=linejoin,
		linemitre=linemitre, ordered=ordered, ...)
	add_mark(x, "line", encoding=encoding, data=data,
		transform=transform, params=params)
})
