
# create matter-backed ALTREP objects

make_matter_altrep <- function(x) {
	.Call("C_makeMatterAltrep", x)
}

