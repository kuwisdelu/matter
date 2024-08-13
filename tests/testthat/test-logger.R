require(testthat)
require(matter)

context("logging")

test_that("simple_logger", {

	sl <- simple_logger()
	sl$log("Hello")
	sl$log("world!")

	expect_equal(path(sl), sl$logfile)
	expect_true(grepl("Hello", sl$buffer[1L], fixed=TRUE))
	expect_true(grepl("world!", sl$buffer[2L], fixed=TRUE))
	
	expect_message(sl$message("This is a message!"))
	expect_warning(sl$warning("This is a warning!"))
	expect_error(sl$stop("This is an error!"))

	fun <- function(...) sl$stop("panic!")
	try(fun(arg), silent=TRUE)
	last_entry <- sl$buffer[length(sl$buffer)]

	expect_true(grepl("fun(arg)", last_entry, fixed=TRUE))

	buffer <- sl$buffer
	newfile <- tempfile("logger", fileext=".log")
	sl$move(newfile)

	expect_equal(readLines(sl$logfile), buffer)

	sl$log("last entry")
	sl$close()
	log <- readLines(newfile)
	
	expect_gt(length(log), length(buffer))
	expect_length(sl$logfile, 0)

})

test_that("simple_logger - finalizer", {

	sl <- simple_logger(tempfile("logger", fileext=".log"))
	sl$log("Hello")
	sl$log("world!")
	logfile <- sl$logfile

	rm(list="sl", envir=environment(NULL))
	gc(full=TRUE)

	log <- readLines(logfile)

	expect_true(grepl("Hello", log[1L], fixed=TRUE))
	expect_true(grepl("world!", log[2L], fixed=TRUE))

})
