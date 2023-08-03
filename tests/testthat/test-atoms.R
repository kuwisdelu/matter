require(testthat)
require(matter)

context("atoms-class")

test_that("atoms combining/subsetting", {

	x <- atoms(type="integer", extent=10)
	y <- atoms(type="double", extent=10)

	a <- cbind(x, y)
	expect_equal(c(10,2), dim(a))
	a <- rbind(a, a)
	expect_equal(c(20,2), dim(a))

	expect_equal(cbind(x, y), a[c(1,3)])
	expect_equal(rbind(x, x), a[c(1,2)])
	expect_equal(a[c(1,3)], a[1:10,])
	expect_equal(c(15,2), dim(a[1:15,]))
	expect_equal(c(10,2), dim(a[6:15,]))
	expect_equal(length(a[c(1,3,5,11:15),]), 8L)
	expect_equal(x[1:5,], a[1:5,1])
	expect_equal(y[1:5,], a[1:5,2])
	expect_equal(a[c(3,1)], a[1:10,c(2,1)])

	b <- cbind(x, rbind(x, y), rbind(y, x), y)
	expect_equal(dim(b), c(NA, 4))
	expect_equal(dim(b[1:5,]), c(5, 4))
	expect_equal(dim(b[1:5,1:2]), c(5, 2))
	expect_equal(as.vector(dims(b)), c(10,20,20,10))
	expect_equal(b[c(4,5,2,3)], b[,c(3,2)])

})

test_that("atoms read/write - raw", {

	path <- tempfile()
	file.create(path)
	x <- atoms(path, "raw", extent=10, readonly=FALSE)
	value <- as.raw(1:10)
	i <- c(1,3,5:9)
	write_atom(x, 1L, value)
	
	expect_equal(value, read_atom(x, 1L, "raw"))
	expect_equal(value[i], read_atoms(x, i, "raw"))
	expect_equal(value[rev(i)], read_atoms(x, rev(i), "raw"))
	
	value[i] <- as.raw(as.integer(value[i]) + 100)
	write_atoms(x, i, value[i])
	
	expect_equal(value, read_atom(x, 1L, "raw"))
	expect_equal(value[i], read_atoms(x, i, "raw"))
	expect_equal(value[rev(i)], read_atoms(x, rev(i), "raw"))

})

test_that("atoms read/write - short", {

	path <- tempfile()
	file.create(path)
	x <- atoms(path, "short", extent=10, readonly=FALSE)
	value <- 1:10
	i <- c(1,3,5:9)
	write_atom(x, 1L, value)
	
	expect_equal(value, read_atom(x, 1L, "integer"))
	expect_equal(value[i], read_atoms(x, i, "integer"))
	expect_equal(value[rev(i)], read_atoms(x, rev(i), "integer"))
	
	value[i] <- value[i] + 100
	write_atoms(x, i, value[i])
	
	expect_equal(value, read_atom(x, 1L, "integer"))
	expect_equal(value[i], read_atoms(x, i, "integer"))
	expect_equal(value[rev(i)], read_atoms(x, rev(i), "integer"))

})

test_that("atoms read/write - int", {

	path <- tempfile()
	file.create(path)
	x <- atoms(path, "integer", extent=10, readonly=FALSE)
	value <- 1:10
	i <- c(1,3,5:9)
	write_atom(x, 1L, value)
	
	expect_equal(value, read_atom(x, 1L, "integer"))
	expect_equal(value[i], read_atoms(x, i, "integer"))
	expect_equal(value[rev(i)], read_atoms(x, rev(i), "integer"))
	
	value[i] <- value[i] + 100
	write_atoms(x, i, value[i])
	
	expect_equal(value, read_atom(x, 1L, "integer"))
	expect_equal(value[i], read_atoms(x, i, "integer"))
	expect_equal(value[rev(i)], read_atoms(x, rev(i), "integer"))

})

test_that("atoms read/write - long", {

	path <- tempfile()
	file.create(path)
	x <- atoms(path, "long", extent=10, readonly=FALSE)
	value <- 1:10
	i <- c(1,3,5:9)
	write_atom(x, 1L, value)
	
	expect_equal(value, read_atom(x, 1L, "integer"))
	expect_equal(value[i], read_atoms(x, i, "integer"))
	expect_equal(value[rev(i)], read_atoms(x, rev(i), "integer"))
	
	value[i] <- value[i] + 100
	write_atoms(x, i, value[i])
	
	expect_equal(value, read_atom(x, 1L, "integer"))
	expect_equal(value[i], read_atoms(x, i, "integer"))
	expect_equal(value[rev(i)], read_atoms(x, rev(i), "integer"))

})

test_that("atoms read/write - float", {

	path <- tempfile()
	file.create(path)
	y <- atoms(path, "float", extent=10, readonly=FALSE)
	value <- (1:10 + 1:10 * 0.11)
	i <- c(1,3,5:9)
	write_atom(y, 1L, value)
	
	expect_equal(value, read_atom(y, 1L, "double"), tolerance=1e-5)
	expect_equal(value[i], read_atoms(y, i, "double"), tolerance=1e-5)
	expect_equal(value[rev(i)], read_atoms(y, rev(i), "double"), tolerance=1e-5)

	value[i] <- value[i] + 100
	write_atoms(y, i, value[i])
	
	expect_equal(value, read_atom(y, 1L, "double"), tolerance=1e-5)
	expect_equal(value[i], read_atoms(y, i, "double"), tolerance=1e-5)
	expect_equal(value[rev(i)], read_atoms(y, rev(i), "double"), tolerance=1e-5)

})

test_that("atoms read/write - double", {

	path <- tempfile()
	file.create(path)
	y <- atoms(path, "double", extent=10, readonly=FALSE)
	value <- (1:10 + 1:10 * 0.11)
	i <- c(1,3,5:9)
	write_atom(y, 1L, value)
	
	expect_equal(value, read_atom(y, 1L, "double"))
	expect_equal(value[i], read_atoms(y, i, "double"))
	expect_equal(value[rev(i)], read_atoms(y, rev(i), "double"))

	value[i] <- value[i] + 100
	write_atoms(y, i, value[i])
	
	expect_equal(value, read_atom(y, 1L, "double"))
	expect_equal(value[i], read_atoms(y, i, "double"))
	expect_equal(value[rev(i)], read_atoms(y, rev(i), "double"))

})

