require(testthat)
require(matter)

context("atoms-class")

test_that("atoms combining/subsetting", {

	x <- atoms2(type="integer", extent=10)
	y <- atoms2(type="double", extent=10)

	z1 <- cbind(x, y)
	expect_equal(c(10,2), dim(z1))
	z1 <- rbind(z1, z1)
	expect_equal(c(20,2), dim(z1))
	z2 <- rbind(x, y)
	expect_equal(c(20,1), dim(z2))
	z2 <- cbind(z2, z2)
	expect_equal(c(20,2), dim(z2))

	expect_equal(cbind(x, y), z1[c(1,3)])
	expect_equal(rbind(x, x), z1[c(1,2)])
	expect_equal(rbind(x, y), z2[c(1,2)])
	expect_equal(cbind(x, x), z2[c(1,3)])
	expect_equal(z1[c(1,3)], z1[1:10,])
	expect_equal(z2[c(1,3)], z2[1:10,])
	expect_equal(c(15,2), dim(z2[1:15,]))
	expect_equal(c(15,2), dim(z1[1:15,]))
	expect_equal(c(10,2), dim(z1[6:15,]))
	expect_equal(c(10,2), dim(z2[6:15,]))
	expect_equal(length(z1[c(1,3,5,11:15),]), 8L)
	expect_equal(length(z2[c(1,3,5,11:15),]), 8L)
	expect_equal(x[1:5,], z1[1:5,1])
	expect_equal(y[1:5,], z1[1:5,2])

	z <- cbind(rbind(x, y), x, y, rbind(x, y))
	expect_equal(dim(z), c(NA, 4))
	expect_equal(dim(z[1:5,]), c(5, 4))
	expect_equal(dim(z[1:5,1:2]), c(5, 2))
	expect_equal(as.vector(dims(z)), c(20,10,10,20))

})

test_that("atoms read/write - raw", {

	path <- tempfile()
	file.create(path)
	x <- atoms2(path, "raw", extent=10, readonly=FALSE)
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

test_that("atoms read/write - integer", {

	path <- tempfile()
	file.create(path)
	x <- atoms2(path, "integer", extent=10, readonly=FALSE)
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

test_that("atoms read/write - double", {

	path <- tempfile()
	file.create(path)
	y <- atoms2(path, "double", extent=10, readonly=FALSE)
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

