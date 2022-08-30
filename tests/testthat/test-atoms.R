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

})

test_that("atoms read/write", {

	path1 <- tempfile()
	file.create(path1)
	x <- atoms2(path1, "integer", extent=10, readonly=FALSE)
	value1 <- 1:10
	i1 <- c(1,3,5:9)
	write_atom(x, 1L, value1)
	
	expect_equal(value1, read_atom(x, 1L, "integer"))
	expect_equal(value1[i1], read_atoms(x, i1, "integer"))
	expect_equal(value1[rev(i1)], read_atoms(x, rev(i1), "integer"))
	
	value1[i1] <- value1[i1] + 100
	write_atoms(x, i1, value1[i1])
	
	expect_equal(value1, read_atom(x, 1L, "integer"))
	expect_equal(value1[i1], read_atoms(x, i1, "integer"))
	expect_equal(value1[rev(i1)], read_atoms(x, rev(i1), "integer"))

	path2 <- tempfile()
	file.create(path2)
	y <- atoms2(path2, "double", extent=10, readonly=FALSE)
	value2 <- (1:10 + 1:10 * 0.11)
	i2 <- c(1,3,5:9)
	write_atom(y, 1L, value2)
	
	expect_equal(value2, read_atom(y, 1L, "double"))
	expect_equal(value2[i1], read_atoms(y, i1, "double"))
	expect_equal(value2[rev(i1)], read_atoms(y, rev(i1), "double"))

	value2[i2] <- value2[i2] + 100
	write_atoms(y, i2, value2[i2])
	
	expect_equal(value2, read_atom(y, 1L, "double"))
	expect_equal(value2[i2], read_atoms(y, i2, "double"))
	expect_equal(value2[rev(i2)], read_atoms(y, rev(i2), "double"))

})

