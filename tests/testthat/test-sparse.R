library(testthat)

test_that("check validity method exists", {
  expect_false({
    validity_method <- getValidity(getClassDef("sparse_numeric"))
    is.null(validity_method)
  })
})

test_that("check validity method", {
  expect_true({
    x <- new("sparse_numeric",
             value = c(1, 2, 3, 1),
             pos = c(1L, 2L, 3L, 5L),
             length = 5L)
    validObject(x)
  })
})

test_that("check validity method 2", {
  expect_error({
    x <- new("sparse_numeric",
             value = c(1, 2, 3, 1),
             pos = c(1L, 2L, 3L, 5L),
             length = 5L)
    x@length <- 2L
    validObject(x)
  })
})

test_that("check coercion return class", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
  }, "sparse_numeric")
})

test_that("check for show method", {
  expect_no_error({
    getMethod("show", "sparse_numeric")
  })
})

test_that("check for plot method", {
  expect_no_error({
    getMethod("plot", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for + method", {
  expect_no_error({
    getMethod("+", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for - method", {
  expect_no_error({
    getMethod("-", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("check for * method", {
  expect_no_error({
    getMethod("*", c("sparse_numeric", "sparse_numeric"))
  })
})

test_that("sparse add generic", expect_true(isGeneric("sparse_add")))
test_that("sparse mult generic", expect_true(isGeneric("sparse_mult")))
test_that("sparse sub generic", expect_true(isGeneric("sparse_sub")))
test_that("sparse crossprod generic", expect_true(isGeneric("sparse_crossprod")))

test_that("sparse add formals", {
  expect_true(length(formals(sparse_add)) >= 2L)
})

test_that("sparse mult formals", {
  expect_true(length(formals(sparse_mult)) >= 2L)
})

test_that("sparse sub formals", {
  expect_true(length(formals(sparse_sub)) >= 2L)
})

test_that("sparse crossprod formals", {
  expect_true(length(formals(sparse_crossprod)) >= 2L)
})

test_that("check returned class for add", {
  expect_s4_class({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, "sparse_numeric")
})

test_that("sparse_add", {
  result <- as(c(1, 1, 0, 1, 6), "sparse_numeric")
  expect_equal({
    x <- as(c(0, 0, 0, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 0, 0, 4), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("sparse add dense", {
  result <- as(c(2, 4, 6, 10, 12), "sparse_numeric")
  expect_equal({
    x <- as(c(1, 3, 4, 1, 2), "sparse_numeric")
    y <- as(c(1, 1, 2, 9, 10), "sparse_numeric")
    sparse_add(x, y)
  }, result)
})

test_that("all zero wrong length", {
  expect_error({
    x <- as(rep(0, 10), "sparse_numeric")
    y <- as(rep(0, 9), "sparse_numeric")
    sparse_add(x, y)
  })
})

# validity tests

test_that("invalid pos triggers validity error", {
  expect_error({
    new("sparse_numeric",
        value = 1,
        pos = 0L,
        length = 5L)
  })
})

test_that("length must be positive", {
  expect_error({
    new("sparse_numeric",
        value = 1,
        pos = 1L,
        length = 0L)
  })
})

test_that("value/pos length mismatch is allowed but reconstructed correctly", {
  x <- new("sparse_numeric",
           value = c(1, 2),
           pos = c(10L, 20L),
           length = 30L)
  expect_s4_class(x, "sparse_numeric")
})

# coercion tests

test_that("coercion sparse -> numeric produces correct dense vector", {
  x <- new("sparse_numeric", value = c(2, 5), pos = c(2L, 5L), length = 6L)
  dense <- as(x, "numeric")
  expect_equal(dense, c(0, 2, 0, 0, 5, 0))
})

test_that("coercion of all-zero vector yields empty sparse slots", {
  x <- as(c(0, 0, 0, 0), "sparse_numeric")
  expect_length(x@value, 0)
  expect_length(x@pos, 0)
})


test_that("show method prints without error", {
  x <- as(c(0, 1, 0, 2), "sparse_numeric")
  expect_no_error(show(x))
})

test_that("plot method works even with zero overlap", {
  x <- as(c(1, 0, 0), "sparse_numeric")
  y <- as(c(0, 2, 0), "sparse_numeric")
  expect_no_error(plot(x, y))
})

test_that("plot method works with full overlap", {
  x <- as(c(1, 2, 3), "sparse_numeric")
  y <- as(c(3, 2, 1), "sparse_numeric")
  expect_no_error(plot(x, y))
})

test_that("operator overloading uses sparse methods", {
  x <- as(c(0, 1, 2), "sparse_numeric")
  y <- as(c(3, 0, 1), "sparse_numeric")
  expect_equal(x + y, sparse_add(x, y))
  expect_equal(x - y, sparse_sub(x, y))
  expect_equal(x * y, sparse_mult(x, y))
})

test_that("sparse_crossprod equals numeric crossprod", {
  x <- as(c(1, 2, 0, 3), "sparse_numeric")
  y <- as(c(0, 4, 1, 2), "sparse_numeric")
  expect_equal(sparse_crossprod(x, y), sum(as(x, "numeric") * as(y, "numeric")))
})


test_that("mean works on sparse_numeric", {
  x <- as(c(1, 0, 2, 0, 3), "sparse_numeric")
  expect_equal(mean(x), mean(c(1, 0, 2, 0, 3)))
})

test_that("norm method computes Euclidean norm", {
  x <- as(c(3, 4), "sparse_numeric")
  expect_equal(norm(x), 5)
})

test_that("norm of zero vector is zero", {
  x <- as(c(0, 0, 0), "sparse_numeric")
  expect_equal(norm(x), 0)
})


test_that("standardize works on a simple vector", {
  x <- as(c(1, 2, 3), "sparse_numeric")
  z <- standardize(x)
  z_dense <- as(z, "numeric")
  expect_equal(mean(z_dense), 0)
})

test_that("standardize errors on zero-variance vector", {
  x <- as(c(1, 1, 1, 1), "sparse_numeric")
  expect_error(standardize(x))
})

test_that("standardize works on vector with many zeros", {
  x <- as(c(0, 0, 10, 0, 0), "sparse_numeric")
  expect_no_error(standardize(x))
})

test_that("sparse_div handles division by zero producing Inf", {
  x <- as(c(1, 0, 4), "sparse_numeric")
  y <- as(c(0, 0, 2), "sparse_numeric")
  res <- sparse_div(x, y)
  dense <- as(res, "numeric")
  expect_true(is.infinite(dense[1]))
})

test_that("sparse_sub works correctly", {
  x <- as(c(1, 0, 5), "sparse_numeric")
  y <- as(c(2, 3, 1), "sparse_numeric")
  expect_equal(sparse_sub(x, y), as(c(-1, -3, 4), "sparse_numeric"))
})

test_that("sparse_mult handles partial overlap", {
  x <- as(c(1, 0, 4), "sparse_numeric")
  y <- as(c(0, 2, 3), "sparse_numeric")
  # 1*0 = 0; 0*2 = 0; 4*3 = 12
  expect_equal(sparse_mult(x, y), as(c(0, 0, 12), "sparse_numeric"))
})
