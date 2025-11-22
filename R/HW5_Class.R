#' Sparse Numeric Vector Class
#'
#' An S4 class for representing sparse numeric vectors.
#'
#' @slot value Numeric vector of non-zero values.
#' @slot pos Integer vector giving positions of the non-zero entries.
#' @slot length Integer giving total vector length.
#'
#' @export
setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

#' Validity Check for sparse_numeric
#'
#' Ensures that positions are within the allowed range and that `length` is valid.
#'
#' @param object A \code{sparse_numeric} object
#' @name sparse_validity
setValidity("sparse_numeric", function(object) {
  if (any(object@pos < 1 | object@pos > object@length)) {
    return("'pos' must be between 1 and 'length'")
  }

  if (length(object@length) != 1 || object@length < 1) {
    return("'length' must be a single positive integer")
  }

  TRUE
})

#' Add Two Sparse Vectors
#'
#' @param x,y Objects of class \code{sparse_numeric}
#' @param ... Additional arguments
#'
#' @export
setGeneric("sparse_add", function(x, y, ...)
  standardGeneric("sparse_add"))

#' Subtract Two Sparse Vectors
#'
#' @param x,y Objects of class \code{sparse_numeric}
#' @param ... Additional arguments
#'
#' @export
setGeneric("sparse_sub", function(x, y, ...)
  standardGeneric("sparse_sub"))

#' Elementwise multiply two vectors
#'
#' @param x,y Objects of class \code{sparse_numeric}
#' @param ... Additional arguments
#'
#' @export
setGeneric("sparse_mult", function(x, y, ...)
  standardGeneric("sparse_mult"))

#' Elementwise divide two vectors
#'
#' @param x,y Objects of class \code{sparse_numeric}
#' @param ... Additional arguments
#'
#' @export
setGeneric("sparse_div", function(x, y, ...)
  standardGeneric("sparse_div"))

#' Sparse Cross Product
#'
#' Computes the inner product between two sparse vectors.
#'
#' @param x,y Objects of class \code{sparse_numeric}
#' @param ... Additional arguments
#'
#' @export
setGeneric("sparse_crossprod", function(x, y, ...)
  standardGeneric("sparse_crossprod"))

#' @describeIn sparse_add Add two sparse vectors
#' @export
setMethod("sparse_add", signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length)
              stop("Vectors must have the same length")

            all_pos <- sort(unique(c(x@pos, y@pos)))
            new_value <- sapply(all_pos, function(p) {
              val_x <- ifelse(p %in% x@pos, x@value[x@pos == p], 0)
              val_y <- ifelse(p %in% y@pos, y@value[y@pos == p], 0)
              val_x + val_y
            })

            nonzero_idx <- which(new_value != 0)

            new(
              "sparse_numeric",
              value = new_value[nonzero_idx],
              pos = all_pos[nonzero_idx],
              length = x@length
            )
          })

#' @describeIn sparse_sub Subtract two sparse vectors
#' @export
setMethod("sparse_sub", signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length)
              stop("Vectors must have the same length")

            all_pos <- sort(unique(c(x@pos, y@pos)))
            new_value <- sapply(all_pos, function(p) {
              val_x <- ifelse(p %in% x@pos, x@value[x@pos == p], 0)
              val_y <- ifelse(p %in% y@pos, y@value[y@pos == p], 0)
              val_x - val_y
            })

            nonzero_idx <- which(new_value != 0)

            new(
              "sparse_numeric",
              value = new_value[nonzero_idx],
              pos = all_pos[nonzero_idx],
              length = x@length
            )
          })

#' @describeIn sparse_mult Elementwise multiply
#' @export
setMethod("sparse_mult", signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length)
              stop("Vectors must have the same length")

            all_pos <- sort(unique(c(x@pos, y@pos)))
            new_value <- sapply(all_pos, function(p) {
              val_x <- ifelse(p %in% x@pos, x@value[x@pos == p], 0)
              val_y <- ifelse(p %in% y@pos, y@value[y@pos == p], 0)
              val_x * val_y
            })

            nonzero_idx <- which(new_value != 0)

            new(
              "sparse_numeric",
              value = new_value[nonzero_idx],
              pos = all_pos[nonzero_idx],
              length = x@length
            )
          })

#' @describeIn sparse_div Elementwise division
#' @export
setMethod("sparse_div", signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length)
              stop("Vectors must have the same length")

            all_pos <- sort(unique(c(x@pos, y@pos)))
            new_value <- sapply(all_pos, function(p) {
              val_x <- ifelse(p %in% x@pos, x@value[x@pos == p], 0)
              val_y <- ifelse(p %in% y@pos, y@value[y@pos == p], 0)
              val_x / val_y
            })

            nonzero_idx <- which(new_value != 0)

            new(
              "sparse_numeric",
              value = new_value,
              pos = nonzero_idx,
              length = x@length
            )
          })

#' @describeIn sparse_crossprod Sparse dot product
#' @export
setMethod("sparse_crossprod", signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            if (x@length != y@length)
              stop("Vectors must have the same length")

            all_pos <- sort(unique(c(x@pos, y@pos)))
            new_value <- sapply(all_pos, function(p) {
              val_x <- ifelse(p %in% x@pos, x@value[x@pos == p], 0)
              val_y <- ifelse(p %in% y@pos, y@value[y@pos == p], 0)
              val_x * val_y
            })

            sum(new_value)
          })

#' Add two sparse_numeric objects
#'
#' Overloads the `+` operator for `sparse_numeric` objects.
#'
#' @param e1 A `sparse_numeric` object
#' @param e2 A `sparse_numeric` object
#'
#' @export
setMethod("+", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_add(e1, e2))

#' Subtract two sparse_numeric objects
#'
#' Overloads the `-` operator for `sparse_numeric` objects.
#'
#' @param e1 A `sparse_numeric` object
#' @param e2 A `sparse_numeric` object
#'
#' @export
setMethod("-", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_sub(e1, e2))

#' Multiply two sparse_numeric objects
#'
#' Overloads the `*` operator for `sparse_numeric` objects.
#'
#' @param e1 A `sparse_numeric` object
#' @param e2 A `sparse_numeric` object
#'
#' @export
setMethod("*", signature(e1 = "sparse_numeric", e2 = "sparse_numeric"),
          function(e1, e2) sparse_mult(e1, e2))

setAs("numeric", "sparse_numeric", function(from) {
  nonzero_idx <- which(from != 0)
  new(
    "sparse_numeric",
    value = from[nonzero_idx],
    pos = as.integer(nonzero_idx),
    length = length(from)
  )
})

setAs("sparse_numeric", "numeric", function(from) {
  out <- numeric(from@length)
  out[from@pos] <- from@value
  out
})

#' Print out sparse vector
#'
#' @param object The sparse numeric vector
#'
#' @export
setMethod("show", "sparse_numeric", function(object) {
  cat(
    "Sparse numeric vector of length",
    object@length,
    "with",
    length(object@value),
    "non-zero entries\n"
  )
  cat("Positions:", object@pos, "\n")
  cat("Values:   ", object@value, "\n")
})

#' Plot Two Sparse Numeric Vectors
#'
#' Generates an Rplot which shows points at which two vectors are non-zero, and their values.
#'
#' @param x,y sparse_numeric objects
#' @param ... extra arguments
#'
#' @importFrom methods new as show
#' @importFrom graphics points legend
#'
#' @export
setMethod("plot",
          signature(x = "sparse_numeric", y = "sparse_numeric"),
          function(x, y, ...) {
            overlapping = intersect(x@pos, y@pos)

            x_vals <- x@value[match(overlapping, x@pos)]
            y_vals <- y@value[match(overlapping, y@pos)]

            df <- data.frame(pos = overlapping,
                             x = x_vals,
                             y = y_vals)

            if (length(overlapping) == 0) {
              rng <- c(0, 1)
            } else {
              rng <- range(c(df$x, df$y))
            }

            plot(
              df$pos,
              df$x,
              col = "blue",
              pch = 16,
              xlab = "Indices",
              ylab = "Value",
              ylim = rng,
              xlim = c(1, x@length),
              main = sprintf("Overlapping Indices: (Count %d)", length(overlapping))
            )
            points(df$pos, df$y, col = "red", pch = 16)
            legend("topright", legend = c("x", "y"), col = c("blue", "red"), pch = 16)
          })

#' Sparse Vector Mean
#'
#' Computes the mean of a sparse numeric vector
#'
#' @param x A sparse_numeric object
#'
#' @export
setMethod("mean", signature(x="sparse_numeric"), function(x) {
  sum(x@value) / x@length
})

#' Sparse vector norm
#'
#' Computes the Euclidean norm of a sparse numeric vector
#'
#' @param x A sparse_numeric object
#' @param ... Additional arguments
#'
#' @export
setGeneric("norm", function(x, ...)
  standardGeneric("norm"))

#' Standardize a Sparse Numeric Vector
#'
#' Z-score standardizes a sparse numeric vector
#'
#' @param x A sparse_numeric object
#' @param ... Additional arguments
#'
#' @export
setGeneric("standardize", function(x, ...)
  standardGeneric("standardize"))

#' @describeIn norm Euclidean norm
#' @export
setMethod("norm", signature(x="sparse_numeric"), function(x, ...) {
  sqrt(sum(x@value ^ 2))
})

#' @describeIn standardize Vector standardization
#' @export
setMethod("standardize", signature(x="sparse_numeric"), function(x, ...) {
  n <- x@length
  k <- length(x@value)

  mu <- mean(x)
  nz_centered_sq <- sum((x@value - mu)^2)

  zero_count <- n - k
  zero_contrib <- zero_count * mu^2

  var <- (nz_centered_sq + zero_contrib) / n
  sd <- sqrt(var)

  if (sd == 0) {
    stop("Cannot standardize: the vector has zero variance")
  }

  dense <- as(x, "numeric")
  standardized <- (dense - mu) / sd

  as(standardized, "sparse_numeric")
})
