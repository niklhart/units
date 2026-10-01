expect_valid_mixed <- function(x) {
  expect_s3_class(x, "mixed_units")
  expect_type(x, "double")
  id <- attr(x, "unit_id")
  dictionary <- attr(x, "unit_dictionary")
  expect_type(id, "integer")
  expect_length(id, length(x))
  expect_type(dictionary, "list")
  expect_true(all(vapply(dictionary, inherits, TRUE, "symbolic_units")))
  expect_true(all(is.na(id) | (id >= 1L & id <= length(dictionary))))
  expect_true(all(is.na(units:::.mixed_values(x)[is.na(id)])))
  expect_identical(anyDuplicated(units:::.mixed_keys(dictionary)), 0L)
}

expect_mixed <- function(x, values, labels) {
  expect_valid_mixed(x)
  expect_s3_class(x, "mixed_units")
  expect_type(x, "double")
  expect_equal(units:::.mixed_values(x), as.double(values))
  if (length(x)) expect_identical(unname(as.character(units(x))), labels)
  else expect_length(units(x), 0)
  expect_length(attr(x, "unit_id"), length(x))
}

test_that("dictionary constructors attach scaled labels and merge aliases", {
  x <- mixed_units(c(a = 1, b = 2, c = NA, d = 4), c("meter", "m", "s", "100 m"))
  reference <- mapply(set_units, c(1, 2, NA, 4), c("meter", "m", "s", "100 m"),
    mode = "standard", SIMPLIFY = FALSE)
  expect_equal(unname(as.list(x)), reference)
  expect_length(attr(x, "unit_dictionary"), 3)
  expect_identical(names(x), letters[1:4])
  expect_error(mixed_units(NA_real_, NA_character_))
  expect_mixed(mixed_units(numeric(), character()), numeric(), character())
})

test_that("dictionary slices, repetition and replacement remap codes", {
  x <- mixed_units(c(a = 1, b = NA, c = 3), c("m", "s", "kg"))
  expect_mixed(x[c(3, 1, NA)], c(3, 1, NA), c("kg", "m", "NULL"))
  expect_null(x[c(3, 1, NA)][[3]])
  expect_identical(x[[2]], set_units(NA_real_, s))
  expect_length(attr(x[FALSE], "unit_dictionary"), 0)
  expect_length(attr(x[2], "unit_dictionary"), 1)
  expect_mixed(rep(x, each = 2), c(1, 1, NA, NA, 3, 3), rep(c("m", "s", "kg"), each = 2))
  y <- mixed_units(c(4, 5), c("kg", "m"))
  expect_mixed(c(x, y), c(1, NA, 3, 4, 5), c("m", "s", "kg", "kg", "m"))
  x[c(1, 2)] <- y
  expect_mixed(x, c(4, 5, 3), c("kg", "m", "kg"))
  x[5] <- y[2]
  expect_mixed(x, c(4, 5, 3, NA, 5), c("kg", "m", "kg", "NULL", "m"))
  x[[1]] <- set_units(2, s)
  expect_identical(x[[1]], set_units(2, s))
  x[1] <- NULL
  expect_length(x, 4)
})

test_that("conversion groups both source and target and delegates actual values", {
  value <- c(a = 0, b = 100, c = NA, d = 32, e = 212)
  from <- c("degC", "degC", "degC", "degF", "degF")
  to <- c("K", "degF", "K", "degC", "K")
  x <- mixed_units(value, from)
  reference <- mapply(function(v, f, t)
    set_units(set_units(v, f, mode = "standard"), t, mode = "standard"),
    unname(value), from, to, SIMPLIFY = FALSE)
  expect_equal(as.list(set_units(x, to)), reference)
  expect_null(names(set_units(x, to))) # baseline conversion drops names
  expect_equal(as.list(set_units(x, "K")), unname(lapply(as.list(x), set_units, "K", mode = "standard")))
  expect_identical(set_units(mixed_units(1:2, "m"), "meter"), mixed_units(1:2, "m"))
  expect_error(set_units(x, "kg"))
  expect_error(set_units(x, c("K", "degC")))
  expect_error(set_units(x, as_units("K")))
  expect_error(set_units(x[FALSE], character()))
  expect_error(set_units(x[c(1, NA)], "K"))
  # Instrument the public homogeneous method: conversions must call it once
  # per observed source/target pair, with all the values in that group.
  calls <- list()
  original <- getS3method("set_units", "units")
  testthat::local_mocked_bindings(set_units.units = function(x, value, ...) {
    calls[[length(calls) + 1L]] <<- x
    original(x, value, ...)
  }, .package = "units")
  set_units(x, to)
  expect_length(calls, 4)
  expect_true(all(vapply(calls, inherits, TRUE, "units")))
  expect_equal(sort(lengths(calls)), c(1L, 1L, 1L, 2L))
})

test_that("grouped arithmetic matches scalar units and baseline operators", {
  x <- mixed_units(c(a = 1, b = 2, c = NA, d = 4), c("m", "s", "m", "s"))
  y <- mixed_units(c(100, 2), c("cm", "s"))
  for (op in c("+", "-", "*", "/", "==", "!=")) {
    reference <- mapply(op, as.list(x), as.list(y), SIMPLIFY = op %in% c("==", "!="))
    got <- do.call(op, list(x, y))
    expect_equal(if (inherits(got, "mixed_units")) as.list(got) else got, reference)
  }
  expect_mixed(x * 2, c(2, 4, NA, 8), c("m", "s", "m", "s"))
  expect_error(x + 1)
  expect_error(-x)
  expect_error(x < y)
  expect_error(expect_warning(x * set_units(1, m)))
  expect_warning(x[1:3] * y, "multiple")
  expect_identical(duplicated(mixed_units(c(1, 1, 1), c("m", "s", "m"))), c(FALSE, FALSE, TRUE))
  expect_null(names(unique(x)))
})

test_that("vctrs preserves dictionary meaning in casts and missing buffers", {
  skip_if_not_installed("vctrs")
  source <- mixed_units(c(1, NA, 3), c("s", "mg", "L"))
  target <- mixed_units(c(0, 0, 0), c("L", "s", "mg"))
  blank <- vctrs::vec_init(target, 4)
  expect_identical(attr(blank, "unit_dictionary"), attr(target, "unit_dictionary"))
  filled <- vctrs::vec_assign(blank, c(3, 1, 4), source)
  expect_mixed(filled, c(NA, NA, 1, 3), c("mg", "NULL", "s", "L"))
  expect_mixed(vctrs::vec_slice(filled, c(4, 3)), c(3, 1), c("L", "s"))
  expect_length(attr(vctrs::vec_slice(source, integer()), "unit_dictionary"), 0)
  expect_length(attr(vctrs::vec_ptype(source), "unit_dictionary"), 3)
  expect_error(vctrs::vec_cast(source, source[FALSE]), "does not contain")
  ptype <- vctrs::vec_ptype_common(source, target)
  expect_mixed(vctrs::vec_c(source, target, .ptype = ptype), c(1, NA, 3, 0, 0, 0),
    c("s", "mg", "L", "L", "s", "mg"))
  a <- mixed_units(c(1, 2), c("mg", "L"))
  b <- mixed_units(c(2, 1), c("L", "mg"))
  expect_identical(vctrs::vec_equal(a, b[2:1]), c(TRUE, TRUE))
  expect_identical(vctrs::vec_duplicate_detect(vctrs::vec_c(a, b)), rep(TRUE, 4))
})

test_that("sparse pivots preserve known and unknown missing units", {
  skip_if_not_installed("tidyr")
  d <- data.frame(id = c(1, 1, 2), measurement = c("mass", "volume", "mass"))
  d$value <- mixed_units(c(1, NA, 3), c("mg", "L", "mg"))
  wide <- tidyr::pivot_wider(d, names_from = measurement, values_from = value)
  expect_mixed(wide$mass, c(1, 3), c("mg", "mg"))
  expect_mixed(wide$volume, c(NA, NA), c("L", "NULL"))
  wide$absent <- vctrs::vec_init(d$value, 2)
  roundtrip <- tidyr::pivot_wider(tidyr::pivot_longer(wide, c(mass, volume, absent)),
    names_from = name, values_from = value)
  expect_mixed(roundtrip$absent, c(NA, NA), c("NULL", "NULL"))
  expect_identical(attr(roundtrip$absent, "unit_dictionary"), attr(d$value, "unit_dictionary"))
})

test_that("numeric storage does not silently enable dimension-blind generics", {
  x <- mixed_units(c(1, NA), c("m", "s"))
  expect_error(sum(x))
  expect_error(sqrt(x))
  expect_warning(expect_true(is.na(mean(x))))
  expect_identical(as.vector(x), c(1, NA_real_))
  expect_identical(as.vector(x, "double"), c(1, NA_real_))
  expect_identical(is.na(x[c(1, 2, NA)]), c(FALSE, TRUE, TRUE))
  expect_identical(lengths(x[c(1, NA)]), c(1L, 1L))
  expect_true(isTRUE(all.equal(x, x[c(2, 1)][c(2, 1)])))
})

test_that("vctrs slicing and assignment preserve element names", {
  skip_if_not_installed("vctrs")
  x <- mixed_units(c(a = 1, b = 2), c("m", "s"))
  expect_identical(names(vctrs::vec_slice(x, c(2, 1, 2))), c("b", "a", "b"))
  expect_identical(names(vctrs::vec_assign(x, 1, x[2])), c("a", "b"))
  expect_identical(names(vctrs::vec_c(x, x)), rep(c("a", "b"), 2))
})

test_that("bare scalar [[ replacement creates dimensionless units", {
  original <- mixed_units(c(a = 1, b = 2), c("m", "s"))
  for (value in list(3, 3L, NA_real_, NA, NaN, Inf)) {
    x <- original
    expected <- original
    x[[1]] <- value
    expected[[1]] <- set_units(value, 1)
    expect_identical(x, expected)
    expect_identical(x[[1]], set_units(value, 1))
    expect_type(x, "double")
    expect_identical(names(x), c("a", "b"))
    expect_identical(x[[2]], original[[2]])
  }
  x <- original
  x[["a"]] <- 3
  expect_identical(x[["a"]], set_units(3, 1))
  x[[4]] <- 5
  expect_null(x[[3]])
  expect_identical(x[[4]], set_units(5, 1))
  x[[1]] <- set_units(6, kg)
  expect_identical(x[[1]], set_units(6, kg))
  x[[1]] <- NULL
  expect_length(x, 3)
  expect_identical(x[[1]], original[[2]])
})

test_that("[[ replacement rejects invalid elements without changing the object", {
  original <- mixed_units(c(a = 1, b = 2), c("m", "s"))
  for (value in list("text", NA_character_, TRUE, factor("m"), list(3),
                    numeric(), 1:2, set_units(1:2, m), 1 + 2i, matrix(3))) {
    x <- original
    expect_error(x[[1]] <- value, "replacement must be")
    expect_identical(x, original)
  }
  x <- original
  expect_error(x[["a"]] <- "text", "replacement must be")
  expect_identical(x, original)
})

test_that("all construction paths require valid numeric dictionary storage", {
  u <- units(set_units(1, m))
  new <- units:::.new_mixed
  for (args in list(list(1, 1, list(u)), list(1:2, 1L, list(u)),
                   list(1, 0L, list(u)), list(1, 2L, list(u)),
                   list(1, NA_integer_, list(u)), list(1, 1L, list("m")),
                   list(1, 1L, list(u, u)), list(list(1), 1L, list(u))))
    expect_error(do.call(new, args), "invalid mixed_units representation")
  expect_error(units:::.as.mixed_units(list("text")), "units scalars")
  expect_error(units:::.as.mixed_units(list(numeric())), "units scalars")
  expect_valid_mixed(units:::.as.mixed_units(list(set_units(c(inner = 1), m), NULL)))
  expect_valid_mixed(set_units(2:3, m) ^ c(1, 2))
  expect_valid_mixed(mixed_units(set_units(c(a = 1, b = NA), m)))
  expect_valid_mixed(mixed_units(numeric(), character()))
  expect_warning(expect_valid_mixed(mixed_units(1:3, c("m", "s"))), "multiple")
})

test_that("replacement and concatenation never switch to lists", {
  original <- mixed_units(c(a = 1, b = 2), c("m", "s"))
  for (value in list("text", list(3), factor("m"), TRUE)) {
    x <- original
    expect_error(x[1] <- value)
    expect_identical(x, original)
    expect_error(x$a <- value)
    expect_identical(x, original)
    expect_error(c(x, value))
  }
  x <- original
  x[[1]] <- set_units(c(inner = 3), kg)
  expect_valid_mixed(x)
  expect_identical(names(x), names(original))
  expect_identical(x[[1]], set_units(3, kg))
  x[2] <- 4
  expect_mixed(x, c(3, 4), c("kg", "1"))
  x[["a"]] <- 5
  expect_mixed(x, c(5, 4), c("1", "1"))
  expect_mixed(c(original, 3, set_units(4, kg), NULL), 1:4, c("m", "s", "1", "kg"))
  x[[4]] <- original[2]
  expect_mixed(x, c(5, 4, NA, 2), c("1", "1", "NULL", "s"))
  x[c("a", "b")] <- NULL
  expect_mixed(x, c(NA, 2), c("NULL", "s"))
  x <- x[integer()]
  expect_mixed(x, numeric(), character())
  expect_length(attr(x, "unit_dictionary"), 0)
  corrupt <- structure(list(set_units(1, m)), class = "mixed_units")
  expect_error(c(original, corrupt), "invalid mixed_units representation")
})

test_that("unknown-unit arithmetic stays numeric and distinguishes known-unit NA", {
  x <- mixed_units(c(a = 1, b = NA), c("m", "s"))[c(1, 2, NA)]
  for (op in c("*", "/", "+", "-")) {
    y <- do.call(op, list(x, x))
    expect_valid_mixed(y)
    expect_true(is.na(attr(y, "unit_id")[3]))
    expect_false(is.na(attr(y, "unit_id")[2]))
    expect_true(is.na(units:::.mixed_values(y)[3]))
  }
  expect_mixed(x * 2, c(2, NA, NA), c("m", "s", "NULL"))
  expect_identical(unname(x == x), c(TRUE, NA, NA))
  expect_valid_mixed(x[3] * x)
  expect_valid_mixed(x[FALSE] * x)
  expect_valid_mixed(c(x, x[3]))
  y <- x
  y[1] <- x[3]
  expect_valid_mixed(y)
  expect_null(y[[1]])
  expect_error(x + 1, "both operands")
})

test_that("core operations do not materialize scalar units lists", {
  x <- mixed_units(c(a = 1, b = 2, c = 1), c("m", "s", "m"))
  y <- mixed_units(c(3, 4), c("kg", "m"))
  testthat::local_mocked_bindings(as.list.mixed_units = function(...) {
    stop("unexpected scalar-list materialization")
  }, .package = "units")
  expect_valid_mixed(x[c(3, 2, NA)])
  expect_identical(x[[1]], set_units(1, m))
  expect_identical(x[["a"]], set_units(1, m))
  x[[1]] <- 2
  x[["b"]] <- set_units(3, kg)
  x[2:3] <- y
  expect_valid_mixed(x)
  expect_valid_mixed(c(x, y))
  expect_valid_mixed(x * 2)
  expect_valid_mixed(x[c(1, NA)] * 2)
  expect_equal(unname(drop_units(x)), c(2, 3, 4))
  expect_equal(as.numeric(x), c(2, 3, 4))
  expect_identical(unname(is.na(x)), rep(FALSE, 3))
  expect_identical(unname(lengths(x)), rep(1L, 3))
  expect_identical(as.character(x), c("2", "3", "4"))
  expect_identical(duplicated(c(x, x)), rep(c(FALSE, TRUE), each = 3))
  expect_identical(anyDuplicated(c(x, x)), 4L)
  expect_valid_mixed(unique(c(x, x)))
  homogeneous <- mixed_units(c(1, 200), c("m", "cm"))
  expect_equal(as_units(homogeneous), set_units(c(1, 2), m))
  expect_true(isTRUE(all.equal(homogeneous, mixed_units(c(100, 2), c("cm", "m")))))
  expect_valid_mixed(set_units(homogeneous, "cm"))
  expect_error(sqrt(x), "not supported")
  expect_error(sum(x), "not supported")
})

test_that("duplicate detection respects unit identity and unknown IDs", {
  x <- mixed_units(c(1, 100, NA, NaN, 1, NA, NaN), c("m", "cm", "m", "m", "m", "m", "m"))
  expect_identical(duplicated(x), c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
  expect_identical(duplicated(x, fromLast = TRUE), c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(anyDuplicated(x, fromLast = TRUE), 4L)
  x <- x[c(1, 2, NA, NA)]
  expect_identical(duplicated(x), c(FALSE, FALSE, FALSE, TRUE))
  expect_identical(duplicated(c(x[1], x[1]), incomparables = list(set_units(1, m))), c(FALSE, FALSE))
})

test_that("scalar ordering and missing median results retain valid storage", {
  x <- mixed_units(c(NA, 2), "m")
  expect_valid_mixed(median(x))
  expect_null(median(x)[[1]])
  expect_mixed(median(x, na.rm = TRUE), 2, "m")
  expect_identical(xtfrm(x), c(NA_real_, 1))
  expect_error(median(mixed_units(1:2, "m")), "ordering")
  expect_identical(duplicated(x, incomparables = NULL), duplicated(x))
  unknown <- x[NA_integer_]
  expect_identical(duplicated(c(unknown, unknown), incomparables = list(set_units(NA_real_, s))),
    c(FALSE, TRUE))
  expect_error(as_units(unknown), "unknown units")
})


test_that("removed convenience methods use ordinary numeric defaults", {
  x <- mixed_units(c(a = 1, b = NA_real_), c("m", "s"))[c(1, 2, NA)]
  values <- units:::.mixed_values(x)
  names(values) <- names(x)
  expect_identical(is.na(x), is.na(values))
  expect_identical(lengths(x), lengths(values))
  expect_identical(as.vector(x), as.vector(values))
  for (n in c(0L, 2L, 4L)) {
    result <- x
    length(result) <- n
    expected <- values
    length(expected) <- n
    expect_identical(result, expected)
  }
  expect_error(x$a, "atomic vectors")
  original <- x
  for (value in list(3, set_units(3, m), NULL)) {
    expect_error(x$a <- value, "not supported")
    expect_identical(x, original)
  }
  for (generic in c("$", "median", "lengths", "as.vector", "is.na", "length<-"))
    expect_null(getS3method(generic, "mixed_units", optional = TRUE))
})
