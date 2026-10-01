expect_mixed <- function(x, values, labels) {
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
  expect_identical(as.vector(x), x)
  expect_identical(as.vector(x, "double"), c(1, NA_real_))
  y <- x
  length(y) <- 3
  expect_identical(y, c(as.list(x), list(NULL)))
  expect_identical(is.na(x[c(1, 2, NA)]), is.na(as.list(x[c(1, 2, NA)])))
  expect_identical(lengths(x[c(1, NA)]), c(1L, 0L))
  expect_true(isTRUE(all.equal(x, x[c(2, 1)][c(2, 1)])))
})

test_that("vctrs slicing and assignment preserve element names", {
  skip_if_not_installed("vctrs")
  x <- mixed_units(c(a = 1, b = 2), c("m", "s"))
  expect_identical(names(vctrs::vec_slice(x, c(2, 1, 2))), c("b", "a", "b"))
  expect_identical(names(vctrs::vec_assign(x, 1, x[2])), c("a", "b"))
  expect_identical(names(vctrs::vec_c(x, x)), rep(c("a", "b"), 2))
})
