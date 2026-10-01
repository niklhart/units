# Numeric storage and dictionary helpers. Symbolic units preserve custom units and
# the package's formatting semantics without reparsing labels.
.mixed_units_key <- function(u) if (is.null(u)) "NULL" else ud_char(u)
.mixed_keys <- function(d) vapply(d, .mixed_units_key, "")
.mixed_values <- function(x) {
  attributes(x) <- NULL
  x
}
.validate_mixed_data <- function(x, id, dictionary) {
  if (!is.numeric(x) || is.complex(x) || !is.null(dim(x)) ||
      !is.integer(id) || length(id) != length(x) || !is.null(dim(id)) ||
      !is.list(dictionary) ||
      !all(vapply(dictionary, function(u) inherits(u, "symbolic_units") &&
        is.character(u$numerator) && is.character(u$denominator), TRUE)) ||
      any(!is.na(id) & (id < 1L | id > length(dictionary))) ||
      any(!is.na(.mixed_values(x)[is.na(id)])) || anyDuplicated(.mixed_keys(dictionary)))
    stop("invalid mixed_units representation", call. = FALSE)
  invisible(NULL)
}
.validate_mixed <- function(x) {
  .validate_mixed_data(x, attr(x, "unit_id"), attr(x, "unit_dictionary"))
  x
}
.new_mixed <- function(x, id, dictionary, nm = NULL, compact = TRUE) {
  .validate_mixed_data(x, id, dictionary)
  if (compact) {
    used <- sort(unique(id[!is.na(id)]))
    if (!length(id) || length(used)) {
      id <- match(id, used)
      dictionary <- dictionary[used]
    }
  }
  structure(as.double(x), unit_id = as.integer(id),
    unit_dictionary = dictionary, names = nm, class = "mixed_units")
}
.mixed_assemble <- function(x, id, dictionary, nm = NULL) {
  keys <- .mixed_keys(dictionary)
  keep <- !duplicated(keys)
  .new_mixed(x, match(keys, keys[keep])[id], dictionary[keep], nm)
}
.as.mixed_units <- function(x) {
  stopifnot(is.list(x))
  valid <- vapply(x, function(a) is.null(a) ||
    (inherits(a, "units") && is.numeric(a) && !is.complex(a) &&
     length(a) == 1L && is.null(dim(a))), TRUE)
  if (!all(valid)) stop("expected a list of units scalars or NULL", call. = FALSE)
  dictionary <- lapply(x, function(a) if (!is.null(a)) units(a))
  known <- !vapply(x, is.null, TRUE)
  keys <- .mixed_keys(dictionary)
  keep <- known & !duplicated(keys)
  .new_mixed(vapply(x, function(a) if (is.null(a)) NA_real_ else as.numeric(a), 0.),
    match(keys, keys[keep]), dictionary[keep], names(x))
}
# constructor function:
#' Create or convert to a mixed units vector
#' @param x numeric, or vector of class \code{units}
#' @param values character vector with units encodings, or list with symbolic units of class \code{mixed_symbolic_units}
#' @param value see values
#' @param ... ignored
#' @details if \code{x} is of class \code{units}, \code{values} should be missing or of class \code{mixed_symbolic_units}; if \code{x} is numeric, \code{values} should be a character vector the length of \code{x}.
#' Numeric values are stored with integer unit IDs and a dictionary of symbolic
#' units. Subsetting and repetition preserve the mixed class; scalar extraction
#' with \code{[[} returns an ordinary \code{units} object, or \code{NULL} for
#' a missing element with unknown units. Use \code{as.list()} to materialize
#' the former list representation. Direct inspection of storage and iteration
#' with \code{for} no longer expose scalar units objects.
#' Numeric replacements and concatenated numbers receive dimensionless units.
#' Replacements must be numeric, \code{units}, or \code{mixed_units} values;
#' \code{[[<-} requires a scalar. Assigning \code{NULL} deletes elements.
#' Invalid inputs raise an error rather than changing representation.
#' \code{$} and \code{$<-} are unsupported; use named indexing with \code{[[}
#' or \code{[}. The \code{is.na}, \code{lengths}, \code{as.vector},
#' \code{as.character} and \code{mean} functions use the standard numeric
#' behavior; \code{mean} averages stored values without converting units.
#' Arithmetic involving an unknown unit returns a missing value
#' with an unknown unit; a missing value with a known unit retains its unit.
#' All mixed results use numeric storage, integer IDs and a unit dictionary.
#' @examples
#' a <- 1:4
#' u <- c("m/s", "km/h", "mg/L", "g")
#' mixed_units(a, u)
#' units(a) = as_units("m/s")
#' mixed_units(a) # converts to mixed representation
#' @export
mixed_units <- function(x, values, ...) UseMethod("mixed_units")

#' @export
mixed_units.units <- function(x, values, ...) {
  stopifnot(missing(values))
  .new_mixed(.mixed_values(x), rep.int(1L, length(x)), list(units(x)), names(x))
}

#' @export
mixed_units.numeric <- function(x, values, ...) {
  stopifnot(is.character(values), is.numeric(x), !is.complex(x))
  if (!length(x) || !length(values))
    return(.new_mixed(numeric(), integer(), list(), if (!is.null(names(x))) character()))
  n <- max(length(x), length(values))
  if (n %% min(length(x), length(values)))
    warning("longer argument not a multiple of length of shorter")
  nm <- names(x)
  x <- rep_len(.mixed_values(x), n)
  labels <- unique(values)
  id <- rep_len(match(values, labels), n)
  dictionary <- vector("list", length(labels))
  for (i in split(seq_len(n), id)) {
    k <- id[i[1L]]
    u <- set_units(x[i], labels[k], mode = "standard")
    x[i] <- as.numeric(u)
    dictionary[[k]] <- units(u)
  }
  .mixed_assemble(x, id, dictionary, nm)
}

#' @export
as.list.mixed_units <- function(x, ...) {
  d <- attr(x, "unit_dictionary")
  id <- attr(x, "unit_id")
  value <- .mixed_values(x)
  out <- lapply(seq_along(x), function(i)
    if (is.na(id[i])) NULL else .as.units(value[i], d[[id[i]]]))
  names(out) <- names(x)
  out
}

#' @export
as.double.mixed_units <- function(x, ...) {
  if (anyNA(attr(x, "unit_id")))
    stop("cannot coerce elements with unknown units to numeric", call. = FALSE)
  .mixed_values(x)
}

#' @export
`[[.mixed_units` <- function(x, i, ...) {
  if (length(i) != 1L) stop("[[ requires a single index", call. = FALSE)
  if (is.na(i) || (is.character(i) && (i == "" || is.na(match(i, names(x)))))) return(NULL)
  id <- attr(x, "unit_id")
  names(id) <- names(x)
  k <- id[[i, ...]]
  if (is.na(k)) return(NULL)
  v <- .mixed_values(x)
  names(v) <- names(x)
  .as.units(v[[i, ...]], attr(x, "unit_dictionary")[[k]])
}

#' @export
`[.mixed_units` <- function(x, i, ...) {
  v <- .mixed_values(x)
  names(v) <- names(x)
  id <- attr(x, "unit_id")
  names(id) <- names(x)
  out <- v[i]
  .new_mixed(out, id[i], attr(x, "unit_dictionary"), names(out))
}

# Normalize unit-bearing operands and bare numbers at the public boundary.
# All internal operations then use the same dictionary representation.
.mixed_input <- function(x) {
  if (inherits(x, "mixed_units")) return(.validate_mixed(x))
  if (inherits(x, "units")) return(mixed_units(x))
  if (!is.object(x) && is.null(dim(x)) && !is.complex(x) &&
      (is.numeric(x) || (is.logical(x) && all(is.na(x)))))
    return(.mixed_set_names(mixed_units(as.numeric(x), "1"), names(x)))
  stop("expected numeric values, units, or mixed_units", call. = FALSE)
}
.mixed_set_names <- function(x, nm) { names(x) <- nm; x }

#' @export
`[<-.mixed_units` <- function(x, i, ..., value) {
  if (is.null(value)) {
    positions <- seq_along(x)
    names(positions) <- names(x)
    return(x[!seq_along(x) %in% positions[i]])
  }
  value <- .mixed_input(value)
  d <- c(attr(x, "unit_dictionary"), attr(value, "unit_dictionary"))
  v <- .mixed_values(x)
  names(v) <- names(x)
  id <- attr(x, "unit_id")
  names(id) <- names(x)
  v[i] <- .mixed_values(value)
  id[i] <- attr(value, "unit_id") + length(attr(x, "unit_dictionary"))
  .mixed_assemble(v, id, d, names(v))
}

#' @export
`[[<-.mixed_units` <- function(x, i, ..., value) {
  if (length(i) != 1L || is.na(i)) stop("[[ requires a single nonmissing index", call. = FALSE)
  if (is.null(value)) {
    if (is.character(i) && !i %in% names(x)) return(x)
    positions <- seq_along(x)
    names(positions) <- names(x)
    return(x[-positions[[i, ...]]])
  }
  if (length(value) != 1L || !is.null(dim(value)))
    stop("replacement must be a numeric scalar, a units scalar, or NULL", call. = FALSE)
  value <- tryCatch(.mixed_input(value), error = function(e)
    stop("replacement must be a numeric scalar, a units scalar, or NULL", call. = FALSE))
  v <- .mixed_values(x)
  names(v) <- names(x)
  id <- attr(x, "unit_id")
  names(id) <- names(x)
  v[[i, ...]] <- .mixed_values(value)
  id[[i, ...]] <- attr(value, "unit_id") + length(attr(x, "unit_dictionary"))
  .mixed_assemble(v, id, c(attr(x, "unit_dictionary"),
    attr(value, "unit_dictionary")), names(v))
}

#' @export
rep.mixed_units <- function(x, ...) x[rep(seq_along(x), ...)]

#' @export
c.mixed_units <- function(...) {
  args <- Filter(Negate(is.null), list(...))
  args <- lapply(args, .mixed_input)
  offsets <- cumsum(c(0L, vapply(args, function(x) length(attr(x, "unit_dictionary")), 0L)))
  ids <- lapply(seq_along(args), function(i) attr(args[[i]], "unit_id") + offsets[i])
  vals <- lapply(args, function(x) { v <- .mixed_values(x); names(v) <- names(x); v })
  v <- do.call(c, vals)
  .mixed_assemble(v, unlist(ids, use.names = FALSE),
    unlist(lapply(args, attr, "unit_dictionary"), recursive = FALSE, use.names = FALSE), names(v))
}

#' @export
set_units.mixed_units <- function(x, value, ..., mode = "standard") {
  if (!is.character(value)) stop("use character string to denote target unit")
  # Use the baseline data.frame recycling contract, including its empty errors.
  n <- length(x)
  if (!n || !length(value) || length(value) > n || n %% length(value)) {
    data.frame(val = as.numeric(x), from = I(units(x)), to = value, idx = seq_along(x))
    stop("argument 1 is not a vector")
  }
  values <- as.numeric(x) # also preserves the error for unknown units
  labels <- unique(value)
  target <- rep_len(match(value, labels), n)
  source <- attr(x, "unit_id")
  dictionary <- attr(x, "unit_dictionary")
  groups <- split(seq_len(n), paste(source, target, sep = ":"))
  out <- numeric(n)
  id <- integer(n)
  result_units <- vector("list", length(groups))
  for (g in seq_along(groups)) {
    i <- groups[[g]]
    source_unit <- dictionary[[source[i[1L]]]]
    ordinary <- .as.units(values[i], source_unit)
    converted <- set_units(ordinary, labels[target[i[1L]]], mode = "standard")
    out[i] <- as.numeric(converted)
    id[i] <- g
    result_units[[g]] <- units(converted)
  }
  # Dictionary order follows first occurrence, independently of group ordering.
  order <- unique(id)
  .mixed_assemble(out, match(id, order), result_units[order])
}

#' @export
#' @name mixed_units
`units<-.mixed_units` <- function(x, value) set_units(x, value)

#' @export
units.mixed_units <- function(x) {
  u <- attr(x, "unit_dictionary")[attr(x, "unit_id")]
  names(u) <- names(x)
  structure(u, class = "mixed_symbolic_units")
}

#' @export
as.character.mixed_symbolic_units <- function(x, ...) {
  sapply(x, function(i) if (!is.null(i)) as.character(i) else "NULL")
}

#' @export
as_units.mixed_units <- function(x, ...) {
  if (!length(x)) stop("cannot convert empty mixed_units to units", call. = FALSE)
  if (anyNA(attr(x, "unit_id")))
    stop("cannot convert elements with unknown units to units", call. = FALSE)
  id <- attr(x, "unit_id")
  dictionary <- attr(x, "unit_dictionary")
  target <- dictionary[[id[1L]]]
  values <- .mixed_values(x)
  for (i in split(seq_along(x), id)) {
    source <- .as.units(values[i], dictionary[[id[i[1L]]]])
    values[i] <- as.numeric(set_units(source, target, mode = "standard"))
  }
  names(values) <- names(x)
  .as.units(values, target)
}

#' @export
format.mixed_units <- function(x, ...) sapply(as.list(x), format, ...)

.cat_units_table <- function(x) {
  cat("Mixed units: ")
  if (!length(x)) return()
  x <- as.character(units(x))
  u <- unique(x)
  n <- tabulate(factor(x, u))
  cat(paste(u, " (", n, ")", sep = "", collapse = ", "), "\n")
}

#' @export
print.mixed_units <- function(x, ...) {
  .cat_units_table(x)
  cat(paste(format(x, ...), collapse = ", "), "\n")
  invisible(x)
}

#' @export
str.mixed_units <- function(object, ...) {
  .cat_units_table(object)
  cat(capture.output(str(as.list(object), ...))[-1], sep = "\n")
}

#' @name drop_units
#' @export
drop_units.mixed_units <- function(x) {
  if (!length(x)) return(setNames(list(), character()))
  out <- as.numeric(x)
  names(out) <- names(x)
  out
}

.Ops.mixed_units <- function(e1, e2, op) {
  if (!op %in% c("==", "!=", "*", "/", "+", "-"))
    stop(paste("operation", op, "not supported"))
  if (missing(e2)) stop('argument "e2" is missing, with no default')
  if (inherits(e2, "units")) e2 <- mixed_units(e2)
  mixed1 <- inherits(e1, "mixed_units")
  mixed2 <- inherits(e2, "mixed_units")
  plain <- function(x) !is.object(x) && is.null(dim(x)) &&
    (is.numeric(x) || is.logical(x)) && !is.complex(x)
  if ((!mixed1 && !plain(e1)) || (!mixed2 && !plain(e2)))
    stop("operands must be numeric or mixed_units", call. = FALSE)
  if (op %in% c("+", "-", "==", "!=") && (!mixed1 || !mixed2))
    stop('both operands of the expression should be "units" objects', call. = FALSE)
  if (!length(e1) || !length(e2))
    return(.new_mixed(numeric(), integer(), list(), if (!is.null(names(e1))) character()))
  n <- max(length(e1), length(e2))
  if (n %% min(length(e1), length(e2))) warning("longer argument not a multiple of length of shorter")
  a <- rep_len(if (mixed1) .mixed_values(e1) else e1, n)
  b <- rep_len(if (mixed2) .mixed_values(e2) else e2, n)
  ia <- if (mixed1) rep_len(attr(e1, "unit_id"), n) else rep.int(0L, n)
  ib <- if (mixed2) rep_len(attr(e2, "unit_id"), n) else rep.int(0L, n)
  da <- attr(e1, "unit_dictionary")
  db <- attr(e2, "unit_dictionary")
  known <- which(!is.na(ia) & !is.na(ib))
  groups <- split(known, paste(ia[known], ib[known], sep = ":"))
  comparison <- op %in% c("==", "!=")
  out <- if (comparison) rep(NA, n) else rep(NA_real_, n)
  id <- rep(NA_integer_, n)
  dictionary <- vector("list", length(groups))
  for (g in seq_along(groups)) {
    i <- groups[[g]]
    left <- if (mixed1) .as.units(a[i], da[[ia[i[1L]]]]) else a[i]
    right <- if (mixed2) .as.units(b[i], db[[ib[i[1L]]]]) else b[i]
    result <- do.call(op, list(left, right))
    out[i] <- if (comparison) result else as.numeric(result)
    if (!comparison) dictionary[[g]] <- units(result)
    id[i] <- g
  }
  nm <- names(e1)
  if (comparison) { names(out) <- nm; return(out) }
  order <- unique(id[!is.na(id)])
  .mixed_assemble(out, match(id, order), dictionary[order], nm)
}

#' @export
duplicated.mixed_units <- function(x, incomparables = FALSE, ...) {
  values <- .mixed_values(x)
  id <- attr(x, "unit_id")
  groups <- c(unname(split(seq_along(x), id)), list(which(is.na(id))))
  out <- logical(length(x))
  for (i in groups) out[i] <- duplicated(values[i], ...)
  if (length(incomparables) && !identical(incomparables, FALSE)) {
    excluded <- if (inherits(incomparables, "mixed_units")) incomparables else
      .as.mixed_units(incomparables)
    d <- .mixed_keys(attr(x, "unit_dictionary"))
    other <- match(.mixed_keys(attr(excluded, "unit_dictionary")), d)[attr(excluded, "unit_id")]
    for (i in seq_along(excluded)) {
      if (is.na(other[i]) && !is.na(attr(excluded, "unit_id")[i])) next
      same <- match(.mixed_values(x), .mixed_values(excluded)[i], nomatch = 0L) == 1L &
        match(attr(x, "unit_id"), other[i], nomatch = 0L) == 1L
      out[same] <- FALSE
    }
  }
  out
}

#' @export
anyDuplicated.mixed_units <- function(x, incomparables = FALSE, fromLast = FALSE, ...) {
  which_duplicate <- which(duplicated(x, incomparables = incomparables, fromLast = fromLast, ...))
  if (!length(which_duplicate)) return(0L)
  if (fromLast) which_duplicate[length(which_duplicate)] else which_duplicate[1L]
}

#' @export
unique.mixed_units <- function(x, incomparables = FALSE, ...) {
  out <- x[!duplicated(x, incomparables = incomparables, ...)]
  names(out) <- NULL
  out
}

#' @export
as.data.frame.mixed_units <- as.data.frame.AsIs

# Share the group method to prevent primitive numeric fall-through.
#' @export
Ops.mixed_units <- Ops.units

# Explicit behavior prevents numeric storage from enabling operations that
# would discard dimensions without a unit compatibility check.
#' @export
Math.mixed_units <- function(x, ...) stop(paste("operation", .Generic, "not supported"), call. = FALSE)
#' @export
Summary.mixed_units <- function(..., na.rm = FALSE) {
  stop(paste("operation", .Generic, "not supported"), call. = FALSE)
}
#' @export
xtfrm.mixed_units <- function(x) {
  known <- !is.na(x)
  if (sum(known) > 1L) stop("ordering is not supported for mixed_units", call. = FALSE)
  out <- rep(NA_real_, length(x))
  out[known] <- 1
  out
}
#' @method all.equal mixed_units
#' @export
all.equal.mixed_units <- function(target, current, ...) {
  if (!inherits(current, "mixed_units")) return("current is not a mixed_units object")
  if (length(target) != length(current)) return("Lengths differ")
  a <- attr(target, "unit_id")
  b <- attr(current, "unit_id")
  if (!identical(is.na(a), is.na(b))) return("Unknown units differ")
  values <- .mixed_values(current)
  known <- which(!is.na(a))
  for (i in split(known, paste(a[known], b[known], sep = ":"))) {
    source <- .as.units(values[i], attr(current, "unit_dictionary")[[b[i[1L]]]])
    values[i] <- as.numeric(set_units(source,
      attr(target, "unit_dictionary")[[a[i[1L]]]], mode = "standard"))
  }
  names(values) <- names(current)
  expected <- .mixed_values(target)
  names(expected) <- names(target)
  all.equal(expected, values, ...)
}

#' @export
`$<-.mixed_units` <- function(x, name, value) {
  stop("$<- is not supported for mixed_units; use [[<- or [<-", call. = FALSE)
}
