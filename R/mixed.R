# Numeric storage and dictionary helpers. Symbolic units preserve custom units and
# the package's formatting semantics without reparsing labels.
.mixed_units_key <- function(u) if (is.null(u)) "NULL" else ud_char(u)
.mixed_keys <- function(d) vapply(d, .mixed_units_key, "")
.mixed_values <- function(x) {
  attributes(x) <- NULL
  x
}
.new_mixed <- function(x, id, dictionary, nm = NULL, compact = TRUE) {
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
  stopifnot(is.list(unclass(x)))
  valid <- vapply(x, function(a) is.null(a) ||
    (inherits(a, "units") && length(a) == 1L && is.null(names(a))), TRUE)
  # Preserve the historically permissive list assignment/concatenation API.
  if (!all(valid)) return(structure(x, class = c("mixed_units", "list")))
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
#' Assigning a bare numeric scalar with \code{[[<-} creates a dimensionless
#' \code{units} element, equivalent to assigning \code{set_units(value, 1)}.
#' Other replacements must be numeric \code{units} scalars, or \code{NULL}
#' to delete an element; invalid replacements raise an error.
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
  mixed_units(unclass(x), rep(as.character(units(x)), length(x)))
}

#' @export
mixed_units.numeric <- function(x, values, ...) {
  stopifnot(is.character(values), is.numeric(x))
  if (!ud_recycles(x, values))
    return(.as.mixed_units(mapply(set_units, x, values, mode = "standard", SIMPLIFY = FALSE)))
  n <- max(length(x), length(values))
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
  if (is.list(x)) return(unclass(x))
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
  if (is.list(x)) return(as.double(unclass(x)))
  if (anyNA(attr(x, "unit_id"))) return(as.double(as.list(x)))
  .mixed_values(x)
}

#' @export
as.character.mixed_units <- function(x, ...) as.character(as.list(x))

#' @export
`[[.mixed_units` <- function(x, i, ...) {
  if (is.list(x)) return(unclass(x)[[i, ...]])
  if (length(i) != 1L) return(as.list(x)[[i, ...]])
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
  if (is.list(x)) return(.as.mixed_units(unclass(x)[i]))
  v <- .mixed_values(x)
  names(v) <- names(x)
  id <- attr(x, "unit_id")
  names(id) <- names(x)
  out <- v[i]
  .new_mixed(out, id[i], attr(x, "unit_dictionary"), names(out))
}

#' @export
`[<-.mixed_units` <- function(x, i, ..., value) {
  if (inherits(value, "units")) value <- mixed_units(value)
  if (is.list(x) || !inherits(value, "mixed_units") || is.list(value)) {
    out <- as.list(x)
    if (inherits(value, "mixed_units")) value <- as.list(value)
    out[i] <- value
    return(.as.mixed_units(out))
  }
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
`[[<-.mixed_units` <- function(x, ..., value) {
  if (length(value) == 1L && !is.object(value) && is.null(dim(value)) &&
      !is.complex(value) &&
      (is.numeric(value) || (is.logical(value) && is.na(value))))
    value <- set_units(value, "1", mode = "standard")
  if (!is.null(value) &&
      (!inherits(value, "units") || length(value) != 1L ||
       !is.numeric(value) || is.complex(value) || !is.null(dim(value))))
    stop("replacement must be a numeric scalar, a units scalar, or NULL", call. = FALSE)
  out <- as.list(x)
  out[[...]] <- value
  .as.mixed_units(out)
}

#' @export
rep.mixed_units <- function(x, ...) x[rep(seq_along(x), ...)]

#' @export
c.mixed_units <- function(...) {
  args <- list(...)
  if (!all(vapply(args, function(x) inherits(x, "mixed_units") && !is.list(x), TRUE)))
    return(.as.mixed_units(do.call(c, lapply(args, function(x)
      if (inherits(x, "mixed_units")) as.list(x) else unclass(x)))))
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
  if (is.list(x)) {
    dictionary <- unclass(units(x))
    keys <- .mixed_keys(dictionary)
    source <- match(keys, unique(keys))
    dictionary <- dictionary[!duplicated(keys)]
  } else {
    source <- attr(x, "unit_id")
    dictionary <- attr(x, "unit_dictionary")
  }
  groups <- split(seq_len(n), paste(source, target, sep = ":"))
  out <- numeric(n)
  id <- integer(n)
  result_units <- vector("list", length(groups))
  for (g in seq_along(groups)) {
    i <- groups[[g]]
    source_unit <- dictionary[[source[i[1L]]]]
    ordinary <- if (is.null(source_unit)) values[i] else .as.units(values[i], source_unit)
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
  if (is.list(x)) u <- lapply(unclass(x), function(a) if (inherits(a, "units")) units(a) else NULL)
  else u <- attr(x, "unit_dictionary")[attr(x, "unit_id")]
  names(u) <- names(x)
  structure(u, class = "mixed_symbolic_units")
}

#' @export
as.character.mixed_symbolic_units <- function(x, ...) {
  sapply(x, function(i) if (!is.null(i)) as.character(i) else "NULL")
}

#' @export
as_units.mixed_units <- function(x, ...) {
  set_units(do.call(c, as.list(x)), value = units(x[[1]]), mode = "standard")
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
drop_units.mixed_units <- function(x) sapply(as.list(x), drop_units)

.Ops.mixed_units <- function(e1, e2, op) {
  if (!op %in% c("==", "!=", "*", "/", "+", "-"))
    stop(paste("operation", op, "not supported"))
  if (missing(e2)) stop('argument "e2" is missing, with no default')
  if (inherits(e2, "units")) e2 <- mixed_units(e2)
  mixed1 <- inherits(e1, "mixed_units")
  mixed2 <- inherits(e2, "mixed_units")
  # Irregular lists and unknown elements keep mapply's historical behavior.
  irregular <- function(x) is.list(x) || (inherits(x, "mixed_units") && anyNA(attr(x, "unit_id")))
  if (irregular(e1) || irregular(e2) || !length(e1) || !length(e2)) {
    a <- if (mixed1) as.list(e1) else e1
    b <- if (mixed2) as.list(e2) else e2
    ret <- mapply(op, a, b, SIMPLIFY = op %in% c("==", "!="))
    return(if (is.list(ret)) .as.mixed_units(ret) else ret)
  }
  n <- max(length(e1), length(e2))
  if (n %% min(length(e1), length(e2))) warning("longer argument not a multiple of length of shorter")
  a <- rep_len(if (mixed1) .mixed_values(e1) else e1, n)
  b <- rep_len(if (mixed2) .mixed_values(e2) else e2, n)
  ia <- if (mixed1) rep_len(attr(e1, "unit_id"), n) else rep.int(0L, n)
  ib <- if (mixed2) rep_len(attr(e2, "unit_id"), n) else rep.int(0L, n)
  da <- attr(e1, "unit_dictionary")
  db <- attr(e2, "unit_dictionary")
  groups <- split(seq_len(n), paste(ia, ib, sep = ":"))
  comparison <- op %in% c("==", "!=")
  out <- if (comparison) logical(n) else numeric(n)
  id <- integer(n)
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
  order <- unique(id)
  .mixed_assemble(out, match(id, order), dictionary[order], nm)
}

#' @export
duplicated.mixed_units <- function(x, incomparables = FALSE, ...) {
  duplicated(as.list(x), incomparables = incomparables, ...)
}

#' @export
anyDuplicated.mixed_units <- function(x, incomparables = FALSE, ...) {
  anyDuplicated(as.list(x), incomparables = incomparables, ...)
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

# Preserve list-era behavior for generics that otherwise silently compute on
# the new numeric storage without checking dimensions.
#' @export
Math.mixed_units <- function(x, ...) do.call(.Generic, c(list(as.list(x)), list(...)))
#' @export
Summary.mixed_units <- function(..., na.rm = FALSE) {
  args <- lapply(list(...), function(x) if (inherits(x, "mixed_units")) as.list(x) else x)
  do.call(.Generic, c(args, list(na.rm = na.rm)))
}
#' @export
mean.mixed_units <- function(x, ...) mean(as.list(x), ...)
#' @export
median.mixed_units <- function(x, ...) median(as.list(x), ...)
#' @export
xtfrm.mixed_units <- function(x) xtfrm(as.list(x))
#' @export
is.na.mixed_units <- function(x) is.na(as.list(x))
#' @export
as.vector.mixed_units <- function(x, mode = "any") {
  if (mode %in% c("any", "list")) x else as.vector(as.list(x), mode)
}
#' @method lengths mixed_units
#' @export
lengths.mixed_units <- function(x, use.names = TRUE) lengths(as.list(x), use.names)
#' @export
all.equal.mixed_units <- function(target, current, ...) {
  if (!inherits(current, "mixed_units")) return("current is not a mixed_units object")
  all.equal(as.list(target), as.list(current), ...)
}

#' @export
`length<-.mixed_units` <- function(x, value) {
  out <- as.list(x)
  length(out) <- value
  out
}
#' @export
`$.mixed_units` <- function(x, name) as.list(x)[[name, exact = FALSE]]
#' @export
`$<-.mixed_units` <- function(x, name, value) {
  out <- as.list(x)
  out[[name]] <- value
  .as.mixed_units(out)
}
