type_sum.units <- function(x, ...) {
  gr = units_options("group")
  # see https://github.com/r-lib/pillar/issues/73 : currently the [ and ] mess up things.
  structure(paste0(gr[1], as.character(units(x)), gr[2]),
            class = "type_sum_units")
}

type_sum.mixed_units <- function(x, ...) {
  "mixed_units"
}

pillar_shaft.units <- function(x, ...) {
  pillar::pillar_shaft(unclass(x), ...)
}

pillar_shaft.mixed_units <- function(x, ...) {
  u_char <- pillar::style_subtle(sapply(x, function(unit) format(unit[0])))
  out <- paste0(as.numeric(x), u_char)
  pillar::new_pillar_shaft_simple(out, ..., align = "right", min_width = 6)
}

format_type_sum.type_sum_units <- function(x, width, ...) {
  pillar::style_subtle(x)
}


# vctrs proxying and restoration -------------------------------------

vec_proxy.units = function(x, ...) {
  x
}
vec_restore.units = function(x, to, ...) {
  restore_units(x, to)
}
restore_units <- function(x, to) {
  out <- .as.units(x, units(to))
  attr(out, "pillar") <- attr(to, "pillar")
  out
}


# vctrs coercion -----------------------------------------------------

vec_ptype2.units.units = function(x, y, ..., x_arg = "", y_arg = "") {
  x_units = units(x)
  y_units = units(y)

  if (!ud_are_convertible(x_units, y_units))
    vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)

  x_bare = drop_units(x)
  y_bare = drop_units(y)
  common = vctrs::vec_ptype2(x_bare, y_bare, ..., x_arg = x_arg, y_arg = y_arg)

  # Use left-hand side units
  set_units(common, x_units, mode = "standard")
}

vec_cast.units.units = function(x, to, ..., x_arg = "", to_arg = "") {
  x_units = units(x)
  to_units = units(to)

  if (!ud_are_convertible(x_units, to_units))
    vctrs::stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)

  # Convert to target units before converting base type. Unit
  # conversion might change the type and so must happen first.
  out = set_units(x, to_units, mode = "standard")

  out_bare = drop_units(out)
  to_bare = drop_units(to)
  out = vctrs::vec_cast(out_bare, to_bare, ..., x_arg = x_arg, to_arg = to_arg)

  # Set target units again
  set_units(out, to_units, mode = "standard")
}


#nocov start
register_all_s3_methods <- function() {
  register_s3_method("pillar::type_sum", "units")
  register_s3_method("pillar::type_sum", "mixed_units")
  register_s3_method("pillar::pillar_shaft", "units")
  register_s3_method("pillar::pillar_shaft", "mixed_units")
  register_s3_method("pillar::format_type_sum", "type_sum_units")
  register_s3_method("vctrs::vec_proxy", "units")
  register_s3_method("vctrs::vec_restore", "units")
  register_s3_method("vctrs::vec_ptype2", "units.units")
  register_s3_method("vctrs::vec_cast", "units.units")
  for (generic in c("vec_proxy", "vec_proxy_equal", "vec_restore", "vec_ptype"))
    register_s3_method(paste0("vctrs::", generic), "mixed_units")
  register_s3_method("vctrs::vec_ptype2", "mixed_units.mixed_units")
  register_s3_method("vctrs::vec_cast", "mixed_units.mixed_units")
  register_s3_method("ggplot2::scale_type", "units")
  register_s3_method("ggplot2::limits", "units")
}

register_s3_method <- function(generic, class, fun=NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]

  if (is.null(fun))
    fun <- get(paste0(generic, ".", class), envir=parent.frame())
  stopifnot(is.function(fun))

  if (package %in% loadedNamespaces())
    registerS3method(generic, class, fun, envir=asNamespace(package))

  # Always register hook in case package is later unloaded & reloaded
  setHook(packageEvent(package, "onLoad"), function(...)
    registerS3method(generic, class, fun, envir=asNamespace(package)))
}
# nocov end

# Mixed vectors carry integer codes through the proxy. Symbolic metadata lives
# on the type prototype; factor levels align different dictionary orders.
vec_proxy.mixed_units <- function(x, ...) {
  unit <- structure(attr(x, "unit_id"),
    levels = .mixed_keys(attr(x, "unit_dictionary")), class = "factor")
  value <- .mixed_values(x)
  names(value) <- names(x)
  vctrs::new_data_frame(list(value = value, unit = unit))
}
vec_proxy_equal.mixed_units <- vec_proxy.mixed_units
vec_restore.mixed_units <- function(x, to, ...) {
  dictionary <- attr(to, "unit_dictionary")
  pos <- match(levels(x$unit), .mixed_keys(dictionary))
  stopifnot(!anyNA(pos))
  .new_mixed(x$value, as.integer(x$unit), dictionary[pos], names(x$value))
}
vec_ptype.mixed_units <- function(x, ...) {
  .new_mixed(double(), integer(), attr(x, "unit_dictionary"), compact = FALSE)
}
vec_ptype2.mixed_units.mixed_units <- function(x, y, ...) {
  dictionary <- c(attr(x, "unit_dictionary"), attr(y, "unit_dictionary"))
  dictionary <- dictionary[!duplicated(.mixed_keys(dictionary))]
  .new_mixed(double(), integer(), dictionary, compact = FALSE)
}
vec_cast.mixed_units.mixed_units <- function(x, to, ...) {
  dictionary <- attr(to, "unit_dictionary")
  id <- match(.mixed_keys(attr(x, "unit_dictionary")), .mixed_keys(dictionary))[attr(x, "unit_id")]
  if (any(!is.na(attr(x, "unit_id")) & is.na(id)))
    stop("Target unit dictionary does not contain all source units.")
  .new_mixed(.mixed_values(x), id, dictionary, names(x), compact = FALSE)
}
