# Separate sessions: Rscript tools/benchmark/mixed.R LIBRARY OUTPUT_PREFIX [EXTRA_LIBRARY]
args <- commandArgs(TRUE)
.libPaths(c(args[1], if (length(args) > 2) args[3], .libPaths()))
library(units)
stopifnot(requireNamespace("bench"), requireNamespace("lobstr"))
set.seed(20261001)
rows <- sizes <- checks <- list()
for (k in c(8L, 2L)) {
  n <- 10000L
  labels <- c("m", "s", "kg", "L", paste0("m^", 2:5))[seq_len(k)]
  targets <- c("cm", "min", "g", "mL", paste0("cm^", 2:5))[seq_len(k)]
  id <- sample(rep_len(seq_len(k), n))
  value <- runif(n, 1, 10)
  x <- mixed_units(value, labels[id])
  y <- mixed_units(value / 2, targets[id])
  z <- mixed_units(value, sample(labels[id]))
  take <- sample.int(n, n / 2L)
  replacement <- function() { out <- x; out[1:100] <- mixed_units(1, "K"); out }
  jobs <- list(construct = function() mixed_units(value, labels[id]),
    convert = function() set_units(x, targets[id]), slice = function() x[take],
    replace = replacement, concatenate = function() c(x, z),
    scale = function() x * 2, add = function() x + y,
    multiply_pairs = function() x * z, repetition = function() rep(x, 2))
  sizes[[length(sizes) + 1L]] <- data.frame(k = k,
    bytes = as.numeric(do.call(lobstr::obj_size, c(list(x), unname(attributes(x))))))
  for (job in names(jobs)) {
    message("k=", k, " ", job)
    f <- jobs[[job]]
    result <- f()
    # Decode outside timing. Repetition intentionally gains the mixed class.
    scalar <- as.list(result)
    checks[[paste(k, job)]] <- list(value = vapply(scalar, as.numeric, 0.),
      unit = vapply(scalar, deparse_unit, ""), names = names(result))
    b <- bench::mark(f(), iterations = 5, check = FALSE, filter_gc = FALSE)
    rows[[length(rows) + 1L]] <- data.frame(k = k, workload = job,
      median_ms = as.numeric(b$median) * 1000, allocated_bytes = as.numeric(b$mem_alloc))
  }
}
write.csv(do.call(rbind, rows), paste0(args[2], "-timings.csv"), row.names = FALSE)
write.csv(do.call(rbind, sizes), paste0(args[2], "-sizes.csv"), row.names = FALSE)
saveRDS(checks, paste0(args[2], "-checks.rds"))
capture.output(sessionInfo(), file = paste0(args[2], "-session.txt"))
