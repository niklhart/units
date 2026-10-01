# mixed_units redesign benchmark

Baseline: unchanged main `a823fef46d92ee4e1b2e2fa28ddf4c19d67cefb6`.
R 4.5.2, arm64 macOS; separate installations and sequential sessions using the
same dependency library and seed. Each fixture contains 10,000 elements with
balanced, shuffled unit types. Five iterations, including garbage collection;
these are exploratory medians, not performance guarantees.

| Workload | Main, 8 types (ms) | Redesign, 8 types (ms) | Main, 2 types (ms) | Redesign, 2 types (ms) |
| --- | ---: | ---: | ---: | ---: |
| construct | 19.351 | 1.150 | 20.211 | 0.642 |
| convert | 87.352 | 2.643 | 85.288 | 1.650 |
| slice | 0.024 | 0.140 | 0.023 | 0.105 |
| replace | 0.113 | 0.426 | 0.202 | 0.324 |
| concatenate | 0.112 | 0.675 | 0.122 | 0.631 |
| scale | 598.723 | 2.076 | 546.508 | 1.557 |
| add | 687.010 | 1.954 | 579.170 | 1.601 |
| multiply_pairs | 474.229 | 4.775 | 465.529 | 1.712 |
| repetition | 0.095 | 0.419 | 0.092 | 0.401 |
| scalar_replace | 0.093 | 0.337 | 0.104 | 0.304 |
| drop_units | 90.346 | 0.006 | 90.403 | 0.005 |
| as_units | 430.476 | 0.231 | 483.761 | 0.209 |
| duplicated | 1.030 | 0.469 | 0.937 | 0.473 |
| unique | 0.984 | 0.721 | 1.005 | 0.630 |
| known_missing_scale | 529.869 | 1.924 | 584.642 | 1.488 |
| unknown_missing_scale | 527.154 | 1.941 | 542.449 | 1.547 |

Retained size (lobstr, with explicit attribute roots to avoid undercounting IDs):
8 types **1,764,864 → 124,808 bytes** (93% smaller); 2 types
**1,762,256 → 122,152 bytes**. The redesign is slower for slicing, replacement,
concatenation and repetition; validation and dictionary maintenance add work to
these operations. Main's repetition loses
its class, whereas the redesign preserves it.

Raw CSVs include profiler-reported allocations. These are not retained size,
peak memory, or complete allocation estimates: R undercounts small allocations,
which strongly favors main's scalar-list arithmetic in those figures.
`multiply_pairs` independently shuffles operand units: eight types yield up to
64 observed pairs, versus four for two types. The missing-value workloads replace
one in every 100 elements with a known-unit NA or an unknown-unit missing value.
Duplicate detection and `unique()` use two copies of the input. The `as_units()`
workload uses alternating metres and centimetres so conversion to a common unit
is possible.

All **32** workload results agree after decoding values, unit labels and names
outside timing. Unknown measurements are decoded as missing values with unknown
units: main stores these as NULL or zero-length list elements, whereas the
redesign uses missing values and IDs. The separate compatibility
runner covers **63** constructor, conversion, arithmetic, extraction,
replacement, coercion and error cases. It normalizes storage and errors (error
wording and warning text are not checked). Cases deliberately changed by the
redesign are checked separately in the regression suite. Broader API tests live in
`tests/testthat/test_mixed_dictionary.R`.

To reproduce the comparison, install the baseline and the current working tree
into separate R package libraries. An R library is simply a directory containing
installed packages; using two directories lets each run load the intended version
of `units`. Both runs use the same R installation and dependencies.

The commands below assume that R can build `units` on your machine, including
its udunits-2 system dependency, and that the R packages `Rcpp`, `bench`, and
`lobstr` are installed in your usual R library. Run these shell commands from
the repository root:

```sh
# Create temporary source, package-library and results directories.
export benchmark_dir="$(mktemp -d)"
mkdir -p "$benchmark_dir/source-main" "$benchmark_dir/lib-main" \
  "$benchmark_dir/lib-redesign" "$benchmark_dir/results"

# Install the exact baseline used above, without changing the working tree.
git archive a823fef46d92ee4e1b2e2fa28ddf4c19d67cefb6 | \
  tar -x -C "$benchmark_dir/source-main"
R CMD INSTALL --library="$benchmark_dir/lib-main" "$benchmark_dir/source-main"

# Install the redesign, including any uncommitted changes.
R CMD INSTALL --library="$benchmark_dir/lib-redesign" .

# Run each version in a separate R session, one after the other.
Rscript tools/benchmark/mixed.R \
  "$benchmark_dir/lib-main" "$benchmark_dir/results/main"
Rscript tools/benchmark/mixed.R \
  "$benchmark_dir/lib-redesign" "$benchmark_dir/results/redesign"

# Compare public behavior separately from the timed workloads.
Rscript tools/benchmark/mixed-compatibility.R \
  "$benchmark_dir/lib-main" "$benchmark_dir/results/main-compatibility.rds"
Rscript tools/benchmark/mixed-compatibility.R \
  "$benchmark_dir/lib-redesign" "$benchmark_dir/results/redesign-compatibility.rds"
```

Each script's first argument selects the library containing `units`. The second
is an output filename prefix for the benchmark, or an output filename for the
compatibility checks. The benchmark also accepts an optional third argument: a
library containing `bench`, `lobstr`, and their dependencies, if these are not
installed in your usual R library. It is unnecessary in the example above.

The benchmark writes timings and retained sizes as CSV files, checked values as
RDS files, and R/package versions as text. To verify that both versions returned
the same results, run this in the same shell after the commands above:

```sh
Rscript -e '
  results <- file.path(Sys.getenv("benchmark_dir"), "results")
  for (suffix in c("checks", "compatibility")) {
    baseline <- readRDS(file.path(results, paste0("main-", suffix, ".rds")))
    redesign <- readRDS(file.path(results, paste0("redesign-", suffix, ".rds")))
    stopifnot(identical(baseline, redesign))
  }
  cat("Both result comparisons passed. Results are in", results, "\n")
'
```

The recorded results behind the table above are in `mixed-results/`.

The redesign stores numeric values alongside integer references to a dictionary
of units. Conversion and arithmetic collect values with the same unit pair and
process them together using the existing methods for ordinary `units` vectors.
The conversion method `set_units.units` is unchanged.

Optional vctrs integration keeps those references aligned during slicing,
combining and reshaping. Empty subsets discard unused units, but empty type
prototypes and buffers awaiting assignment retain the unit information needed
to fill them. Regression tests cover different dictionary orders, missing values
with known or unknown units, sparse pivots, and preservation of element names.

Assigning a bare numeric scalar with `[[<-` now gives it a dimensionless unit:

```r
x <- mixed_units(c(1, 2), c("m", "s"))
x[[1]] <- 3  # equivalent to x[[1]] <- set_units(3, 1)
```

The object keeps dictionary storage, and `x[[1]]` returns an ordinary `units`
scalar. The same dimensionless interpretation applies to numeric values in
`[<-` and `c()`. These deliberately change the old behavior, which left bare
numbers inside the underlying list. These corrections, like class-preserving
`rep()`, are covered by regression tests
rather than required to match the baseline.

Invalid scalar replacements, such as `x[[1]] <- "text"`, now raise an error.
All mixed results have numeric storage, integer unit IDs and a unit dictionary;
there is no list-backed representation. Replacement accepts numeric or
unit-bearing values, with `[[<-` restricted to scalars. Assigning `NULL` deletes
elements. `$` and `$<-` are unsupported; use `[[` or `[` with a name instead.
`is.na()`, `lengths()` and `as.vector()` use the ordinary numeric defaults.
There is no special resizing method: changing the length with `length<-` drops
the unit metadata and class, as it does for ordinary `units` vectors. Arithmetic with an unknown unit produces a missing value with
an unknown unit, while known-unit missing values retain their units.

Validation ran the full test suite with `NOT_CRAN=true`, including plotting and
pillar snapshots. Package checks used `--no-manual --no-build-vignettes` and
`_R_CHECK_FORCE_SUGGESTS_=false`. Both versions had the same three warnings:
one compiler warning for `-Wfixed-enum-extension` and two for unbuilt vignettes.
NISTunits and measurements were unavailable. Tests, examples and code checks
passed; manual and rendered-vignette checks were not performed.
