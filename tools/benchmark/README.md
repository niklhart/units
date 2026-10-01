# mixed_units redesign benchmark

Baseline: unchanged main `a823fef46d92ee4e1b2e2fa28ddf4c19d67cefb6`.
R 4.5.2, arm64 macOS; separate installations and sequential sessions using the
same dependency library and seed. Each fixture contains 10,000 elements with
balanced, shuffled unit types. Five iterations, including garbage collection;
these are exploratory medians, not performance guarantees.

| Workload | Main, 8 types (ms) | Redesign, 8 types (ms) | Main, 2 types (ms) | Redesign, 2 types (ms) |
| --- | ---: | ---: | ---: | ---: |
| construct | 18.894 | 0.998 | 18.067 | 0.646 |
| convert | 85.029 | 2.208 | 65.917 | 1.605 |
| slice | 0.024 | 0.081 | 0.024 | 0.067 |
| replace | 0.259 | 0.255 | 0.122 | 0.232 |
| concatenate | 0.149 | 0.343 | 0.107 | 0.331 |
| scale | 549.676 | 1.716 | 548.294 | 1.378 |
| add | 656.038 | 1.789 | 604.639 | 1.368 |
| multiply_pairs | 555.080 | 4.457 | 454.138 | 1.532 |
| repetition | 0.104 | 0.290 | 0.098 | 0.245 |

Retained size (lobstr, with explicit attribute roots to avoid undercounting IDs):
8 types **1,764,864 → 124,808 bytes** (93% smaller); 2 types
**1,762,256 → 122,152 bytes**. The redesign is slower for slicing, concatenation
and repetition; two-type replacement is also slower. Main's repetition loses
its class, whereas the redesign preserves it.

Raw CSVs include profiler-reported allocations. These are not retained size,
peak memory, or complete allocation estimates: R undercounts small allocations,
which strongly favors main's scalar-list arithmetic in those figures.
`multiply_pairs` independently shuffles operand units: eight types yield up to
64 observed pairs, versus four for two types.

All **18** workload results have exactly identical decoded numeric values,
canonical unit labels and names outside timing. The separate compatibility
runner covers **74** constructor, conversion, arithmetic, extraction,
replacement, coercion and error cases. It normalizes storage and errors (error
wording and warning text are not checked). Broader API regression tests live in
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

There is a compatibility exception to numeric storage. The old list-based
implementation allowed assignment of elements that are not scalar `units`
objects, such as a bare number:

```r
x <- mixed_units(c(1, 2), c("m", "s"))
x[[1]] <- 3
```

Here the first element is the plain number `3`, with no unit attached; the second
still has unit `s`. The redesign continues to accept this assignment. Since the
dictionary representation cannot express an arbitrary list element, the entire
object switches back to a list while retaining its `mixed_units` class. This
preserves the assigned contents without inventing a unit for `3` or introducing
a new assignment error. Such objects do not receive the dictionary storage or
grouped-arithmetic benefits measured above. Assigning a scalar `units` object,
such as `x[[1]] <- set_units(3, m)`, allows dictionary storage again when all
elements are suitable.

Validation ran the full test suite with `NOT_CRAN=true`, including plotting and
pillar snapshots. Package checks used `--no-manual --no-build-vignettes` and
`_R_CHECK_FORCE_SUGGESTS_=false`. Both versions had the same three warnings:
one compiler warning for `-Wfixed-enum-extension` and two for unbuilt vignettes.
NISTunits and measurements were unavailable. Tests, examples and code checks
passed; manual and rendered-vignette checks were not performed.
