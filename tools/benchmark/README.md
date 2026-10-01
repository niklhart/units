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

Reproduce from the repository root (bench and lobstr must be installed in the
shared dependency library):

```sh
Rscript tools/benchmark/mixed.R MAIN_LIBRARY OUTPUT/main SHARED_LIBRARY
Rscript tools/benchmark/mixed.R REDESIGN_LIBRARY OUTPUT/redesign SHARED_LIBRARY
Rscript tools/benchmark/mixed-compatibility.R MAIN_LIBRARY OUTPUT/main-compatibility.rds
Rscript tools/benchmark/mixed-compatibility.R REDESIGN_LIBRARY OUTPUT/redesign-compatibility.rds
```

Compare the paired `*-checks.rds` and `*-compatibility.rds` files with
`identical(readRDS(...), readRDS(...))`. Raw results and session information are
in `mixed-results/`.

The implementation stores doubles with an integer `unit_id` attribute and a
`unit_dictionary` of symbolic units. Construction processes distinct labels;
conversion groups source/target unit pairs and delegates actual values to the
unchanged `set_units.units` method. Arithmetic similarly delegates observed
operand-unit pairs to ordinary `units` vectors.

Optional vctrs integration carries unit IDs as factor codes. Empty subsets
remove unused dictionary entries, while type prototypes and nonempty buffers
with entirely unknown units retain their dictionaries for subsequent assignment.
Casts reject known units absent from the target dictionary. Regression coverage
includes reordered dictionaries, known-unit missing values, sparse pivots,
missing output columns, and names during slicing and assignment.

Numeric storage changes direct iteration, storage inspection and serialization.
Use `as.list()` when scalar `units` objects are needed. Irregular objects created
by permissive list assignment retain a legacy list representation and do not
receive the dictionary representation's performance benefits.

Validation ran the full test suite with `NOT_CRAN=true`, including plotting and
pillar snapshots. Package checks used `--no-manual --no-build-vignettes` and
`_R_CHECK_FORCE_SUGGESTS_=false`. Both versions had the same three warnings:
one compiler warning for `-Wfixed-enum-extension` and two for unbuilt vignettes.
NISTunits and measurements were unavailable. Tests, examples and code checks
passed; manual and rendered-vignette checks were not performed.
