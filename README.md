# processdiagram

[![R-CMD-check](https://github.com/freejas/processdiagram/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/freejas/processdiagram/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE.md)

`processdiagram` creates publication-quality path diagrams and manuscript-ready figure notes for supported PROCESS-style mediation models in R.

It was designed around a simple principle: **if a figure is meant to represent a PROCESS analysis, the values shown in the figure should come from PROCESS itself**. The package can call a user-supplied `process()` function, preserve a bootstrap seed, validate the supported path structure independently, and use PROCESS's reported values as the authoritative values for the diagram.

`processdiagram` can also reconstruct figures directly from compatible PROCESS `save = 2` files without rerunning the analysis or knowing the original bootstrap seed.

`processdiagram` is independent software. It does not contain or redistribute PROCESS source code. PROCESS is copyrighted software by Andrew F. Hayes and must be obtained separately from the [official PROCESS download page](https://afhayes.com/download.html).

## PROCESS compatibility

`process_for_diagram()` supports the PROCESS for R output formats used by **PROCESS 4.1 and PROCESS 5.0** for the mediation structures currently supported by this package.

PROCESS 5.0 changed how standardized path information is printed. PROCESS 4.1 printed a one-column `Standardized coefficients` table, whereas PROCESS 5.0 prints a `Scale-free and standardized measures of association` table containing `standYX`, `standY`, and other measures. `processdiagram` handles both formats. To preserve the same figure semantics used with PROCESS 4.1, paths from a dichotomous focal X use PROCESS 5's `standY` value, while other paths use `standYX`. PROCESS 5 also adds `(StandY)` and `(StandYX)` to the standardized indirect-effect headings; both versions are recognized automatically.

`import_process_save2()` supports both **PROCESS for R 5.0** and compatible **PROCESS 4.1** `save = 2` matrices for the mediation structures supported by the package. Standardized PROCESS 5 files are detected from the saved scale-free/standardized association tables; unstandardized 4.1 and 5.0 files share the same supported layout. When an unstandardized file is structurally version-ambiguous, the importer can still recover the analysis, and `process_version = "5.0"` may be supplied when exact provenance is desired.

PROCESS 5.0 also introduces additional analysis options that are outside `processdiagram`'s current scope (for example, errors-in-variables estimation and several diagnostic/cluster options). Version compatibility here means the **supported mediation structures and standard estimation workflow** used by `processdiagram`; unsupported PROCESS options are not silently approximated and may fail validation.

## What is supported

The package currently supports:

- PROCESS Model 4, including multiple parallel mediators
- PROCESS Model 6 serial mediation
- PROCESS Model 81 with three to six mediators
- custom lower-triangular `bmatrix` path specifications
- OLS mediator equations
- dichotomous Y with a logistic final equation
- PROCESS-style standardized coefficients when PROCESS makes them available
- percentile-bootstrap indirect effects
- PROCESS for R 4.1 and 5.0 through `process_for_diagram()`
- PROCESS for R 5.0 and compatible 4.1 `save = 2` files
- indirect-effect contrast rows, including `contrast = 1`
- SVG, PDF, PNG, and TIFF export
- manuscript-ready figure notes generated from the same values used in the figure

It does **not** attempt to implement every feature of PROCESS. Unsupported structures should fail clearly rather than being silently approximated.

## Choose the right workflow

| Goal | Function |
|---|---|
| Make a figure from a new/current PROCESS analysis | `process_for_diagram()` |
| Create/recreate a figure from a PROCESS `save = 2` CSV/matrix | `import_process_save2()` |
| Fit a supported model independently of PROCESS | `fit_process_diagram()` |
| Fit and immediately draw with the native engine | `process_diagram()` |
| Draw an existing fit | `plot()` / `draw_process_diagram()` |
| Save SVG/PDF/TIFF/PNG | `save_process_diagram()` |
| Generate manuscript figure-note text | `figure_note()` |


## Runnable examples

The examples below use datasets distributed with R, primarily `mtcars` and
`airquality`, so the code can be copied and run without access to the data used
to develop the package. They are intended to demonstrate the API and figure
workflow, not to make substantive causal claims about those datasets.

A completely self-contained example that does **not** require PROCESS is:

```r
library(processdiagram)

fit <- fit_process_diagram(
  mtcars,
  x = "am",
  y = "mpg",
  m = "wt",
  model = 4,
  standardized = TRUE,
  boot = 0
)

plot(
  fit,
  labels = c(
    X = "Transmission\n(0 = automatic, 1 = manual)",
    M1 = "Weight",
    Y = "Fuel economy"
  )
)
```

## Installation

`processdiagram` is not on CRAN. Install the current release from GitHub with:

```r
install.packages("remotes")
remotes::install_github("freejas/processdiagram")
```

Then load it normally:

```r
library(processdiagram)
```

To install from a local clone or downloaded source directory instead:

```r
remotes::install_local("path/to/processdiagram")
```

## 1. Preferred workflow: run PROCESS and build the figure

Download PROCESS from Andrew F. Hayes's [official download page](https://afhayes.com/download.html), source it normally so that `process()` exists in the R session, then use `process_for_diagram()` instead of calling `process()` directly for analyses that will become figures. The PROCESS examples use `boot = 5000` to show the manuscript workflow; while experimenting with the API, use `boot = 0` for much faster runs.

### Model 4

```r
fit <- process_for_diagram(
  mtcars,
  x = "am",
  y = "mpg",
  m = "wt",
  model = 4,
  standardized = TRUE,
  boot = 5000
)

plot(
  fit,
  labels = c(
    X = "Transmission\n(0 = automatic, 1 = manual)",
    M1 = "Weight",
    Y = "Fuel economy"
  )
)
```

If `seed` is omitted, `processdiagram` generates one once, passes it explicitly to PROCESS, and stores it in the returned object:

```r
fit$provenance$seed
```

This prevents a future rerun from silently using a different bootstrap seed.

### Model 6

```r
fit <- process_for_diagram(
  mtcars,
  x = "am",
  y = "mpg",
  m = c("wt", "qsec"),
  model = 6,
  standardized = TRUE,
  boot = 5000
)

plot(
  fit,
  labels = c(
    X = "Transmission\n(0 = automatic, 1 = manual)",
    M1 = "Weight",
    M2 = "Quarter-mile time",
    Y = "Fuel economy"
  ),
  show_indirect = FALSE
)
```

### Model 81 with a dichotomous outcome

```r
aq <- transform(
  airquality,
  EvenDay = as.integer(Day %% 2 == 0)
)

fit <- process_for_diagram(
  aq,
  x = "Month",
  y = "EvenDay",
  m = c("Temp", "Wind", "Solar.R"),
  model = 81,
  standardized = TRUE,
  boot = 5000
)
```

PROCESS disables `stand = 1` when Y is dichotomous. `processdiagram` detects this behavior and stores the effective result accordingly:

```r
fit$provenance$standardized_requested
fit$provenance$standardized_effective
fit$provenance$outcome_type
```

For such a fit, omit `standardized = TRUE` when plotting. Paths predicting Y are logistic-regression coefficients in log-odds units.

```r
plot(
  fit,
  labels = c(
    X = "Month",
    M1 = "Temperature",
    M2 = "Wind",
    M3 = "Solar radiation",
    Y = "Even-numbered day\n(0 = No, 1 = Yes)"
  ),
  show_indirect = FALSE
)
```

## 2. Custom `bmatrix` models

A custom PROCESS lower-triangular `bmatrix` can be supplied directly:

```r
fit <- process_for_diagram(
  mtcars,
  x = "am",
  y = "mpg",
  m = c("wt", "hp", "qsec"),
  bmatrix = c(1, 1, 0, 1, 0, 0, 1, 1, 1, 1),
  standardized = TRUE,
  boot = 5000
)
```

The statistical mediator identities remain `M1`, `M2`, and `M3` in the order supplied to `m`: here `M1 = wt`, `M2 = hp`, and `M3 = qsec`. `mediator_order` changes only their visual positions. To avoid accidentally treating `M1`, `M2`, and `M3` as top/middle/bottom positions, variable names are recommended whenever mediators are reordered:

```r
plot(
  fit,
  mediator_order = c("hp", "wt", "qsec"),
  labels = c(
    am = "Transmission",
    wt = "Weight",
    hp = "Horsepower",
    qsec = "Quarter-mile time",
    mpg = "Fuel economy"
  )
)
```

Here Horsepower is drawn at the top, but it remains `M2` statistically and keeps the `am -> hp` and `hp -> mpg` coefficients. Changing `mediator_order` never changes the fitted model or indirect-effect identities. You can inspect the role mapping at any time with `fit$spec$roles`.

## 3. Import a PROCESS `save = 2` result

If a PROCESS analysis was saved with `save = 2` and written to CSV, use `import_process_save2()`. PROCESS 5.0 is supported directly, and compatible 4.1 files remain supported for older analyses. A small PROCESS-5-format example file is bundled with the package, so this example is runnable as written:

```r
example_file <- system.file(
  "extdata", "process5_model4_save2.csv", package = "processdiagram"
)

imported_fit <- import_process_save2(
  example_file,
  x = "X",
  y = "Y",
  m = "M",
  model = 4
)

imported_fit$validation$ok
imported_fit$provenance$process_version
plot(imported_fit)
```

For your own file, replace `example_file` with its path and supply the original model specification.

The importer does **not** rerun PROCESS. Bootstrap SEs and confidence intervals are read from the saved file and remain authoritative, even when the original random seed is no longer available.

Import provenance uses `save2_*` names (for example, `save2_format` and
`save2_indirect_contrast_rows_per_block`) because `save = 2` import is a current
workflow for PROCESS 5 as well as a backward-compatibility workflow for 4.1 files.

For a binary-Y analysis, identify it explicitly because an unstandardized flattened `save = 2` matrix cannot reliably reveal whether the final equation was OLS or logistic:

```r
saved_fit <- import_process_save2(
  "path/to/process_save2.csv",
  x = "X",
  y = "Y_binary",
  m = c("M1", "M2", "M3"),
  model = 81,
  outcome_type = "binary"
)
```

The importer internally recomputes indirect-effect point estimates from the recovered paths and checks them against the saved PROCESS indirect-effect rows. If the supplied model specification does not fit the file structure, strict mode stops rather than guessing.

## 4. Native engine

`fit_process_diagram()` is an independent implementation used for validation and for supported analyses that do not need PROCESS itself:

```r
fit <- fit_process_diagram(
  mtcars,
  x = "am",
  y = "mpg",
  m = "wt",
  model = 4,
  standardized = TRUE,
  boot = 5000,
  seed = 12345
)
```

When a manuscript figure is explicitly intended to report a PROCESS analysis, prefer `process_for_diagram()` so the figure values come from PROCESS.

## 5. Inspect the returned object

A `process_diagram_fit` contains the diagram-ready estimates plus provenance and validation information:

```r
fit$paths
fit$indirect
fit$validation
fit$provenance
```

For fits created by `process_for_diagram()`, the original PROCESS return object and captured console output are also retained:

```r
fit$process_result
fit$process_output
```

Useful convenience output:

```r
print(fit)
indirect_effects(fit)
```

## 6. Figure labels and layouts

Display labels can be supplied by role (`X`, `M1`, ..., `Y`) or by variable name:

```r
labels <- c(
  X = "Transmission\n(0 = automatic, 1 = manual)",
  M1 = "Weight",
  M2 = "Quarter-mile time",
  Y = "Fuel economy"
)

plot(fit, labels = labels)
```

`layout = "auto"` is the recommended default. It currently selects dedicated manuscript layouts for:

- one-mediator Model 4
- two-mediator Model 6
- three-mediator Model 81

Other supported structures use the generic layout.

Available explicit presets are:

```r
c("generic", "model4_classic", "model6_classic", "model81_classic")
```

## 7. Significance and styling

By default:

- significant paths are solid
- nonsignificant paths are dash-dot
- stars use `* p < .05`, `** p < .01`, and `*** p < .001`

Customize the appearance with `process_theme()`. Text, line work, and node fills can all be set independently:

```r
my_theme <- process_theme(
  text_color = "#333333",       # node labels + coefficient text
  line_color = "#555555",       # arrows + node borders
  node_fill = "#F7F7F7",
  font_family = "sans",
  node_font_size = 9,
  coefficient_font_size = 8.5,
  arrow_width = 1
)

plot(fit, theme = my_theme)
```

For variable-specific fills, give `node_fill` a named vector. Names may be PROCESS roles (`X`, `M1`, ..., `Y`) or the actual variable names. These names are resolved against the fitted model itself (not the current layout), so colors stay attached to the variable even if mediators are visually reordered:

```r
my_theme <- process_theme(
  text_color = "#222222",
  line_color = "#444444",
  node_fill = c(
    am   = "#D9EAF7",
    wt   = "#E2F0D9",
    qsec = "#FFF2CC",
    mpg  = "#E4DFEC"
  )
)

plot(fit, theme = my_theme)
```

`node_border` and `node_text` also accept named vectors when individual nodes need different border or text colors. Use `.default` as a fallback when only some nodes are overridden:

```r
process_theme(
  node_fill = c(.default = "white", M1 = "#DDEBF7"),
  node_border = c(.default = "#555555", Y = "#000000"),
  node_text = c(.default = "#222222", Y = "#7F0000")
)
```

If you want separate global colors rather than the convenience shorthands, use `node_text`, `coefficient_color`, `node_border`, and `arrow_color` directly. Explicit settings take precedence over `text_color` and `line_color`. Standard R color names and hex colors are accepted.

## 8. Manuscript-ready figure notes

Generate a note from the same validated values used in the figure:

```r
figure_note(
  fit,
  labels = labels,
  mediator_order = c("M1", "M2")
)
```

For standardized fits, standardized indirect effects are paired with their standardized bootstrap confidence intervals. For binary-Y fits, the note identifies the final logistic paths as log-odds coefficients.

## 9. Publication-quality export

SVG and PDF are vector formats and are preferred when the journal workflow accepts them.

```r
save_process_diagram(
  fit,
  "Figure_1.svg",
  width = 7,
  height = 3.8,
  labels = labels,
  show_indirect = FALSE
)
```

For a 600-dpi TIFF:

```r
save_process_diagram(
  fit,
  "Figure_1.tiff",
  width = 7,
  height = 3.8,
  dpi = 600,
  labels = labels,
  show_indirect = FALSE,
  note_file = TRUE
)
```

With `note_file = TRUE`, a companion `Figure_1_note.txt` is written beside the figure.

## 10. Validation modes for `process_for_diagram()`

The recommended default is:

```r
validation = "paths"
```

This independently refits the supported path equations and checks indirect-effect point estimates without unnecessarily repeating the full bootstrap.

For auditing/development:

```r
validation = "full"
```

This also repeats the bootstrap using the native engine.

To skip independent validation entirely:

```r
validation = "none"
```

For manuscript work, keeping the default `"paths"` validation is recommended.

## 11. Common issues

### "Standardized coefficients are unavailable"

If Y is dichotomous, PROCESS disables standardization. Plot the fit without forcing `standardized = TRUE`.

### A saved PROCESS file does not match the supplied model

Make sure `x`, `y`, `m`, `model`, and/or `bmatrix` describe the **original PROCESS call**, not merely the figure you remember. The flattened `save = 2` format does not store variable names or the model specification.

### Historical bootstrap CIs differ from a new rerun

That is expected when the original seed is unknown. `import_process_save2()` preserves the historical CIs instead of regenerating them.

### SVG/TIFF support is unavailable

Install the optional export packages:

```r
install.packages(c("svglite", "ragg"))
```

## More examples

Copy/paste examples are installed under `inst/examples/` in the source package:

- `00_native_quick_start.R`
- `01_model4.R`
- `02_model6.R`
- `03_model81_binary_y.R`
- `04_custom_bmatrix.R`
- `05_save2_import.R`

A longer guide is available at `inst/guides/getting-started.md`.

## Citation

If you use `processdiagram` in published work, run:

```r
citation("processdiagram")
```

Recommended citation:

> Freeman, J. D. (2026). *processdiagram: Publication-Quality Path Diagrams for PROCESS-Style Mediation Models* (R package version 0.1.0). https://github.com/freejas/processdiagram

The package is independent software and does not contain or redistribute PROCESS source code. Cite PROCESS separately as appropriate for the analysis you report.
