# Getting started with processdiagram

This guide is a practical companion to the package help and README.


## About the examples

Examples in this guide use datasets distributed with R so they can be copied and
run on any standard R installation. They are demonstrations of the package API,
not substantive causal analyses of the example datasets.

PROCESS is not redistributed with this package. Obtain it from Andrew F. Hayes's official download page: <https://afhayes.com/download.html>. `process_for_diagram()` supports PROCESS for R 4.1 and 5.0 for the mediation structures currently supported by the package.

If PROCESS itself is not available, start with the native engine:

```r
library(processdiagram)

fit <- fit_process_diagram(
  mtcars, x = "am", y = "mpg", m = "wt", model = 4,
  standardized = TRUE, boot = 0
)
plot(fit)
```

## The basic object lifecycle

Most work follows the same sequence:

```r
fit <- process_for_diagram(...)
fit$validation$ok
plot(fit, ...)
figure_note(fit, ...)
save_process_diagram(fit, ...)
```

For analyses available as saved PROCESS `save = 2` output, replace the first line with `import_process_save2(...)`.
For a PROCESS-independent analysis, replace it with `fit_process_diagram(...)`.

## New PROCESS analyses

Use `process_for_diagram()` whenever the figure is meant to reproduce a PROCESS
analysis. It asks PROCESS for `save = 2`, captures the console output, stores the
seed, validates the supported structure independently, and uses the PROCESS values
in the final object. PROCESS 5.0 changed the standardized-output tables; the wrapper
recognizes both the 4.1 and 5.0 formats automatically.

```r
fit <- process_for_diagram(
  mtcars,
  x = "am",
  y = "mpg",
  m = c("wt", "qsec"),
  model = 6,
  standardized = TRUE,
  boot = 5000,
  validation = "paths"
)
```

A useful audit sequence is:

```r
fit$validation$ok
fit$paths
fit$indirect
fit$provenance
```

## Plotting

```r
plot(
  fit,
  labels = c(
    X = "Transmission\n(0 = automatic, 1 = manual)",
    M1 = "Weight",
    M2 = "Quarter-mile time",
    Y = "Fuel economy"
  ),
  digits = 2,
  stars = TRUE,
  show_indirect = FALSE,
  layout = "auto"
)
```

The role names `X`, `M1`, ..., `Y` refer to fixed statistical roles in the fitted
model. In particular, `M1`, `M2`, and so on are assigned from the order supplied
to `m`; they never mean "top mediator" or "middle mediator." `mediator_order`
may change visual position without changing those identities. When reordering
mediators, naming `labels` and `mediator_order` with the original variable names
is recommended because it makes that mapping explicit. Use `fit$spec$roles` to
inspect the role-to-variable mapping.

## Binary Y

When PROCESS identifies Y as dichotomous it disables `stand = 1`. Therefore a
binary-Y fit may have `standardized_requested = TRUE` but
`standardized_effective = FALSE`. Do not force `standardized = TRUE` when drawing
such a fit.

```r
fit$provenance$standardized_requested
fit$provenance$standardized_effective
fit$provenance$outcome_type
```

Paths into the binary outcome are reported in the log-odds metric.

## PROCESS save=2 files

`import_process_save2()` supports PROCESS for R 5.0 and compatible 4.1 `save = 2` matrices for the mediation structures supported by processdiagram. PROCESS 5 standardized matrices are detected from their saved scale-free/standardized association blocks. For unstandardized files, the supported 4.1 and 5.0 layouts are structurally identical; set `process_version = "5.0"` only when you need explicit version provenance.

A flattened `save = 2` object is not self-describing. Supply the original model
specification explicitly:

```r
example_file <- system.file(
  "extdata", "process5_model4_save2.csv", package = "processdiagram"
)

saved <- import_process_save2(
  example_file,
  x = "X",
  y = "Y",
  m = "M",
  model = 4
)
```

For a binary outcome, the same call additionally needs `outcome_type = "binary"`. The following is a template for a user's own saved file:

```r
saved <- import_process_save2(
  "process_save2.csv",
  x = "X",
  y = "Y",
  m = c("M1", "M2", "M3"),
  model = 81,
  outcome_type = "binary"
)
```

The saved bootstrap values are retained. The importer validates the indirect
point effects against products of the recovered path coefficients.

## Figure notes

```r
figure_note(
  fit,
  labels = c(
    X = "Transmission", M1 = "Weight",
    M2 = "Quarter-mile time", Y = "Fuel economy"
  )
)
```

Use the same `labels`, `mediator_order`, `standardized`, and `digits` arguments as
the figure when you want the note and image to stay synchronized.

## Export

Vector:

```r
save_process_diagram(fit, "figure.svg", width = 7, height = 3.8)
```

High-resolution raster:

```r
save_process_diagram(
  fit,
  "figure.tiff",
  width = 7,
  height = 3.8,
  dpi = 600,
  note_file = TRUE
)
```

## What to archive with a manuscript

For reproducibility, keep at least:

- the analysis script containing the `process_for_diagram()` call;
- the fitted object's provenance/seed or the original PROCESS output;
- any `save = 2` CSV used by the importer;
- the exact labels/layout/export call used to create the submitted figure.

For a new PROCESS analysis, `fit$provenance` contains the explicit seed and package/R
metadata used by `processdiagram`.


## Styling

Use `process_theme()` to control figure text, arrows/borders, and node fills. The convenience arguments `text_color` and `line_color` change the figure text and line work together:

```r
th <- process_theme(
  text_color = "#333333",
  line_color = "#555555",
  node_fill = "#F7F7F7"
)
plot(fit, theme = th)
```

For variable-specific formatting, `node_fill`, `node_border`, and `node_text` may be named by statistical role (`X`, `M1`, ..., `Y`) or by the underlying variable name. The color stays attached to the statistical variable even if mediators are visually reordered. `.default` supplies a fallback for nodes that are not explicitly named.

```r
th <- process_theme(
  node_fill = c(
    .default = "white",
    X = "#D9EAF7",
    M1 = "#E2F0D9",
    Y = "#FCE4D6"
  ),
  node_border = c(.default = "#555555", Y = "#000000"),
  text_color = "#222222"
)
plot(fit, theme = th)
```

For separate global settings, use `node_text`, `coefficient_color`, `node_border`, and `arrow_color` directly. Explicit settings take precedence over `text_color` and `line_color`.
