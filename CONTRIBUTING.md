# Contributing to processdiagram

Bug reports, reproducible examples, documentation improvements, and focused pull requests are welcome.

## Reporting a bug

Please include:

- the `processdiagram` version;
- your R version and operating system;
- whether the fit came from `process_for_diagram()`, `import_process_save2()`, or the native engine;
- the PROCESS version when relevant;
- a minimal reproducible example using public or simulated data whenever possible; and
- the full error message and `traceback()` output when available.

Do not attach confidential datasets or copyrighted PROCESS source code. A small synthetic example or a minimally necessary `save = 2` matrix is preferred.

## Pull requests

Before opening a pull request, run:

```r
devtools::document()
devtools::test()
devtools::check()
```

Please keep changes narrowly scoped and add regression tests for fixes that affect parsing, model identity, or rendered values.
