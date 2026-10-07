# processdiagram 0.1.0

## Initial public release

- Creates publication-quality path diagrams and manuscript-ready figure notes for supported PROCESS-style mediation analyses.
- Supports PROCESS Models 4, 6, and 81 plus supported custom lower-triangular `bmatrix` specifications.
- `process_for_diagram()` supports current PROCESS for R 5.0 output and compatible PROCESS 4.1 output for existing analyses, while using PROCESS-reported values as authoritative figure values.
- `import_process_save2()` supports PROCESS 5.0 `save = 2` matrices and compatible 4.1 matrices, including standardized output, bootstrap diagnostics trailers, and indirect-effect contrast rows.
- Includes an independent native validation engine, explicit seed/provenance tracking, binary-outcome handling, and strict internal consistency checks.
- Supports dedicated layouts for Models 4, 6, and 81, generic supported layouts, mediator reordering with variable-identity binding, significance styling, and variable-specific node fill/border/text colors.
- Exports SVG, PDF, PNG, and TIFF and can generate manuscript-ready figure-note text from the same values used in the diagram.
- Includes GitHub Actions R CMD check, contribution guidance, package/repository citation metadata, and public-facing documentation.
