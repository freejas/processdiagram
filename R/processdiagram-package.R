#' processdiagram: Publication-quality PROCESS-style mediation diagrams
#'
#' `processdiagram` creates publication-quality path diagrams and manuscript-ready
#' figure notes for supported PROCESS-style mediation models. It provides three
#' complementary workflows:
#'
#' * [process_for_diagram()] runs a user-supplied PROCESS function, preserves a
#'   seed, validates the supported path structure, and uses PROCESS's values as
#'   the authoritative figure values.
#' * [import_process_save2()] reconstructs a diagram-ready object from compatible
#'   PROCESS `save = 2` output without rerunning the analysis.
#' * [fit_process_diagram()] fits supported models with an independent native
#'   engine.
#'
#' The current statistical scope includes PROCESS Models 4, 6, and 81, custom
#' lower-triangular b-matrix specifications, OLS mediator equations, and supported
#' dichotomous-Y models with a logistic final equation.
#'
#' @section Recommended workflow:
#' For a new figure intended to report a PROCESS analysis, use
#' [process_for_diagram()], inspect `$validation`, then draw with [plot()] and
#' export with [save_process_diagram()]. Use [figure_note()] to generate the
#' corresponding manuscript figure note.
#'
#' @section Saved PROCESS analyses:
#' Use [import_process_save2()] when a PROCESS `save = 2`
#' result is available. The importer preserves the bootstrap values stored in that
#' file; it does not need the original bootstrap seed.
#'
#' @section Independence from PROCESS:
#' PROCESS source code is not bundled with or redistributed by this package.
#' `process_for_diagram()` expects the user to make a compatible `process()`
#' function available in the R session.
#'
#' @examples
#' fit <- fit_process_diagram(
#'   mtcars, x = "am", y = "mpg", m = "wt", model = 4, boot = 0
#' )
#' plot(
#'   fit,
#'   labels = c(X = "Transmission", M1 = "Weight", Y = "Fuel economy")
#' )
#'
#' @keywords internal
"_PACKAGE"
