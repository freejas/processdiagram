#' Save a process diagram
#'
#' SVG and PDF are preferred for publication because they are vector formats.
#' TIFF/PNG are rendered at the requested DPI.
#'
#' @param fit A `process_diagram_fit` object.
#' @param filename Output path ending in .svg, .pdf, .png, .tif, or .tiff.
#' @param width,height Physical dimensions in inches.
#' @param dpi Raster resolution for PNG/TIFF.
#' @param note_file If `TRUE`, also save a manuscript-ready text note beside the
#'   image using the suffix `_note.txt`. A character path can be supplied instead.
#' @param note_labels Optional labels used only in the generated note.
#' @param ... Drawing options passed to `draw_process_diagram()`. Relevant label,
#'   standardization, digit, star, and mediator-order options are also reused by
#'   `figure_note()` when `note_file` is enabled.
#' @return Invisibly returns the normalized output filename.
#' @seealso [draw_process_diagram()], [figure_note()], [save_process_note()]
#' @examples
#' fit <- fit_process_diagram(
#'   mtcars, x = "am", y = "mpg", m = "wt", model = 4, boot = 0
#' )
#' outfile <- tempfile(fileext = ".pdf")
#' save_process_diagram(fit, outfile, width = 7, height = 3.8)
#' unlink(outfile)
#' @export
save_process_diagram <- function(fit, filename, width = 7, height = 4.5,
                                 dpi = 600, note_file = FALSE, note_labels = NULL, ...) {
  ext <- tolower(tools::file_ext(filename))
  if (ext == "svg") {
    if (!requireNamespace("svglite", quietly = TRUE)) {
      .pd_stop("Install svglite to save SVG files: install.packages('svglite')")
    }
    svglite::svglite(filename, width = width, height = height)
  } else if (ext == "pdf") {
    grDevices::pdf(filename, width = width, height = height, useDingbats = FALSE)
  } else if (ext == "png") {
    if (!requireNamespace("ragg", quietly = TRUE)) {
      .pd_stop("Install ragg to save high-resolution PNG files: install.packages('ragg')")
    }
    ragg::agg_png(filename, width = width, height = height, units = "in", res = dpi)
  } else if (ext %in% c("tif", "tiff")) {
    if (!requireNamespace("ragg", quietly = TRUE)) {
      .pd_stop("Install ragg to save high-resolution TIFF files: install.packages('ragg')")
    }
    ragg::agg_tiff(filename, width = width, height = height, units = "in", res = dpi,
                   compression = "lzw")
  } else {
    .pd_stop("Unsupported extension. Use .svg, .pdf, .png, .tif, or .tiff")
  }
  dots <- list(...)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  do.call(draw_process_diagram, c(list(fit = fit), dots))

  if (isTRUE(note_file) || (is.character(note_file) && length(note_file) == 1L)) {
    note_path <- if (isTRUE(note_file)) {
      stem <- tools::file_path_sans_ext(basename(filename))
      file.path(dirname(filename), paste0(stem, "_note.txt"))
    } else {
      note_file
    }
    allowed <- c("labels", "standardized", "digits", "mediator_order", "stars")
    note_dots <- dots[intersect(names(dots), allowed)]
    do.call(
      save_process_note,
      c(list(fit = fit, filename = note_path, note_labels = note_labels), note_dots)
    )
  }

  invisible(normalizePath(filename, mustWork = FALSE))
}
