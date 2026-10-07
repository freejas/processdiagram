# Figure notes --------------------------------------------------------------

.pd_note_clean_label <- function(x) {
  if (length(x) == 0L || is.na(x)) return("")
  parts <- trimws(strsplit(as.character(x), "\n", fixed = TRUE)[[1L]])
  # Figure X labels often put coding on a separate parenthetical line.  That
  # information is useful inside the node but usually clutters a manuscript note.
  parts <- parts[!grepl("^\\(.*\\)$", parts)]
  paste(parts[nzchar(parts)], collapse = " ")
}

.pd_note_label_map <- function(fit, labels = NULL, note_labels = NULL) {
  out <- .pd_label_map(fit, labels)
  out <- stats::setNames(
    vapply(out, .pd_note_clean_label, character(1)),
    names(out)
  )

  if (!is.null(note_labels)) {
    note_labels <- unlist(note_labels)
    for (nm in names(note_labels)) {
      if (nm %in% names(out)) {
        out[[nm]] <- .pd_note_clean_label(note_labels[[nm]])
      } else if (nm %in% fit$spec$nodes) {
        role <- unname(fit$spec$roles[[nm]])
        out[[role]] <- .pd_note_clean_label(note_labels[[nm]])
      }
    }
  }
  out
}

.pd_indirect_display_rows <- function(fit, mediator_order = NULL) {
  if (is.null(fit$indirect) || !nrow(fit$indirect)) return(fit$indirect)
  specific <- fit$indirect[fit$indirect$id != "total", , drop = FALSE]
  if (!nrow(specific)) {
    return(fit$indirect[fit$indirect$id == "total", , drop = FALSE])
  }

  # For ordinary parallel mediation, make the prose follow the same top-to-bottom
  # mediator order as the figure rather than the statistical M1/M2/... order.
  mids <- lapply(strsplit(specific$path, " -> ", fixed = TRUE), function(z) {
    if (length(z) <= 2L) character() else z[2:(length(z) - 1L)]
  })
  if (all(lengths(mids) == 1L)) {
    display_roles <- .pd_normalize_mediator_order(fit$spec, mediator_order)
    row_roles <- vapply(mids, function(z) unname(fit$spec$roles[[z]]), character(1))
    ord <- match(row_roles, display_roles)
    if (all(!is.na(ord))) specific <- specific[order(ord), , drop = FALSE]
  }
  specific
}

.pd_indirect_note_piece <- function(row, fit, label_map, standardized, digits) {
  vars <- strsplit(row$path, " -> ", fixed = TRUE)[[1L]]
  mids <- if (length(vars) > 2L) vars[2:(length(vars) - 1L)] else character()
  mid_labels <- vapply(mids, function(v) {
    role <- unname(fit$spec$roles[[v]])
    if (!is.null(role) && role %in% names(label_map)) label_map[[role]] else v
  }, character(1))
  through <- if (length(mid_labels)) paste(mid_labels, collapse = " \u2192 ") else ""

  if (standardized) {
    eff <- if ("std_effect" %in% names(row)) row$std_effect else NA_real_
    lo <- if ("std_boot_llci" %in% names(row)) row$std_boot_llci else NA_real_
    hi <- if ("std_boot_ulci" %in% names(row)) row$std_boot_ulci else NA_real_
  } else {
    eff <- row$effect
    lo <- if ("boot_llci" %in% names(row)) row$boot_llci else NA_real_
    hi <- if ("boot_ulci" %in% names(row)) row$boot_ulci else NA_real_
  }

  txt <- if (nzchar(through)) paste0("through ", through, " = ") else "= "
  txt <- paste0(txt, .pd_fmt(eff, digits = digits))
  if (is.finite(lo) && is.finite(hi)) {
    txt <- paste0(
      txt, ", 95% CI [",
      .pd_fmt(lo, digits = digits, leading_zero = FALSE), ", ",
      .pd_fmt(hi, digits = digits, leading_zero = FALSE), "]"
    )
  }
  txt
}

#' Generate a manuscript-ready figure note
#'
#' Creates prose from the same validated values used by the diagram.  Specific
#' indirect effects follow the visual mediator order when `mediator_order` is
#' supplied.  For a single-mediator model, the duplicate TOTAL/specific indirect
#' effect is reported only once.
#'
#' @param fit A `process_diagram_fit` object.
#' @param labels Optional figure display labels, as in `draw_process_diagram()`.
#' @param note_labels Optional labels used only in the prose note.  These can
#'   override role names (`X`, `M1`, ..., `Y`) or variable names.
#' @param standardized Report standardized rather than unstandardized effects.
#' @param digits Number of decimals.
#' @param mediator_order Optional visual mediator order, as in
#'   `draw_process_diagram()`.
#' @param line_note Include the line-style significance sentence.
#' @param stars Include the significance-star legend.
#' @param note_prefix Prefix for the note.  The default is Markdown-friendly.
#' @return A single character string.
#' @seealso [save_process_note()], [save_process_diagram()]
#' @examples
#' fit <- fit_process_diagram(
#'   mtcars, x = "am", y = "mpg", m = "wt", model = 4, boot = 0
#' )
#' figure_note(
#'   fit,
#'   labels = c(X = "Transmission", M1 = "Weight", Y = "Fuel economy")
#' )
#' @export
figure_note <- function(fit, labels = NULL, note_labels = NULL,
                        standardized = fit$standardized, digits = 2,
                        mediator_order = NULL, line_note = TRUE, stars = TRUE,
                        note_prefix = "*Note:*") {
  if (!inherits(fit, "process_diagram_fit")) {
    .pd_stop("fit must be a process_diagram_fit object.")
  }

  labs <- .pd_note_label_map(fit, labels = labels, note_labels = note_labels)
  xlab <- labs[["X"]]
  ylab <- labs[["Y"]]
  if (standardized) {
    scale_sentence <- "Path coefficients are standardized."
  } else if (identical(fit$outcome_type, "binary")) {
    scale_sentence <- paste0(
      "Path coefficients are unstandardized. Paths predicting ", ylab,
      " are logistic regression coefficients expressed in log-odds."
    )
  } else {
    scale_sentence <- "Path coefficients are unstandardized."
  }

  pieces <- c(note_prefix, scale_sentence)
  rows <- .pd_indirect_display_rows(fit, mediator_order = mediator_order)

  if (!is.null(rows) && nrow(rows)) {
    if (nrow(rows) == 1L && rows$id[1L] != "total") {
      detail <- .pd_indirect_note_piece(rows[1L, , drop = FALSE], fit, labs,
                                        standardized = standardized, digits = digits)
      pieces <- c(
        pieces,
        paste0("Indirect effect from ", xlab, " to ", ylab, " ", detail, ".")
      )
    } else if (nrow(rows) == 1L && rows$id[1L] == "total") {
      row <- rows[1L, , drop = FALSE]
      if (standardized) {
        eff <- if ("std_effect" %in% names(row)) row$std_effect else NA_real_
        lo <- if ("std_boot_llci" %in% names(row)) row$std_boot_llci else NA_real_
        hi <- if ("std_boot_ulci" %in% names(row)) row$std_boot_ulci else NA_real_
      } else {
        eff <- row$effect
        lo <- row$boot_llci
        hi <- row$boot_ulci
      }
      txt <- paste0("Total indirect effect from ", xlab, " to ", ylab,
                    " = ", .pd_fmt(eff, digits = digits))
      if (is.finite(lo) && is.finite(hi)) {
        txt <- paste0(txt, ", 95% CI [",
                      .pd_fmt(lo, digits = digits, leading_zero = FALSE), ", ",
                      .pd_fmt(hi, digits = digits, leading_zero = FALSE), "]")
      }
      pieces <- c(pieces, paste0(txt, "."))
    } else {
      details <- vapply(seq_len(nrow(rows)), function(i) {
        .pd_indirect_note_piece(rows[i, , drop = FALSE], fit, labs,
                                standardized = standardized, digits = digits)
      }, character(1))
      pieces <- c(
        pieces,
        paste0("Indirect effects from ", xlab, " to ", ylab, ": ",
               paste(details, collapse = "; "), ".")
      )
    }
  }

  if (isTRUE(line_note)) {
    pieces <- c(
      pieces,
      "Solid lines represent significant paths; dashed lines represent non-significant paths."
    )
  }
  if (isTRUE(stars)) {
    pieces <- c(pieces, "* p < .05; ** p < .01; *** p < .001")
  }

  paste(pieces, collapse = " ")
}

#' Save a manuscript-ready figure note
#'
#' @param fit A `process_diagram_fit` object.
#' @param filename Destination `.txt` file.
#' @param ... Arguments passed to `figure_note()`.
#' @return Invisibly returns the normalized filename.
#' @export
save_process_note <- function(fit, filename, ...) {
  note <- figure_note(fit, ...)
  writeLines(note, con = filename, useBytes = TRUE)
  invisible(normalizePath(filename, mustWork = FALSE))
}
