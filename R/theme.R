#' Diagram theme
#'
#' @param node_fill,node_border,node_text Node appearance. Each may be either a
#'   single color applied to every node, or a named character vector/list keyed
#'   by statistical roles (`X`, `M1`, ..., `Y`) and/or model variable names.
#'   Named mappings may include `.default` as the fallback for nodes not named.
#'   Without `.default`, unmatched nodes use the package default (`"white"` for
#'   fill and `"black"` for border/text).
#' @param arrow_color,coefficient_color Edge and coefficient appearance.
#' @param text_color Optional convenience color for all figure text. When
#'   supplied, it is used for `node_text` and `coefficient_color` unless either
#'   of those arguments is supplied explicitly.
#' @param line_color Optional convenience color for arrows and node borders.
#'   When supplied, it is used for `arrow_color` and `node_border` unless either
#'   of those arguments is supplied explicitly.
#' @param font_family Base font family. The default `"sans"` uses the graphics
#'   device's platform-safe sans-serif font and avoids Windows font-database
#'   warnings caused by hard-coding a font that is not registered with R.
#' @param font_size Backward-compatible base font size. Used when the more
#'   specific node/coefficient sizes are not supplied.
#' @param node_font_size,coefficient_font_size Separate node and coefficient sizes.
#' @param node_fontface,coefficient_fontface Font faces for node and coefficient text.
#' @param node_border_width,arrow_width Line widths.
#' @param sig_lty,nonsig_lty Line types for significant and nonsignificant paths.
#' @param arrow_head_mm Arrowhead length in millimetres.
#' @param rotate_coefficients Rotate straight-path coefficient labels to follow
#'   the visual angle of the corresponding arrow on the physical device.
#' @param label_offset Distance of path labels from straight arrows, in npc units.
#' @param node_radius Corner radius in inches. A physical unit keeps rounded
#'   corners visually stable across output sizes and devices.
#' @return A theme list for [draw_process_diagram()] and `plot()` methods for processdiagram fits.
#' @examples
#' process_theme()
#' process_theme(node_font_size = 10, coefficient_font_size = 9)
#' process_theme(
#'   text_color = "#333333",
#'   line_color = "#555555",
#'   node_fill = c(X = "#DDEBF7", M1 = "#E2F0D9", Y = "#FCE4D6")
#' )
#' @export
process_theme <- function(node_fill = "white", node_border = "black",
                          node_text = "black", arrow_color = "black",
                          coefficient_color = "black", font_family = "sans",
                          font_size = 9, node_font_size = NULL,
                          coefficient_font_size = NULL,
                          node_fontface = "bold", coefficient_fontface = "plain",
                          node_border_width = 0.8, arrow_width = 1,
                          sig_lty = "solid", nonsig_lty = "dotdash",
                          arrow_head_mm = 1.5, rotate_coefficients = TRUE,
                          label_offset = .017, node_radius = .10,
                          text_color = NULL, line_color = NULL) {
  node_text_missing <- missing(node_text)
  coefficient_color_missing <- missing(coefficient_color)
  node_border_missing <- missing(node_border)
  arrow_color_missing <- missing(arrow_color)

  if (!is.null(text_color)) {
    if (length(text_color) != 1L) {
      .pd_stop("`text_color` must be a single color. Use `node_text` for node-specific text colors.")
    }
    if (node_text_missing) node_text <- text_color
    if (coefficient_color_missing) coefficient_color <- text_color
  }
  if (!is.null(line_color)) {
    if (length(line_color) != 1L) {
      .pd_stop("`line_color` must be a single color. Use `node_border` for node-specific border colors.")
    }
    if (node_border_missing) node_border <- line_color
    if (arrow_color_missing) arrow_color <- line_color
  }

  if (is.null(node_font_size)) node_font_size <- font_size
  if (is.null(coefficient_font_size)) coefficient_font_size <- font_size * .95
  list(
    node_fill = node_fill,
    node_border = node_border,
    node_text = node_text,
    arrow_color = arrow_color,
    coefficient_color = coefficient_color,
    font_family = font_family,
    font_size = font_size,
    node_font_size = node_font_size,
    coefficient_font_size = coefficient_font_size,
    node_fontface = node_fontface,
    coefficient_fontface = coefficient_fontface,
    node_border_width = node_border_width,
    arrow_width = arrow_width,
    sig_lty = sig_lty,
    nonsig_lty = nonsig_lty,
    arrow_head_mm = arrow_head_mm,
    rotate_coefficients = rotate_coefficients,
    label_offset = label_offset,
    node_radius = node_radius,
    text_color = text_color,
    line_color = line_color
  )
}

.pd_node_style_values <- function(value, lay, setting,
                                  unmatched_default = "black", spec = NULL) {
  if (is.null(value)) value <- unmatched_default
  if (is.list(value)) value <- unlist(value, use.names = TRUE)
  if (!is.character(value)) value <- as.character(value)

  if (length(value) == 1L && (is.null(names(value)) || !nzchar(names(value)[1L]))) {
    return(rep(value, nrow(lay)))
  }

  if (is.null(names(value)) || any(!nzchar(names(value)))) {
    .pd_stop(
      "`", setting, "` must be either one color or a named vector/list. ",
      "Name node-specific colors with roles (X, M1, ..., Y) or variable names."
    )
  }

  # Resolve style keys against the statistical model specification first, not
  # against a particular visual layout.  This keeps node appearance attached to
  # the same variable identity as labels and path coefficients and prevents a
  # visual reorder/layout transformation from making a real model variable look
  # like an unknown styling key.
  if (!is.null(spec)) {
    canonical_vars <- c(spec$x, spec$m, spec$y)
    canonical_roles <- c("X", paste0("M", seq_along(spec$m)), "Y")
  } else {
    canonical_vars <- as.character(lay$variable)
    canonical_roles <- as.character(lay$role)
  }
  role_to_var <- stats::setNames(canonical_vars, canonical_roles)

  valid_names <- unique(c(".default", canonical_roles, canonical_vars))
  unknown <- setdiff(names(value), valid_names)
  if (length(unknown)) {
    .pd_stop(
      "Unknown name(s) in `", setting, "`: ", paste(unknown, collapse = ", "),
      ". Use node roles (X, M1, ..., Y), model variable names, or `.default`. ",
      "Available model variables: ", paste(canonical_vars, collapse = ", "), "."
    )
  }

  fallback <- if (".default" %in% names(value)) {
    value[[which(names(value) == ".default")[1L]]]
  } else {
    unmatched_default
  }

  by_var <- stats::setNames(rep(fallback, length(canonical_vars)), canonical_vars)
  assigned <- stats::setNames(rep(FALSE, length(canonical_vars)), canonical_vars)
  assigned_value <- stats::setNames(rep(NA_character_, length(canonical_vars)), canonical_vars)

  for (i in seq_along(value)) {
    nm <- names(value)[i]
    if (identical(nm, ".default")) next

    # As with labels, a literal model-variable name takes precedence over a
    # role alias if the two strings happen to collide.
    var <- if (nm %in% canonical_vars) nm else unname(role_to_var[[nm]])
    val <- value[[i]]

    if (isTRUE(assigned[[var]]) && !is.na(assigned_value[[var]]) &&
        !identical(assigned_value[[var]], val)) {
      .pd_stop(
        "Conflicting `", setting, "` colors were supplied for variable '", var,
        "' using both a role name and a variable name. Supply only one override."
      )
    }
    by_var[[var]] <- val
    assigned[[var]] <- TRUE
    assigned_value[[var]] <- val
  }

  missing_layout_vars <- setdiff(as.character(lay$variable), canonical_vars)
  if (length(missing_layout_vars)) {
    .pd_stop(
      "Internal diagram styling error: layout contains variable(s) not present ",
      "in the fitted model: ", paste(missing_layout_vars, collapse = ", "), "."
    )
  }

  unname(by_var[as.character(lay$variable)])
}
