# Drawing ------------------------------------------------------------------

.pd_variable_label_map <- function(fit, labels = NULL) {
  vars <- fit$spec$nodes
  roles <- unname(fit$spec$roles)
  role_to_var <- stats::setNames(vars, roles)
  out <- stats::setNames(vars, vars)
  if (is.null(labels)) return(out)

  if (is.function(labels)) {
    .pd_stop(
      "`labels` must be a named character vector or list, not a function. ",
      "If you wrote `labels = labels`, define that object first or supply the ",
      "named vector directly."
    )
  }
  if (!is.character(labels) && !is.list(labels)) {
    .pd_stop("`labels` must be a named character vector or list.")
  }

  labels <- unlist(labels, use.names = TRUE)
  if (!is.character(labels)) labels <- as.character(labels)
  if (is.null(names(labels)) || any(!nzchar(names(labels)))) {
    .pd_stop("`labels` must be named. Use role names (X, M1, ..., Y) or variable names.")
  }

  valid_names <- c(roles, vars)
  unknown <- setdiff(names(labels), valid_names)
  if (length(unknown)) {
    .pd_stop(
      "Unknown name(s) in `labels`: ", paste(unknown, collapse = ", "),
      ". Use statistical role names (X, M1, ..., Y) or model variable names."
    )
  }

  # Resolve every public-facing role alias to the underlying model variable
  # before applying display text.  From this point forward node labels and path
  # coefficients share the same identity key (the variable name), so reordering
  # or relabeling mediators cannot detach a coefficient from its construct.
  assigned <- character()
  for (i in seq_along(labels)) {
    nm <- names(labels)[i]
    var <- if (nm %in% vars) nm else unname(role_to_var[[nm]])
    value <- labels[[i]]
    if (var %in% assigned && !identical(out[[var]], value)) {
      .pd_stop(
        "Conflicting labels were supplied for variable '", var,
        "' using both a role name and a variable name. Supply only one label."
      )
    }
    out[[var]] <- value
    assigned <- c(assigned, var)
  }
  out
}

# Backward-compatible internal role-keyed view used by note-generation code.
# Drawing itself uses .pd_variable_label_map() so identity is variable-based.
.pd_label_map <- function(fit, labels = NULL) {
  by_var <- .pd_variable_label_map(fit, labels)
  stats::setNames(unname(by_var[fit$spec$nodes]), unname(fit$spec$roles))
}

.pd_diagram_bindings <- function(fit, labels = NULL, mediator_order = NULL,
                                 layout = c("auto", "generic", "model4_classic", "model6_classic", "model81_classic")) {
  layout <- match.arg(layout)
  lay <- .pd_layout(fit$spec, mediator_order = mediator_order, layout = layout)
  by_var <- .pd_variable_label_map(fit, labels)
  lay$label <- unname(by_var[lay$variable])

  paths <- fit$paths
  paths$from_node <- match(paths$from, lay$variable)
  paths$to_node <- match(paths$to, lay$variable)
  if (anyNA(paths$from_node) || anyNA(paths$to_node)) {
    .pd_stop("Internal diagram binding error: one or more path variables have no node.")
  }

  list(nodes = lay, paths = paths)
}

.pd_path_value <- function(fit, from, to, standardized) {
  hit <- fit$paths$from == from & fit$paths$to == to
  if (sum(hit) != 1L) return(NULL)
  row <- fit$paths[hit, , drop = FALSE]
  est <- if (standardized) row$std_estimate else row$estimate
  list(estimate = est, p = row$p)
}

.pd_wrap_node_label <- function(x, width = 15L) {
  if (is.na(x) || grepl("\n", x, fixed = TRUE)) return(x)
  paste(strwrap(x, width = width), collapse = "\n")
}

.pd_angle <- function(x1, y1, x2, y2, device_size = NULL) {
  # npc coordinates are isotropic only on a square device.  Convert the line
  # vector to physical-device proportions before calculating its visible angle.
  if (is.null(device_size)) {
    device_size <- tryCatch(grDevices::dev.size("in"), error = function(e) c(1, 1))
  }
  if (length(device_size) != 2L || any(!is.finite(device_size)) || any(device_size <= 0)) {
    device_size <- c(1, 1)
  }
  dx <- (x2 - x1) * device_size[1]
  dy <- (y2 - y1) * device_size[2]
  atan2(dy, dx) * 180 / pi
}

.pd_bezier_mid_y <- function(y0, yc1, yc2, y3) {
  # Cubic Bezier at t = .5. Kept separate so the direct-effect label can be
  # positioned relative to the curve itself rather than its off-canvas controls.
  .125 * y0 + .375 * yc1 + .375 * yc2 + .125 * y3
}

#' Draw a fitted process diagram
#'
#' @param fit A `process_diagram_fit` object.
#' @param labels Optional named vector/list using statistical role names (`X`,
#'   `M1`, ..., `Y`) or variable names. `M1`, `M2`, etc. always refer to the
#'   order supplied in `m`; they do not refer to visual top/middle/bottom
#'   positions after `mediator_order` is applied. Variable-name labels are
#'   recommended when reordering mediators.
#' @param standardized Plot standardized rather than unstandardized path estimates.
#' @param digits Number of coefficient decimals.
#' @param stars Add significance stars.
#' @param theme A `process_theme()` list.
#' @param show_indirect Show an indirect-effect footer when available.
#' @param mediator_order Optional visual ordering of mediators from top to bottom.
#'   Supply role names (for example `c("M2", "M1", "M3")`) or mediator variable
#'   names. This changes only display position, never the fitted model.
#' @param layout Visual layout preset. `"auto"` chooses an appropriate layout
#'   for the fitted model; currently this means dedicated presets for one-mediator
#'   Model 4, two-mediator Model 6, and three-mediator Model 81, with the generic
#'   manuscript layout otherwise.
#' @return Invisibly returns `fit` after drawing.
#' @seealso [process_theme()], [save_process_diagram()], [figure_note()]
#' @export
draw_process_diagram <- function(fit, labels = NULL, standardized = fit$standardized,
                                 digits = 2, stars = TRUE, theme = process_theme(),
                                 show_indirect = TRUE, mediator_order = NULL,
                                 layout = c("auto", "generic", "model4_classic", "model6_classic", "model81_classic")) {
  if (!inherits(fit, "process_diagram_fit")) .pd_stop("fit must be a process_diagram_fit object.")
  if (isTRUE(standardized) && !isTRUE(fit$standardized) &&
      (is.null(fit$paths$std_estimate) || all(is.na(fit$paths$std_estimate)))) {
    .pd_stop(
      "Standardized coefficients are unavailable for this fit. If Y is dichotomous, ",
      "PROCESS disables stand=1; draw the figure with standardized=FALSE (or omit ",
      "the argument and use the fit default)."
    )
  }
  layout <- match.arg(layout)
  bindings <- .pd_diagram_bindings(
    fit, labels = labels, mediator_order = mediator_order, layout = layout
  )
  lay <- bindings$nodes
  bound_paths <- bindings$paths

  # Resolve node appearance by statistical identity, not by visual row. This
  # lets users give X/M1/... or variable-specific colors without colors
  # detaching when mediators are visually reordered.
  node_fill_values <- .pd_node_style_values(
    theme$node_fill, lay, "node_fill", unmatched_default = "white", spec = fit$spec
  )
  node_border_values <- .pd_node_style_values(
    theme$node_border, lay, "node_border", unmatched_default = "black", spec = fit$spec
  )
  node_text_values <- .pd_node_style_values(
    theme$node_text, lay, "node_text", unmatched_default = "black", spec = fit$spec
  )

  if (identical(layout, "auto")) {
    layout <- if (identical(fit$spec$model, 4L) && length(fit$spec$m) == 1L) {
      "model4_classic"
    } else if (identical(fit$spec$model, 6L) && length(fit$spec$m) == 2L) {
      "model6_classic"
    } else if (identical(fit$spec$model, 81L) && length(fit$spec$m) == 3L) {
      "model81_classic"
    } else {
      "generic"
    }
  }
  grobs <- list()
  gi <- 0L

  # Edges first, so nodes sit cleanly on top.
  for (r in seq_len(nrow(bound_paths))) {
    pr <- bound_paths[r, ]
    a <- lay[pr$from_node, , drop = FALSE]
    b <- lay[pr$to_node, , drop = FALSE]
    if (!nrow(a) || !nrow(b)) next
    ep <- .pd_edge_points(a, b)
    lty <- if (is.na(pr$p) || pr$p < .05) theme$sig_lty else theme$nonsig_lty
    arr <- grid::arrow(type = "closed", length = grid::unit(theme$arrow_head_mm, "mm"))

    direct_xy <- identical(pr$from, fit$spec$x) && identical(pr$to, fit$spec$y)
    center_obstacle <- any(lay$role %in% paste0("M", seq_along(fit$spec$m)) & abs(lay$y - a$y) < .08)

    if (direct_xy && center_obstacle) {
      # Manuscript-style direct effect: route below the mediators rather than
      # crossing through the center of the diagram.
      sx <- a$x
      sy <- a$y - a$h / 2
      ex <- b$x
      ey <- b$y - b$h / 2
      # The control points intentionally sit below the viewport.  The curve
      # itself remains inside the page but bows low enough to clear the lowest
      # mediator box, matching the manuscript-style U-shaped direct path.
      curve_control_y <- if (identical(layout, "model81_classic")) -.10 else -.07
      gi <- gi + 1L
      grobs[[gi]] <- grid::bezierGrob(
        x = grid::unit(c(sx, sx + .015, ex - .015, ex), "npc"),
        y = grid::unit(c(sy, curve_control_y, curve_control_y, ey), "npc"),
        arrow = arr,
        gp = grid::gpar(col = theme$arrow_color, fill = theme$arrow_color,
                        lwd = theme$arrow_width, lty = lty)
      )
      lx <- (sx + ex) / 2
      curve_mid_y <- .pd_bezier_mid_y(sy, curve_control_y, curve_control_y, ey)
      ly <- if (identical(layout, "model81_classic")) {
        curve_mid_y + .032
      } else {
        curve_mid_y - .018
      }
      rot <- 0
    } else {
      gi <- gi + 1L
      grobs[[gi]] <- grid::segmentsGrob(
        x0 = grid::unit(ep[1], "npc"), y0 = grid::unit(ep[2], "npc"),
        x1 = grid::unit(ep[3], "npc"), y1 = grid::unit(ep[4], "npc"),
        arrow = arr,
        gp = grid::gpar(col = theme$arrow_color, fill = theme$arrow_color,
                        lwd = theme$arrow_width, lty = lty)
      )
      lx <- (ep[1] + ep[3]) / 2
      ly <- (ep[2] + ep[4]) / 2
      dx <- ep[3] - ep[1]
      dy <- ep[4] - ep[2]
      norm <- sqrt(dx^2 + dy^2)
      if (norm > 0) {
        lx <- lx - theme$label_offset * dy / norm
        ly <- ly + theme$label_offset * dx / norm
      }
      rot <- if (isTRUE(theme$rotate_coefficients)) .pd_angle(ep[1], ep[2], ep[3], ep[4]) else 0
    }

    val <- if (standardized) pr$std_estimate else pr$estimate
    gi <- gi + 1L
    grobs[[gi]] <- grid::textGrob(
      .pd_path_label(val, pr$p, digits = digits, stars = stars),
      x = grid::unit(lx, "npc"), y = grid::unit(ly, "npc"), rot = rot,
      gp = grid::gpar(col = theme$coefficient_color,
                      fontsize = theme$coefficient_font_size,
                      fontfamily = theme$font_family,
                      fontface = theme$coefficient_fontface)
    )
  }

  # Nodes.
  for (i in seq_len(nrow(lay))) {
    role <- lay$role[i]
    gi <- gi + 1L
    grobs[[gi]] <- grid::roundrectGrob(
      x = grid::unit(lay$x[i], "npc"), y = grid::unit(lay$y[i], "npc"),
      width = grid::unit(lay$w[i], "npc"), height = grid::unit(lay$h[i], "npc"),
      r = grid::unit(theme$node_radius, "inches"),
      gp = grid::gpar(fill = node_fill_values[i], col = node_border_values[i],
                      lwd = theme$node_border_width)
    )
    gi <- gi + 1L
    grobs[[gi]] <- grid::textGrob(
      .pd_wrap_node_label(lay$label[i]),
      x = grid::unit(lay$x[i], "npc"), y = grid::unit(lay$y[i], "npc"),
      gp = grid::gpar(col = node_text_values[i], fontsize = theme$node_font_size,
                      fontfamily = theme$font_family,
                      fontface = theme$node_fontface)
    )
  }

  # Optional indirect-effect text.
  if (show_indirect && nrow(fit$indirect)) {
    ind <- if (nrow(fit$indirect) > 1L && any(fit$indirect$id == "total")) {
      fit$indirect[fit$indirect$id == "total", , drop = FALSE]
    } else fit$indirect[1L, , drop = FALSE]
    if (standardized && "std_effect" %in% names(ind)) {
      eff <- ind$std_effect
      ci_lo <- if ("std_boot_llci" %in% names(ind)) ind$std_boot_llci else NA_real_
      ci_hi <- if ("std_boot_ulci" %in% names(ind)) ind$std_boot_ulci else NA_real_
    } else {
      eff <- ind$effect
      ci_lo <- ind$boot_llci
      ci_hi <- ind$boot_ulci
    }
    footer <- paste0("Indirect Effect: ", .pd_fmt(eff, digits))
    if (!is.na(ci_lo) && !is.na(ci_hi)) {
      footer <- paste0(
        footer, " (", round(fit$conf * 100), "% CI = ",
        .pd_fmt(ci_lo, digits, leading_zero = FALSE), ", ",
        .pd_fmt(ci_hi, digits, leading_zero = FALSE), ")"
      )
    }

    if (identical(layout, "model4_classic") && identical(fit$spec$model, 4L) && length(fit$spec$m) == 1L) {
      xnode <- lay[lay$role == "X", , drop = FALSE]
      ynode <- lay[lay$role == "Y", , drop = FALSE]
      tx <- (xnode$x + ynode$x) / 2
      ty <- xnode$y - 0.055
    } else {
      tx <- .50
      ty <- .018
    }

    gi <- gi + 1L
    grobs[[gi]] <- grid::textGrob(
      footer, x = grid::unit(tx, "npc"), y = grid::unit(ty, "npc"),
      gp = grid::gpar(col = theme$coefficient_color,
                      fontsize = theme$coefficient_font_size,
                      fontfamily = theme$font_family)
    )
  }

  grid::grid.draw(grid::gTree(children = do.call(grid::gList, grobs)))
  invisible(fit)
}

#' @export
plot.process_diagram_fit <- function(x, ...) {
  grid::grid.newpage()
  draw_process_diagram(x, ...)
  invisible(x)
}

#' Fit and immediately draw a PROCESS-style path diagram
#'
#' @param ... Arguments passed to `fit_process_diagram()`.
#' @param labels,theme,digits,stars,show_indirect,mediator_order,layout Drawing options.
#' @return Invisibly returns the fitted `process_diagram_fit` object.
#' @seealso [fit_process_diagram()], [draw_process_diagram()]
#' @examples
#' process_diagram(
#'   mtcars, x = "am", y = "mpg", m = "wt", model = 4, boot = 0,
#'   labels = c(X = "Transmission", M1 = "Weight", Y = "Fuel economy")
#' )
#' @export
process_diagram <- function(..., labels = NULL, theme = process_theme(), digits = 2,
                            stars = TRUE, show_indirect = TRUE,
                            mediator_order = NULL,
                            layout = c("auto", "generic", "model4_classic", "model6_classic", "model81_classic")) {
  fit <- fit_process_diagram(...)
  plot(fit, labels = labels, theme = theme, digits = digits, stars = stars,
       show_indirect = show_indirect, mediator_order = mediator_order,
       layout = layout)
  invisible(fit)
}
