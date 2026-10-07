# Layouts ------------------------------------------------------------------

.pd_normalize_mediator_order <- function(spec, mediator_order = NULL) {
  roles <- paste0("M", seq_along(spec$m))
  if (is.null(mediator_order)) return(roles)

  mediator_order <- as.character(mediator_order)
  out <- character(length(mediator_order))
  for (i in seq_along(mediator_order)) {
    z <- mediator_order[i]
    if (z %in% roles) {
      out[i] <- z
    } else if (z %in% spec$m) {
      out[i] <- unname(spec$roles[[z]])
    } else {
      .pd_stop("Unknown mediator in mediator_order: ", z,
               ". Use mediator role names (M1, M2, ...) or mediator variable names.")
    }
  }
  if (length(out) != length(roles) || !setequal(out, roles)) {
    .pd_stop("mediator_order must contain each mediator exactly once.")
  }
  out
}

.pd_layout <- function(spec, mediator_order = NULL, layout = c("auto", "generic", "model4_classic", "model6_classic", "model81_classic")) {
  layout <- match.arg(layout)
  k <- length(spec$m)
  model <- spec$model
  roles <- unname(spec$roles)
  display_m <- .pd_normalize_mediator_order(spec, mediator_order)

  if (layout == "auto") {
    if (identical(model, 4L) && k == 1L) {
      layout <- "model4_classic"
    } else if (identical(model, 6L) && k == 2L) {
      layout <- "model6_classic"
    } else if (identical(model, 81L) && k == 3L) {
      layout <- "model81_classic"
    } else {
      layout <- "generic"
    }
  }

  if (identical(layout, "model4_classic") && identical(model, 4L) && k == 1L) {
    nodes <- data.frame(role = c("X", "M1", "Y"), x = c(.14, .50, .86),
                        y = c(.28, .76, .28), w = c(.20, .18, .18), h = c(.17, .17, .17))
  } else if (identical(model, 4L) && k == 1L) {
    nodes <- data.frame(role = c("X", "M1", "Y"), x = c(.13, .50, .87),
                        y = c(.50, .76, .50), w = c(.20, .18, .18), h = c(.17, .17, .17))
  } else if (identical(layout, "model6_classic") && identical(model, 6L) && k == 2L) {
    # Compact manuscript layout: the two rows are deliberately closer together
    # than Model 4 so the serial network reads as a low, wide figure rather
    # than a tall diamond.  M1/M2 are also separated slightly more horizontally
    # to leave a clean top path and open crossing space in the centre.
    nodes <- data.frame(
      role = c("X", "M1", "M2", "Y"),
      x = c(.14, .35, .65, .86),
      y = c(.31, .69, .69, .31),
      w = c(.20, .18, .18, .18),
      h = c(.17, .17, .17, .17)
    )
  } else if (identical(model, 6L) && k == 2L) {
    nodes <- data.frame(role = c("X", "M1", "M2", "Y"), x = c(.11, .38, .64, .89),
                        y = c(.50, .74, .74, .50), w = c(.19, .17, .17, .17), h = c(.17, .17, .17, .17))
  } else if (identical(layout, "model81_classic") && identical(model, 81L) && k == 3L) {
    # Manuscript layout for PROCESS Model 81. Compared with the compact preset,
    # this version adds more horizontal breathing room between X and M1 and more
    # vertical separation between M2 and M3 so the figure more closely matches
    # the original manuscript composition.
    nodes <- data.frame(
      role = c("X", "M1", "M2", "M3", "Y"),
      x = c(.11, .43, .68, .66, .91),
      y = c(.52, .52, .83, .25, .52),
      w = c(.19, .16, .16, .17, .16),
      h = c(.14, .14, .14, .14, .14)
    )
  } else if (identical(model, 81L) && k == 3L) {
    nodes <- data.frame(role = c("X", "M1", "M2", "M3", "Y"),
                        x = c(.11, .40, .60, .60, .89),
                        y = c(.50, .50, .80, .20, .50),
                        w = c(.19, .17, .17, .17, .17), h = rep(.17, 5))
  } else {
    # Generic/parallel layout.  The visual order can be changed without altering
    # the statistical identity of M1...Mk.
    if (k == 1L) {
      my <- .72
    } else if (k == 2L) {
      my <- c(.73, .27)
    } else if (k == 3L) {
      my <- c(.80, .50, .20)
    } else {
      my <- seq(.84, .18, length.out = k)
    }
    nodes <- data.frame(
      role = c("X", display_m, "Y"),
      x = c(.11, rep(.51, k), .90),
      y = c(.50, my, .50),
      w = c(.20, rep(.17, k), .17),
      h = rep(.17, k + 2L)
    )
  }
  nodes$variable <- spec$nodes[match(nodes$role, roles)]
  nodes
}

.pd_edge_points <- function(a, b) {
  dx <- b$x - a$x
  dy <- b$y - a$y
  if (abs(dx) < 1e-10 && abs(dy) < 1e-10) return(c(a$x, a$y, b$x, b$y))
  tx <- if (abs(dx) < 1e-10) Inf else (a$w / 2) / abs(dx)
  ty <- if (abs(dy) < 1e-10) Inf else (a$h / 2) / abs(dy)
  ta <- min(tx, ty)
  sx <- a$x + ta * dx
  sy <- a$y + ta * dy
  tx2 <- if (abs(dx) < 1e-10) Inf else (b$w / 2) / abs(dx)
  ty2 <- if (abs(dy) < 1e-10) Inf else (b$h / 2) / abs(dy)
  tb <- min(tx2, ty2)
  ex <- b$x - tb * dx
  ey <- b$y - tb * dy
  c(sx, sy, ex, ey)
}
