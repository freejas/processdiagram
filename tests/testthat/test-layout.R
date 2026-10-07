test_that("mediator display order does not change statistical identity", {
  sp <- process_spec("X", "Y", c("M1var", "M2var", "M3var"),
                     bmatrix = c(1, 1, 0, 1, 0, 0, 1, 1, 1, 1))
  lay <- processdiagram:::.pd_layout(sp, mediator_order = c("M2", "M1", "M3"))
  mids <- lay[grepl("^M", lay$role), ]
  expect_equal(mids$role, c("M2", "M1", "M3"))
  expect_true(all(diff(mids$y) < 0))
  expect_equal(mids$variable, c("M2var", "M1var", "M3var"))
})

test_that("mediator order accepts variable names", {
  sp <- process_spec("X", "Y", c("A", "B", "C"),
                     bmatrix = c(1, 1, 0, 1, 0, 0, 1, 1, 1, 1))
  lay <- processdiagram:::.pd_layout(sp, mediator_order = c("B", "A", "C"))
  mids <- lay[grepl("^M", lay$role), ]
  expect_equal(mids$role, c("M2", "M1", "M3"))
})

test_that("visual angle accounts for device aspect ratio", {
  # A 45-degree npc line should be much shallower on a wide physical device.
  ang <- processdiagram:::.pd_angle(0, 0, 1, 1, device_size = c(7, 3.5))
  expect_equal(ang, atan2(3.5, 7) * 180 / pi, tolerance = 1e-10)
  expect_lt(ang, 45)
})

test_that("three-mediator generic layout leaves room for direct curve", {
  sp <- process_spec("X", "Y", c("A", "B", "C"),
                     bmatrix = c(1, 1, 0, 1, 0, 0, 1, 1, 1, 1))
  lay <- processdiagram:::.pd_layout(sp)
  m3 <- lay[lay$role == "M3", , drop = FALSE]
  expect_gte(m3$y, .20)
})

test_that("direct-effect Bezier midpoint helper is deterministic", {
  expect_equal(processdiagram:::.pd_bezier_mid_y(.415, -.07, -.07, .415), .05125)
})


test_that("three-mediator generic layout is vertically symmetric", {
  sp <- process_spec("X", "Y", c("A", "B", "C"),
                     bmatrix = c(1, 1, 0, 1, 0, 0, 1, 1, 1, 1))
  lay <- processdiagram:::.pd_layout(sp, mediator_order = c("M2", "M1", "M3"))
  mids <- lay[grepl("^M", lay$role), , drop = FALSE]
  expect_equal(mids$y, c(.80, .50, .20), tolerance = 1e-12)
  expect_equal(mids$y[1] - mids$y[2], mids$y[2] - mids$y[3], tolerance = 1e-12)
})


test_that("model4 classic layout uses triangular geometry", {
  sp <- process_spec("X", "Y", c("M"), model = 4)
  lay <- processdiagram:::.pd_layout(sp, layout = "auto")
  expect_equal(lay$role, c("X", "M1", "Y"))
  expect_equal(lay$x, c(.14, .50, .86), tolerance = 1e-12)
  expect_equal(lay$y, c(.28, .76, .28), tolerance = 1e-12)
})

test_that("model4 classic auto-activates only for one-mediator model 4", {
  sp <- process_spec("X", "Y", c("M1", "M2"), model = 4)
  lay <- processdiagram:::.pd_layout(sp, layout = "auto")
  mids <- lay[grepl("^M", lay$role), , drop = FALSE]
  expect_equal(mids$y, c(.73, .27), tolerance = 1e-12)
})


test_that("model6 classic layout uses manuscript serial geometry", {
  sp <- process_spec("X", "Y", c("M1var", "M2var"), model = 6)
  lay <- processdiagram:::.pd_layout(sp, layout = "auto")
  expect_equal(lay$role, c("X", "M1", "M2", "Y"))
  expect_equal(lay$x, c(.14, .35, .65, .86), tolerance = 1e-12)
  expect_equal(lay$y, c(.31, .69, .69, .31), tolerance = 1e-12)
  expect_equal(lay$y[1], lay$y[4], tolerance = 1e-12)
  expect_equal(lay$y[2], lay$y[3], tolerance = 1e-12)
})

test_that("model6 classic crossing paths meet away from node boxes", {
  sp <- process_spec("X", "Y", c("M1var", "M2var"), model = 6)
  lay <- processdiagram:::.pd_layout(sp, layout = "model6_classic")
  x <- lay[lay$role == "X", , drop = FALSE]
  m1 <- lay[lay$role == "M1", , drop = FALSE]
  m2 <- lay[lay$role == "M2", , drop = FALSE]
  y <- lay[lay$role == "Y", , drop = FALSE]
  # The X->M2 and M1->Y diagonals cross around the open center of the figure.
  cross_x <- .50
  cross_y <- x$y + ((cross_x - x$x) / (m2$x - x$x)) * (m2$y - x$y)
  expect_gt(cross_y, x$y + x$h / 2)
  expect_lt(cross_y, m1$y - m1$h / 2)
})


test_that("model6 classic layout is compact and symmetric", {
  sp <- process_spec("X", "Y", c("M1var", "M2var"), model = 6)
  lay <- processdiagram:::.pd_layout(sp, layout = "auto")
  expect_equal(lay$role, c("X", "M1", "M2", "Y"))
  expect_equal(lay$x, c(.14, .35, .65, .86), tolerance = 1e-12)
  expect_equal(lay$y, c(.31, .69, .69, .31), tolerance = 1e-12)
  expect_equal(lay$y[2] - lay$y[1], .38, tolerance = 1e-12)
  expect_equal(lay$y[3] - lay$y[4], .38, tolerance = 1e-12)
})

test_that("model81 classic layout uses manuscript geometry", {
  sp <- process_spec("X", "Y", c("M1var", "M2var", "M3var"), model = 81)
  lay <- processdiagram:::.pd_layout(sp, layout = "auto")
  expect_equal(lay$role, c("X", "M1", "M2", "M3", "Y"))
  expect_equal(lay$x, c(.11, .43, .68, .66, .91), tolerance = 1e-12)
  expect_equal(lay$y, c(.52, .52, .83, .25, .52), tolerance = 1e-12)
  expect_equal(lay$y[1], lay$y[2], tolerance = 1e-12)
  expect_equal(lay$y[1], lay$y[5], tolerance = 1e-12)
  # M2 and M3 should occupy the same right-hand mediator column visually,
  # but a small horizontal stagger is intentional to improve path clearance.
  expect_lt(abs(lay$x[3] - lay$x[4]), .03)
})

test_that("model81 classic preserves M1 as central predecessor", {
  sp <- process_spec("X", "Y", c("M1var", "M2var", "M3var"), model = 81)
  lay <- processdiagram:::.pd_layout(sp, layout = "model81_classic")
  m1 <- lay[lay$role == "M1", , drop = FALSE]
  m2 <- lay[lay$role == "M2", , drop = FALSE]
  m3 <- lay[lay$role == "M3", , drop = FALSE]
  expect_lt(m1$x, m2$x)
  expect_gt(m2$x, m1$x)
  expect_gt(m3$x, m1$x)
  expect_gt(m2$y, m1$y)
  expect_lt(m3$y, m1$y)
})

test_that("model81 direct-effect curve clears lower mediator", {
  sp <- process_spec("X", "Y", c("M1var", "M2var", "M3var"), model = 81)
  lay <- processdiagram:::.pd_layout(sp, layout = "model81_classic")
  x <- lay[lay$role == "X", , drop = FALSE]
  y <- lay[lay$role == "Y", , drop = FALSE]
  m3 <- lay[lay$role == "M3", , drop = FALSE]
  sy <- x$y - x$h / 2
  ey <- y$y - y$h / 2
  # The curve is deliberately deeper for Model 81.  Check both the centre and
  # the vicinity of M3's right edge, where the older geometry could clip it.
  mid <- processdiagram:::.pd_bezier_mid_y(sy, -.10, -.10, ey)
  expect_lt(mid, m3$y - m3$h / 2)
  t_near_m3_right <- (m3$x + m3$w / 2 - x$x) / (y$x - x$x)
  curve_y <- (1 - t_near_m3_right)^3 * sy +
    3 * (1 - t_near_m3_right)^2 * t_near_m3_right * (-.10) +
    3 * (1 - t_near_m3_right) * t_near_m3_right^2 * (-.10) +
    t_near_m3_right^3 * ey
  expect_lt(curve_y, m3$y - m3$h / 2)
})

test_that("model81 classic has more breathing room than v0.0.16 geometry", {
  sp <- process_spec("X", "Y", c("M1var", "M2var", "M3var"), model = 81)
  lay <- processdiagram:::.pd_layout(sp, layout = "model81_classic")
  expect_gt(lay$x[2] - lay$x[1], .27)
  expect_gt(lay$x[3] - lay$x[2], .23)
  expect_lte(max(lay$w), .19)
  expect_equal(lay$w[lay$role == "X"], .19, tolerance = 1e-12)
})

test_that("variable-name labels stay attached to mediator identity after reordering", {
  sp <- process_spec(
    "Group_num", "Pos_Empathy", c("rmob_total", "Ind", "Inter"),
    bmatrix = c(1, 1, 0, 1, 0, 0, 1, 1, 1, 1)
  )
  fit <- list(spec = sp)
  class(fit) <- "process_diagram_fit"

  lab <- processdiagram:::.pd_label_map(
    fit,
    c(
      Group_num = "Country",
      rmob_total = "Relational Mobility",
      Ind = "Independence",
      Inter = "Interdependence",
      Pos_Empathy = "Positive Empathy"
    )
  )
  lay <- processdiagram:::.pd_layout(
    sp,
    mediator_order = c("Ind", "rmob_total", "Inter")
  )
  mids <- lay[grepl("^M", lay$role), , drop = FALSE]

  expect_equal(mids$variable, c("Ind", "rmob_total", "Inter"))
  expect_equal(unname(lab[mids$role]), c("Independence", "Relational Mobility", "Interdependence"))
})

test_that("labels rejects accidentally passing base labels function", {
  sp <- process_spec("X", "Y", "M", model = 4)
  fit <- list(spec = sp)
  class(fit) <- "process_diagram_fit"
  expect_error(
    processdiagram:::.pd_label_map(fit, labels),
    "must be a named character vector or list, not a function"
  )
})


test_that("role labels and coefficients remain bound to the same mediator variable", {
  sp <- process_spec(
    "Group_num", "Pos_Empathy", c("rmob_total", "Ind", "Inter"),
    bmatrix = c(1, 1, 0, 1, 0, 0, 1, 1, 1, 1)
  )
  fit <- list(
    spec = sp,
    paths = data.frame(
      id = paste0("p", 1:7),
      from = c("Group_num", "Group_num", "Group_num", "Group_num", "rmob_total", "Ind", "Inter"),
      to = c("rmob_total", "Ind", "Inter", "Pos_Empathy", "Pos_Empathy", "Pos_Empathy", "Pos_Empathy"),
      estimate = c(1.1271, 1.0828, .2295, .2904, .1320, .1216, .3561),
      std_estimate = c(.8988, 1.1068, .2790, .3931, .2241, .1610, .3965),
      p = c(0, 0, .0025, 0, 0, .0004, 0),
      stringsAsFactors = FALSE
    )
  )
  class(fit) <- "process_diagram_fit"

  b <- processdiagram:::.pd_diagram_bindings(
    fit,
    labels = c(
      X = "Country",
      M2 = "Independence",
      M1 = "Relational Mobility",
      M3 = "Interdependence",
      Y = "Positive Empathy"
    ),
    layout = "generic"
  )

  mediator_nodes <- b$nodes[grepl("^M", b$nodes$role), , drop = FALSE]
  top <- mediator_nodes[which.max(mediator_nodes$y), , drop = FALSE]
  middle <- mediator_nodes[order(abs(mediator_nodes$y - .50))[1L], , drop = FALSE]
  expect_equal(top$variable, "rmob_total")
  expect_equal(top$label, "Relational Mobility")
  expect_equal(middle$variable, "Ind")
  expect_equal(middle$label, "Independence")

  rm_in <- b$paths[b$paths$from == "Group_num" & b$paths$to == "rmob_total", ]
  rm_out <- b$paths[b$paths$from == "rmob_total" & b$paths$to == "Pos_Empathy", ]
  ind_in <- b$paths[b$paths$from == "Group_num" & b$paths$to == "Ind", ]
  ind_out <- b$paths[b$paths$from == "Ind" & b$paths$to == "Pos_Empathy", ]

  expect_equal(rm_in$std_estimate, .8988)
  expect_equal(rm_out$std_estimate, .2241)
  expect_equal(b$nodes$label[rm_in$to_node], "Relational Mobility")
  expect_equal(b$nodes$label[rm_out$from_node], "Relational Mobility")

  expect_equal(ind_in$std_estimate, 1.1068)
  expect_equal(ind_out$std_estimate, .1610)
  expect_equal(b$nodes$label[ind_in$to_node], "Independence")
  expect_equal(b$nodes$label[ind_out$from_node], "Independence")
})

test_that("mediator reordering moves labels and attached path identities together", {
  sp <- process_spec(
    "Xvar", "Yvar", c("A", "B", "C"),
    bmatrix = c(1, 1, 0, 1, 0, 0, 1, 1, 1, 1)
  )
  fit <- list(
    spec = sp,
    paths = data.frame(
      id = paste0("p", 1:7),
      from = c("Xvar", "Xvar", "Xvar", "Xvar", "A", "B", "C"),
      to = c("A", "B", "C", "Yvar", "Yvar", "Yvar", "Yvar"),
      estimate = 1:7,
      std_estimate = (1:7) / 10,
      p = rep(.01, 7),
      stringsAsFactors = FALSE
    )
  )
  class(fit) <- "process_diagram_fit"

  b <- processdiagram:::.pd_diagram_bindings(
    fit,
    labels = c(M1 = "Alpha", M2 = "Beta", M3 = "Gamma", X = "X", Y = "Y"),
    mediator_order = c("M2", "M1", "M3"),
    layout = "generic"
  )
  mids <- b$nodes[grepl("^M", b$nodes$role), , drop = FALSE]
  expect_equal(mids$variable, c("B", "A", "C"))
  expect_equal(mids$label, c("Beta", "Alpha", "Gamma"))
  expect_equal(b$paths$to_node[b$paths$to == "A"][1], which(b$nodes$variable == "A"))
  expect_equal(b$paths$to_node[b$paths$to == "B"][1], which(b$nodes$variable == "B"))
})

test_that("process_theme convenience colors set text and lines", {
  th <- process_theme(text_color = "#333333", line_color = "#666666")
  expect_equal(th$node_text, "#333333")
  expect_equal(th$coefficient_color, "#333333")
  expect_equal(th$node_border, "#666666")
  expect_equal(th$arrow_color, "#666666")

  th2 <- process_theme(
    text_color = "#333333",
    line_color = "#666666",
    node_text = "red",
    arrow_color = "blue"
  )
  expect_equal(th2$node_text, "red")
  expect_equal(th2$coefficient_color, "#333333")
  expect_equal(th2$node_border, "#666666")
  expect_equal(th2$arrow_color, "blue")
})

test_that("node-specific colors stay bound to variables after mediator reordering", {
  sp <- process_spec(
    "Group_num", "Pos_Empathy", c("rmob_total", "Ind", "Inter"),
    bmatrix = c(1, 1, 0, 1, 0, 0, 1, 1, 1, 1)
  )
  lay <- processdiagram:::.pd_layout(
    sp,
    mediator_order = c("Ind", "rmob_total", "Inter"),
    layout = "generic"
  )

  fills <- processdiagram:::.pd_node_style_values(
    c(
      .default = "white",
      Group_num = "grey90",
      rmob_total = "lightblue",
      M2 = "khaki",
      Inter = "mistyrose",
      Pos_Empathy = "honeydew"
    ),
    lay,
    "node_fill",
    unmatched_default = "white"
  )

  expect_equal(fills[lay$variable == "Group_num"], "grey90")
  expect_equal(fills[lay$variable == "rmob_total"], "lightblue")
  expect_equal(fills[lay$variable == "Ind"], "khaki")
  expect_equal(fills[lay$variable == "Inter"], "mistyrose")
  expect_equal(fills[lay$variable == "Pos_Empathy"], "honeydew")
})

test_that("node-specific color mappings reject ambiguous or unknown keys", {
  sp <- process_spec("Xvar", "Yvar", "Mvar", model = 4)
  lay <- processdiagram:::.pd_layout(sp, layout = "model4_classic")

  expect_error(
    processdiagram:::.pd_node_style_values(
      c(M1 = "red", Mvar = "blue"), lay, "node_fill", "white"
    ),
    "Conflicting `node_fill` colors"
  )

  expect_error(
    processdiagram:::.pd_node_style_values(
      c(Z = "red"), lay, "node_fill", "white"
    ),
    "Unknown name"
  )
})

test_that("exact custom three-mediator variable colors resolve from fit specification", {
  sp <- process_spec(
    "Group_num", "Pos_Empathy", c("rmob_total", "Ind", "Inter"),
    bmatrix = c(1, 1, 0, 1, 0, 0, 1, 1, 1, 1)
  )
  lay <- processdiagram:::.pd_layout(sp, layout = "generic")

  fills <- processdiagram:::.pd_node_style_values(
    c(
      Group_num   = "#D9EAF7",
      rmob_total  = "#E2F0D9",
      Ind         = "#FFF2CC",
      Inter       = "#FCE4D6",
      Pos_Empathy = "#E4DFEC"
    ),
    lay,
    "node_fill",
    unmatched_default = "white",
    spec = sp
  )

  expect_equal(
    stats::setNames(fills, lay$variable),
    c(
      Group_num   = "#D9EAF7",
      rmob_total  = "#E2F0D9",
      Ind         = "#FFF2CC",
      Inter       = "#FCE4D6",
      Pos_Empathy = "#E4DFEC"
    )
  )
})

test_that("fit-spec styling remains attached after mediator reordering", {
  sp <- process_spec(
    "Group_num", "Pos_Empathy", c("rmob_total", "Ind", "Inter"),
    bmatrix = c(1, 1, 0, 1, 0, 0, 1, 1, 1, 1)
  )
  lay <- processdiagram:::.pd_layout(
    sp,
    mediator_order = c("Ind", "rmob_total", "Inter"),
    layout = "generic"
  )

  fills <- processdiagram:::.pd_node_style_values(
    c(rmob_total = "blue", Ind = "gold", Inter = "pink"),
    lay,
    "node_fill",
    unmatched_default = "white",
    spec = sp
  )

  expect_equal(fills[lay$variable == "rmob_total"], "blue")
  expect_equal(fills[lay$variable == "Ind"], "gold")
  expect_equal(fills[lay$variable == "Inter"], "pink")
})
