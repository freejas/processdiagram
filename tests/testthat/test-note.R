test_that("figure note matches three-mediator standardized example and visual order", {
  spec <- process_spec(
    x = "Group_num", y = "empathy.ec",
    m = c("Ind", "rmob_total", "Inter"),
    bmatrix = c(1,1,0,1,0,0,1,1,1,1)
  )
  fit <- list(
    spec = spec,
    standardized = TRUE,
    paths = data.frame(p = c(.001, .2)),
    indirect = data.frame(
      id = c("total", "ind1", "ind2", "ind3"),
      path = c(
        "TOTAL INDIRECT",
        "Group_num -> Ind -> empathy.ec",
        "Group_num -> rmob_total -> empathy.ec",
        "Group_num -> Inter -> empathy.ec"
      ),
      effect = c(.3905, .1009, .1651, .1244),
      boot_llci = c(.2189, -.0345, .0513, .0466),
      boot_ulci = c(.5743, .2445, .2927, .2028),
      std_effect = c(.3299, .0853, .1395, .1051),
      std_boot_llci = c(.1853, -.0293, .0438, .0394),
      std_boot_ulci = c(.4855, .2077, .2464, .1711),
      stringsAsFactors = FALSE
    )
  )
  class(fit) <- "process_diagram_fit"

  out <- figure_note(
    fit,
    labels = c(
      X = "Country\n(0 = Japan, 1 = USA)",
      M1 = "Independence",
      M2 = "Relational Mobility",
      M3 = "Interdependence",
      Y = "Empathic Concern"
    ),
    mediator_order = c("M2", "M1", "M3")
  )

  expect_equal(
    out,
    paste0(
      "*Note:* Path coefficients are standardized. ",
      "Indirect effects from Country to Empathic Concern: ",
      "through Relational Mobility = 0.14, 95% CI [.04, .25]; ",
      "through Independence = 0.09, 95% CI [-.03, .21]; ",
      "through Interdependence = 0.11, 95% CI [.04, .17]. ",
      "Solid lines represent significant paths; dashed lines represent non-significant paths. ",
      "* p < .05; ** p < .01; *** p < .001"
    )
  )
})

test_that("single mediator note reports one indirect effect, not duplicate total", {
  spec <- process_spec(x = "Group_num", y = "Pos_Empathy", m = "Ind", model = 4)
  fit <- list(
    spec = spec,
    standardized = TRUE,
    indirect = data.frame(
      id = c("total", "ind1"),
      path = c("TOTAL INDIRECT", "Group_num -> Ind -> Pos_Empathy"),
      effect = c(.2007, .2007),
      boot_llci = c(.1045, .1045),
      boot_ulci = c(.3037, .3037),
      std_effect = c(.2716, .2716),
      std_boot_llci = c(.1435, .1435),
      std_boot_ulci = c(.4056, .4056),
      stringsAsFactors = FALSE
    )
  )
  class(fit) <- "process_diagram_fit"

  out <- figure_note(
    fit,
    labels = c(X = "Country\n(0 = Japan, 1 = USA)", M1 = "Independence", Y = "Positive Empathy")
  )

  expect_match(out, "Indirect effect from Country to Positive Empathy through Independence = 0.27, 95% CI \\[.14, .41\\]\\.")
  expect_false(grepl("TOTAL", out, fixed = TRUE))
})


test_that("binary-outcome note identifies logistic log-odds paths", {
  set.seed(99)
  n <- 120
  x <- rbinom(n, 1, .5)
  m <- 0.5 * x + rnorm(n)
  y <- rbinom(n, 1, plogis(-0.2 + 0.4 * x + 0.6 * m))
  d <- data.frame(x, m, y)
  suppressWarnings({
    f <- fit_process_diagram(
      d, x = "x", y = "y", m = "m", model = 4,
      standardized = TRUE, boot = 0
    )
  })
  note <- figure_note(
    f,
    labels = c(X = "Country", M1 = "Mediator", Y = "Donation")
  )
  expect_match(note, "Path coefficients are unstandardized", fixed = TRUE)
  expect_match(note, "Paths predicting Donation are logistic regression coefficients expressed in log-odds", fixed = TRUE)
})
