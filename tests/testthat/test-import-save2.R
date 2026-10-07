row9 <- function(...) {
  vals <- c(...)
  c(vals, rep(99999, 9L - length(vals)))
}

model4_save2_fixture <- function(standardized = TRUE) {
  rows <- list(
    rep(99999, 9),
    row9(.5, .25, 1, 10, 1, 96, .001),       # M summary
    row9(2.0, .1, 20, 0, 1.8, 2.2),          # intercept
    row9(1.2, .2, 6, 0, .8, 1.6)             # X -> M
  )
  if (standardized) rows <- c(rows, list(row9(1.1)))
  rows <- c(rows, list(
    row9(.6, .36, 1, 12, 2, 95, .001),       # Y summary
    row9(1.0, .2, 5, 0, .6, 1.4),            # intercept
    row9(.5, .1, 5, 0, .3, .7),              # X -> Y
    row9(.25, .05, 5, 0, .15, .35)           # M -> Y
  ))
  if (standardized) rows <- c(rows, list(row9(.6), row9(.2)))
  rows <- c(rows, list(
    row9(.5, .1, 5, 0, .3, .7, if (standardized) .6 else 99999),
    row9(.30, .04, .22, .38)
  ))
  if (standardized) rows <- c(rows, list(row9(.22, .03, .16, .29)))
  do.call(rbind, rows)
}

test_that("save=2 importer reads write.csv save=2 format", {
  mat <- model4_save2_fixture(TRUE)
  tf <- tempfile(fileext = ".csv")
  utils::write.csv(mat, tf, row.names = TRUE)

  fit <- import_process_save2(
    tf, x = "X", y = "Y", m = "M", model = 4
  )

  expect_s3_class(fit, "process_diagram_fit")
  expect_true(fit$validation$ok)
  expect_true(fit$standardized)
  expect_equal(fit$n, 98L)
  expect_equal(fit$provenance$x_type, "binary")
  expect_equal(fit$paths$estimate, c(1.2, .5, .25))
  expect_equal(fit$paths$std_estimate, c(1.1, .6, .2))
  expect_equal(fit$indirect$effect, c(.30, .30))
  expect_equal(fit$indirect$std_effect, c(.22, .22))
  expect_equal(fit$indirect$boot_llci, c(.22, .22))
  expect_equal(fit$indirect$std_boot_ulci, c(.29, .29))
})

test_that("save=2 importer can mark an unstandardized binary outcome", {
  mat <- model4_save2_fixture(FALSE)
  fit <- import_process_save2(
    mat, x = "X", y = "Y", m = "M", model = 4,
    outcome_type = "binary"
  )

  expect_true(fit$validation$ok)
  expect_false(fit$standardized)
  expect_equal(fit$outcome_type, "binary")
  expect_true(all(is.na(fit$paths$std_estimate)))
  expect_match(
    figure_note(fit, labels = c(X = "Country", M1 = "Mediator", Y = "Donation")),
    "logistic regression coefficients expressed in log-odds",
    fixed = TRUE
  )
})

test_that("save=2 importer rejects unsupported extra result blocks", {
  mat <- rbind(model4_save2_fixture(TRUE), row9(123))
  expect_error(
    import_process_save2(mat, x = "X", y = "Y", m = "M", model = 4),
    "save=2"
  )
})

model6_save2_fixture <- function() {
  rows <- list(
    rep(99999, 9),
    # M1: summary, intercept, X, standardized X
    row9(.4, .16, 1, 8, 1, 96, .001),
    row9(1, .1, 10, 0, .8, 1.2),
    row9(.4, .1, 4, 0, .2, .6),
    row9(.35),
    # M2: summary, intercept, X, M1, standardized X/M1
    row9(.5, .25, 1, 9, 2, 95, .001),
    row9(1, .1, 10, 0, .8, 1.2),
    row9(.3, .1, 3, .003, .1, .5),
    row9(.5, .1, 5, 0, .3, .7),
    row9(.28), row9(.45),
    # Y: summary, intercept, X, M1, M2, then standardized rows
    row9(.6, .36, 1, 10, 3, 94, .001),
    row9(1, .1, 10, 0, .8, 1.2),
    row9(.2, .1, 2, .046, .001, .399),
    row9(.6, .1, 6, 0, .4, .8),
    row9(.7, .1, 7, 0, .5, .9),
    row9(.18), row9(.52), row9(.61),
    # Direct effect row
    row9(.2, .1, 2, .046, .001, .399, .18),
    # Unstandardized indirects: total, serial, via M1, via M2
    row9(.59, .08, .43, .75),
    row9(.14, .03, .08, .20),
    row9(.24, .05, .14, .34),
    row9(.21, .04, .13, .29),
    # Partially standardized block (binary X). Point estimates equal products
    # of the standardized path coefficients, as in PROCESS.
    row9(.448875, .04, .215, .575),
    row9(.096075, .015, .04, .15),
    row9(.182, .025, .10, .27),
    row9(.1708, .02, .09, .25)
  )
  do.call(rbind, rows)
}

test_that("save=2 importer reconstructs Model 6 specific indirect paths", {
  fit <- import_process_save2(
    model6_save2_fixture(), x = "X", y = "Y", m = c("M1", "M2"), model = 6
  )
  expect_true(fit$validation$ok)
  expect_true(fit$standardized)
  expect_equal(fit$provenance$x_type, "binary")
  expect_equal(
    fit$indirect$path,
    c(
      "TOTAL INDIRECT",
      "X -> M1 -> M2 -> Y",
      "X -> M1 -> Y",
      "X -> M2 -> Y"
    )
  )
  expect_equal(fit$indirect$effect, c(.59, .14, .24, .21))
  expect_equal(fit$indirect$std_effect, c(.448875, .096075, .182, .1708))
})

test_that("save=2 importer skips pairwise indirect-effect contrast rows", {
  mat <- model6_save2_fixture()
  # Sentinel is row 1. The unstandardized indirect block occupies rows 21:24
  # and the standardized block rows 25:28 in this fixture. Insert three PROCESS
  # pairwise contrast rows after each block.
  c1 <- rbind(row9(.10, .02, .04, .16), row9(-.07, .03, -.13, -.01), row9(.03, .02, -.01, .07))
  c2 <- rbind(row9(.08, .02, .03, .13), row9(-.06, .03, -.12, 0), row9(.02, .02, -.02, .06))
  matc <- rbind(mat[1:24, , drop = FALSE], c1, mat[25:28, , drop = FALSE], c2)

  fit <- import_process_save2(
    matc, x = "X", y = "Y", m = c("M1", "M2"), model = 6
  )
  expect_true(fit$validation$ok)
  expect_equal(fit$indirect$effect, c(.59, .14, .24, .21))
  expect_equal(fit$indirect$std_effect, c(.448875, .096075, .182, .1708))
  expect_equal(fit$provenance$save2_indirect_contrast_rows_per_block, 3L)
})

test_that("save=2 importer gives a model-specification hint when row shape cannot match", {
  mat <- model6_save2_fixture()
  expect_error(
    import_process_save2(mat, x = "X", y = "Y", m = c("M1", "M2", "M3"), model = 4),
    "Check that x, y, m, model/bmatrix describe the original PROCESS call",
    fixed = TRUE
  )
})


model4_save2_fixture_v5 <- function(x_binary = TRUE) {
  # PROCESS 5 standardized save=2 layout.  After each coefficient table it
  # stores a six-column association table (r, sr, pr, standYX, standY, standX)
  # and a three-column eta-squared table.
  xm_standy <- 1.10
  xm_standyx <- 0.55
  xy_standy <- 0.60
  xy_standyx <- 0.30
  my_standyx <- 0.20
  rows <- list(
    rep(99999, 9),
    row9(.5, .25, 1, 10, 1, 96, .001),
    row9(2.0, .1, 20, 0, 1.8, 2.2),
    row9(1.2, .2, 6, 0, .8, 1.6),
    row9(.4, .4, .4, xm_standyx, xm_standy, .56),
    row9(.16, .16, .19),
    row9(.6, .36, 1, 12, 2, 95, .001),
    row9(1.0, .2, 5, 0, .6, 1.4),
    row9(.5, .1, 5, 0, .3, .7),
    row9(.25, .05, 5, 0, .15, .35),
    row9(.3, .3, .3, xy_standyx, xy_standy, .25),
    row9(.2, .2, .2, my_standyx, .18, .21),
    row9(.09, .09, .10),
    row9(.04, .04, .05),
    row9(.5, .1, 5, 0, .3, .7, if (x_binary) xy_standy else xy_standyx),
    row9(.30, .04, .22, .38)
  )
  if (x_binary) {
    # StandY indirect: StandY(X->M) * StandYX(M->Y)
    rows <- c(rows, list(row9(xm_standy * my_standyx, .03, .16, .29)))
  } else {
    # PROCESS stores StandY first, then StandYX for continuous X.
    rows <- c(rows, list(
      row9(xm_standy * my_standyx, .03, .16, .29),
      row9(xm_standyx * my_standyx, .02, .07, .16)
    ))
  }
  do.call(rbind, rows)
}

test_that("save=2 importer auto-detects PROCESS 5 standardized binary-X layout", {
  fit <- import_process_save2(
    model4_save2_fixture_v5(TRUE), x = "X", y = "Y", m = "M", model = 4
  )
  expect_true(fit$validation$ok)
  expect_true(fit$standardized)
  expect_equal(fit$provenance$process_version, "5.0")
  expect_equal(fit$provenance$x_type, "binary")
  expect_equal(fit$paths$std_estimate, c(1.10, .60, .20))
  expect_equal(fit$indirect$std_effect, c(.22, .22))
})

test_that("save=2 importer uses StandYX for PROCESS 5 continuous X", {
  fit <- import_process_save2(
    model4_save2_fixture_v5(FALSE), x = "X", y = "Y", m = "M", model = 4
  )
  expect_true(fit$validation$ok)
  expect_equal(fit$provenance$process_version, "5.0")
  expect_equal(fit$provenance$x_type, "continuous")
  expect_equal(fit$paths$std_estimate, c(.55, .30, .20))
  expect_equal(fit$indirect$std_effect, c(.11, .11))
})

test_that("PROCESS 5 signature resolves row-count ambiguity with PROCESS 4.1", {
  # The continuous-X PROCESS 5 Model 4 fixture has a row count that can overlap
  # a PROCESS 4.1 layout with contrast output.  The six-column association block
  # must identify this as PROCESS 5 rather than relying on row count alone.
  fit <- import_process_save2(
    model4_save2_fixture_v5(FALSE), x = "X", y = "Y", m = "M", model = 4
  )
  expect_equal(fit$provenance$process_version, "5.0")
})


process5_parallel3_contrast_fixture <- function() {
  eq_one <- function(b, stdy) {
    list(
      row9(.4, .16, 1, 8, 1, 96, .001),
      row9(1, .1, 10, 0, .8, 1.2),
      row9(b, .1, 4, 0, b - .2, b + .2),
      row9(.3, .3, .3, stdy / 2, stdy, .25),
      row9(.09, .09, .10)
    )
  }
  rows <- list(rep(99999, 9))
  rows <- c(rows, eq_one(.4, .5), eq_one(.5, .6), eq_one(.6, .7))
  # Final Y equation: X, M1, M2, M3.
  rows <- c(rows, list(
    row9(.7, .49, 1, 15, 4, 93, 0),
    row9(1, .1, 10, 0, .8, 1.2),
    row9(.2, .05, 4, 0, .1, .3),
    row9(.3, .05, 6, 0, .2, .4),
    row9(.4, .05, 8, 0, .3, .5),
    row9(.5, .05, 10, 0, .4, .6),
    row9(.2, .2, .2, .125, .25, .10),
    row9(.3, .3, .3, .4, .35, .30),
    row9(.4, .4, .4, .5, .45, .40),
    row9(.5, .5, .5, .6, .55, .50),
    row9(.04, .04, .05),
    row9(.16, .16, .18),
    row9(.25, .25, .28),
    row9(.36, .36, .40),
    # Direct effect
    row9(.2, .05, 4, 0, .1, .3, .25),
    # Unstandardized total + three specifics
    row9(.62, .08, .46, .78),
    row9(.12, .03, .06, .18),
    row9(.20, .04, .12, .28),
    row9(.30, .05, .20, .40),
    # Three pairwise contrast rows (contrast = 1)
    row9(-.08, .04, -.16, 0),
    row9(-.18, .05, -.28, -.08),
    row9(-.10, .05, -.20, 0),
    # StandY total + specifics
    row9(.92, .10, .72, 1.12),
    row9(.20, .04, .12, .28),
    row9(.30, .05, .20, .40),
    row9(.42, .06, .30, .54),
    # Standardized contrast rows
    row9(-.10, .04, -.18, -.02),
    row9(-.22, .06, -.34, -.10),
    row9(-.12, .05, -.22, -.02)
  ))
  do.call(rbind, rows)
}

test_that("PROCESS 5 custom three-mediator save=2 with contrast=1 imports safely", {
  bmat <- c(1,1,0,1,0,0,1,1,1,1)
  fit <- import_process_save2(
    process5_parallel3_contrast_fixture(),
    x = "X", y = "Y", m = c("M1", "M2", "M3"), bmatrix = bmat
  )
  expect_true(fit$validation$ok)
  expect_equal(fit$provenance$process_version, "5.0")
  expect_equal(fit$provenance$save2_indirect_contrast_rows_per_block, 3L)
  expect_equal(fit$paths$std_estimate, c(.5, .6, .7, .25, .4, .5, .6))
  expect_equal(fit$indirect$effect, c(.62, .12, .20, .30))
  expect_equal(fit$indirect$std_effect, c(.92, .20, .30, .42))
})


process5_custom_binaryx_fixture <- function() {
  # Custom three-parallel-mediator PROCESS 5 save=2 result with contrast=1.
  # The final 0/0/0 row is the PROCESS 5 bootstrap diagnostics trailer.
  rows <- list(
    rep(99999, 9),
    # M1
    row9(.4, .16, 1, 10, 1, 96, .001),
    row9(1, .1, 10, 0, .8, 1.2),
    row9(.8, .1, 8, 0, .6, 1.0),
    row9(.4, .4, .4, .4, .8, .4),
    row9(.16, .16, .19),
    # M2
    row9(.5, .25, 1, 12, 1, 96, .001),
    row9(1, .1, 10, 0, .8, 1.2),
    row9(1.0, .1, 10, 0, .8, 1.2),
    row9(.5, .5, .5, .5, 1.0, .5),
    row9(.25, .25, .33),
    # M3
    row9(.2, .04, 1, 4, 1, 96, .05),
    row9(1, .1, 10, 0, .8, 1.2),
    row9(.3, .1, 3, .003, .1, .5),
    row9(.15, .15, .15, .15, .3, .15),
    row9(.02, .02, .02),
    # Y: intercept, X, M1, M2, M3
    row9(.7, .49, .3, 20, 4, 93, .001),
    row9(1, .1, 10, 0, .8, 1.2),
    row9(.2, .05, 4, 0, .1, .3),
    row9(.2, .05, 4, 0, .1, .3),
    row9(.15, .05, 3, .003, .05, .25),
    row9(.4, .05, 8, 0, .3, .5),
    # association rows: X uses StandY; mediators use StandYX
    row9(.2, .1, .1, .1, .2, .1),
    row9(.2, .1, .1, .2, .2, .2),
    row9(.15, .1, .1, .15, .15, .15),
    row9(.4, .2, .2, .4, .4, .4),
    # eta-sq rows
    row9(.01, .01, .01), row9(.04, .04, .04),
    row9(.02, .02, .02), row9(.16, .16, .16),
    # direct effect
    row9(.2, .05, 4, 0, .1, .3, .2),
    # ordinary indirects: total, M1, M2, M3
    row9(.43, .04, .35, .51),
    row9(.16, .02, .12, .20),
    row9(.15, .02, .11, .19),
    row9(.12, .02, .08, .16),
    # pairwise contrasts
    row9(.01, .02, -.03, .05), row9(.04, .02, 0, .08), row9(.03, .02, -.01, .07),
    # StandY indirects: products of .8*.2, 1*.15, .3*.4
    row9(.43, .04, .35, .51),
    row9(.16, .02, .12, .20),
    row9(.15, .02, .11, .19),
    row9(.12, .02, .08, .16),
    # standardized contrasts
    row9(.01, .02, -.03, .05), row9(.04, .02, 0, .08), row9(.03, .02, -.01, .07),
    # PROCESS 5 bootstrap diagnostics trailer: badboot, singerc, eiverc
    row9(0, 0, 0)
  )
  do.call(rbind, rows)
}

test_that("PROCESS 5 bootstrap trailer detection tolerates named matrix columns", {
  mat <- rbind(
    row9(1, 2, 3, 4),
    row9(0, 0, 0)
  )
  colnames(mat) <- paste0("V", seq_len(ncol(mat)))

  out <- processdiagram:::.pd_save2_extract_v5_boot_trailer(mat, "auto")

  expect_true(out$detected)
  expect_equal(nrow(out$matrix), 1L)
  expect_equal(unname(out$values), c(0, 0, 0))
})

test_that("PROCESS 5 save=2 bootstrap trailer does not masquerade as another schema", {
  fit <- import_process_save2(
    process5_custom_binaryx_fixture(),
    x = "X", y = "Y", m = c("M1", "M2", "M3"),
    bmatrix = c(1,1,0,1,0,0,1,1,1,1)
  )

  expect_true(fit$validation$ok)
  expect_equal(fit$provenance$process_version, "5.0")
  expect_equal(fit$provenance$x_type, "binary")
  expect_true(fit$provenance$save2_process5_bootstrap_trailer)
  expect_equal(unname(fit$provenance$save2_bootstrap_diagnostic_counts), c(0,0,0))
  expect_equal(fit$provenance$save2_indirect_contrast_rows_per_block, 3L)
  expect_equal(fit$validation$mode, "save2-internal")
  expect_equal(fit$provenance$validation_mode, "save2-internal")
  expect_null(fit$provenance$legacy_format)
  expect_null(fit$provenance$legacy_indirect_contrast_rows_per_block)
  expect_equal(fit$indirect$std_effect, c(.43, .16, .15, .12), tolerance = 1e-10)
})
