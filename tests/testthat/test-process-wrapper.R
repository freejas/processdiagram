test_that("PROCESS output parser recovers path and indirect tables", {
  d <- data.frame(
    x = rep(c(0, 1), each = 10),
    m = seq_len(20) / 10,
    y = seq_len(20) / 20 + rep(c(0, 1), each = 10)
  )
  spec <- process_spec("x", "y", "m", model = 4)

  txt <- c(
    "********************** PROCESS for R Version 4.1 **********************",
    "Sample size: 20",
    "Outcome Variable: m",
    "Model:",
    "              coeff        se         t         p      LLCI      ULCI",
    "constant     0.1000    0.1000    1.0000    0.3000   -0.1000    0.3000",
    "x            1.0000    0.2000    5.0000    0.0000    0.6000    1.4000",
    "Standardized coefficients:",
    "              coeff",
    "x            1.2000",
    "Outcome Variable: y",
    "Model:",
    "              coeff        se         t         p      LLCI      ULCI",
    "constant     0.2000    0.1000    2.0000    0.0500    0.0000    0.4000",
    "x            0.3000    0.1000    3.0000    0.0040    0.1000    0.5000",
    "m            0.5000    0.1000    5.0000    0.0000    0.3000    0.7000",
    "Standardized coefficients:",
    "              coeff",
    "x            0.4000",
    "m            0.6000",
    "Indirect effect(s) of X on Y:",
    "              Effect    BootSE  BootLLCI  BootULCI",
    "m             0.5000    0.1000    0.3000    0.7000",
    "Partially standardized indirect effect(s) of X on Y:",
    "              Effect    BootSE  BootLLCI  BootULCI",
    "m             0.5500    0.1100    0.3300    0.7700"
  )

  parsed <- processdiagram:::.pd_parse_process_output(
    txt, spec = spec, data = d, standardized = TRUE
  )
  expect_equal(nrow(parsed$paths), 3)
  expect_equal(parsed$paths$estimate[parsed$paths$from == "m"], 0.5)
  expect_equal(parsed$indirect$boot_llci, 0.3)
  expect_equal(parsed$standardized_indirect$boot_ulci, 0.77)
  expect_equal(parsed$process_version, "4.1")
})


test_that("process_for_diagram stores seed and uses PROCESS values", {
  set.seed(44)
  n <- 120
  x <- rbinom(n, 1, .5)
  m <- 0.7 * x + rnorm(n)
  y <- 0.2 * x + 0.5 * m + rnorm(n)
  d <- data.frame(x, m, y)

  mock_process <- function(data, y, x, m, model = 4, stand = 0,
                           boot = 0, conf = 95, seed = -999, save = 2,
                           progress = 0, outscreen = 1, ...) {
    set.seed(seed)
    fm <- lm(reformulate(x, response = m), data = data)
    fy <- lm(reformulate(c(x, m), response = y), data = data)

    print_block <- function(outcome, fit, predictors) {
      sm <- summary(fit)$coefficients
      ci <- confint(fit, level = conf / 100)
      cat("Outcome Variable:", outcome, "\n")
      cat("Model:\n")
      cat("              coeff        se         t         p      LLCI      ULCI\n")
      rn <- c("constant", predictors)
      terms <- c("(Intercept)", predictors)
      for (i in seq_along(terms)) {
        vals <- c(
          coef(fit)[terms[i]], sm[terms[i], "Std. Error"],
          sm[terms[i], "t value"], sm[terms[i], "Pr(>|t|)"],
          ci[terms[i], 1], ci[terms[i], 2]
        )
        cat(sprintf("%-12s %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f\n",
                    rn[i], vals[1], vals[2], vals[3], vals[4], vals[5], vals[6]))
      }
      if (stand == 1) {
        cat("Standardized coefficients:\n")
        cat("              coeff\n")
        for (pred in predictors) {
          mult <- if (pred == x && length(unique(data[[pred]])) == 2) 1 else sd(data[[pred]])
          b4 <- as.numeric(sprintf("%.4f", coef(fit)[pred]))
          st <- b4 * mult / sd(data[[outcome]])
          cat(sprintf("%-12s %9.4f\n", pred, st))
        }
      }
    }

    cat("********************** PROCESS for R Version TEST **********************\n")
    cat("Sample size:", nrow(data), "\n")
    print_block(m, fm, x)
    print_block(y, fy, c(x, m))

    a <- coef(fm)[x]
    b <- coef(fy)[m]
    ind <- unname(a * b)
    cat("Indirect effect(s) of X on Y:\n")
    cat("              Effect\n")
    cat(sprintf("%-12s %9.4f\n", m, ind))
    if (stand == 1) {
      sind <- ind / sd(data[[y]])
      cat("Partially standardized indirect effect(s) of X on Y:\n")
      cat("              Effect\n")
      cat(sprintf("%-12s %9.4f\n", m, sind))
    }
    invisible(matrix(1, 1, 1))
  }

  f <- process_for_diagram(
    d, x = "x", y = "y", m = "m", model = 4,
    standardized = TRUE, boot = 0, seed = 24680,
    process_fun = mock_process, show_process_output = FALSE
  )

  expect_true(isTRUE(f$validation$ok))
  expect_equal(f$provenance$seed, 24680L)
  expect_equal(f$provenance$engine, "PROCESS")
  expect_equal(f$provenance$values_source, "captured PROCESS output")
  expect_equal(f$paths$estimate, round(f$paths$estimate, 4))
})



test_that("indirect parser stops before adjacent standardized table", {
  lines <- c(
    "Indirect effect(s) of X on Y:",
    "              Effect    BootSE  BootLLCI  BootULCI",
    "m             0.5000    0.1000    0.3000    0.7000",
    "Partially standardized indirect effect(s) of X on Y:",
    "              Effect    BootSE  BootLLCI  BootULCI",
    "m             0.5500    0.1100    0.3300    0.7700"
  )
  got <- processdiagram:::.pd_parse_process_indirect_table(
    lines, "^Indirect effect\\(s\\) of X on Y:$"
  )
  expect_equal(nrow(got), 1L)
  expect_equal(got$boot_llci, 0.3)
})


test_that("single-mediator PROCESS row maps to both total and specific indirect", {
  native <- data.frame(
    id = c("total", "ind1"),
    path = c("TOTAL INDIRECT", "x -> m -> y"),
    effect = c(0.5, 0.5),
    stringsAsFactors = FALSE
  )
  proc <- data.frame(
    label = "m", effect = 0.5,
    boot_se = NA_real_, boot_llci = NA_real_, boot_ulci = NA_real_,
    stringsAsFactors = FALSE
  )
  expect_equal(
    processdiagram:::.pd_align_process_indirect(native, proc, tolerance = 1e-4),
    c(1L, 1L)
  )
})


test_that("PROCESS parser accepts dichotomous Y without standardized coefficient blocks", {
  d <- data.frame(
    x = rep(c(0, 1), each = 10),
    m = seq_len(20) / 10,
    y = rep(c(0, 1), 10)
  )
  spec <- process_spec("x", "y", "m", model = 4)

  txt <- c(
    "********************** PROCESS for R Version 4.1 **********************",
    "Sample size: 20",
    "Outcome Variable: m",
    "Model:",
    "              coeff        se         t         p      LLCI      ULCI",
    "constant     0.1000    0.1000    1.0000    0.3000   -0.1000    0.3000",
    "x            1.0000    0.2000    5.0000    0.0000    0.6000    1.4000",
    "Outcome Variable: y",
    "Model:",
    "              coeff        se         Z         p      LLCI      ULCI",
    "constant    -0.2000    0.3000   -0.6667    0.5050   -0.7880    0.3880",
    "x            0.3000    0.2000    1.5000    0.1336   -0.0920    0.6920",
    "m            0.5000    0.2000    2.5000    0.0124    0.1080    0.8920",
    "",
    "These results are expressed in a log-odds metric.",
    "Indirect effect(s) of X on Y:",
    "              Effect    BootSE  BootLLCI  BootULCI",
    "m             0.5000    0.1000    0.3000    0.7000"
  )

  parsed <- processdiagram:::.pd_parse_process_output(
    txt, spec = spec, data = d, standardized = FALSE
  )
  expect_equal(nrow(parsed$paths), 3L)
  expect_true(all(is.na(parsed$paths$std_estimate)))
  expect_equal(parsed$paths$estimate[parsed$paths$from == "m"], 0.5)
  expect_equal(parsed$indirect$effect, 0.5)
})


test_that("PROCESS 5 parser reads scale-free standardized measures and new headings", {
  d <- data.frame(
    x = rep(c(0, 1), each = 10),
    m = seq_len(20) / 10,
    y = seq_len(20) / 20 + rep(c(0, 1), each = 10)
  )
  spec <- process_spec("x", "y", "m", model = 4)

  txt <- c(
    "**************** PROCESS Procedure for R Version 5.0 ******************",
    "Sample size: 20",
    "Outcome Variable: m",
    "Model:",
    "              coeff        se         t         p      LLCI      ULCI",
    "constant     0.1000    0.1000    1.0000    0.3000   -0.1000    0.3000",
    "x            1.0000    0.2000    5.0000    0.0000    0.6000    1.4000",
    "Scale-free and standardized measures of association:",
    "                 r        sr        pr   standYX    standY    standX",
    "x            0.5000    0.4000    0.4000    0.6000    1.2000    0.5000",
    "",
    "Outcome Variable: y",
    "Model:",
    "              coeff        se         t         p      LLCI      ULCI",
    "constant     0.2000    0.1000    2.0000    0.0500    0.0000    0.4000",
    "x            0.3000    0.1000    3.0000    0.0040    0.1000    0.5000",
    "m            0.5000    0.1000    5.0000    0.0000    0.3000    0.7000",
    "Scale-free and standardized measures of association:",
    "                 r        sr        pr   standYX    standY    standX",
    "x            0.3000    0.2000    0.2000    0.2000    0.4000    0.1500",
    "m            0.5000    0.4000    0.4000    0.6000    0.5000    0.7000",
    "",
    "Indirect effect(s) of X on Y:",
    "              Effect    BootSE  BootLLCI  BootULCI",
    "m             0.5000    0.1000    0.3000    0.7000",
    "Partially standardized (StandY) indirect effect(s) of X on Y:",
    "              Effect    BootSE  BootLLCI  BootULCI",
    "m             0.5500    0.1100    0.3300    0.7700"
  )

  parsed <- processdiagram:::.pd_parse_process_output(
    txt, spec = spec, data = d, standardized = TRUE
  )
  expect_equal(parsed$process_version, "5.0")
  expect_equal(parsed$standardization_style, "process5_scale_free")
  expect_equal(parsed$paths$std_estimate[parsed$paths$from == "x" & parsed$paths$to == "m"], 1.2)
  expect_equal(parsed$paths$std_estimate[parsed$paths$from == "x" & parsed$paths$to == "y"], 0.4)
  expect_equal(parsed$paths$std_estimate[parsed$paths$from == "m"], 0.6)
  expect_equal(parsed$standardized_indirect$boot_ulci, 0.77)
})


test_that("process_for_diagram validates PROCESS 5 scale-free output", {
  set.seed(45)
  n <- 120
  x <- rbinom(n, 1, .5)
  m <- 0.7 * x + rnorm(n)
  y <- 0.2 * x + 0.5 * m + rnorm(n)
  d <- data.frame(x, m, y)

  mock_process5 <- function(data, y, x, m, model = 4, stand = 0,
                            boot = 0, conf = 95, seed = -999, save = 2,
                            progress = 0, outscreen = 1, ...) {
    set.seed(seed)
    fm <- lm(reformulate(x, response = m), data = data)
    fy <- lm(reformulate(c(x, m), response = y), data = data)

    print_block <- function(outcome, fit, predictors) {
      sm <- summary(fit)$coefficients
      ci <- confint(fit, level = conf / 100)
      cat("Outcome Variable:", outcome, "\n")
      cat("Model:\n")
      cat("              coeff        se         t         p      LLCI      ULCI\n")
      rn <- c("constant", predictors)
      terms <- c("(Intercept)", predictors)
      for (i in seq_along(terms)) {
        vals <- c(
          coef(fit)[terms[i]], sm[terms[i], "Std. Error"],
          sm[terms[i], "t value"], sm[terms[i], "Pr(>|t|)"],
          ci[terms[i], 1], ci[terms[i], 2]
        )
        cat(sprintf("%-12s %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f\n",
                    rn[i], vals[1], vals[2], vals[3], vals[4], vals[5], vals[6]))
      }
      if (stand == 1) {
        cat("Scale-free and standardized measures of association:\n")
        cat("                 r        sr        pr   standYX    standY    standX\n")
        for (pred in predictors) {
          b <- unname(coef(fit)[pred])
          sy <- sd(data[[outcome]])
          sx <- sd(data[[pred]])
          stand_yx <- b * sx / sy
          stand_y <- b / sy
          stand_x <- b * sx
          cat(sprintf("%-12s %9.4f %9.4f %9.4f %9.4f %9.4f %9.4f\n",
                      pred, 0, 0, 0, stand_yx, stand_y, stand_x))
        }
        cat("\n")
      }
    }

    cat("**************** PROCESS Procedure for R Version 5.0 ******************\n")
    cat("Sample size:", nrow(data), "\n")
    print_block(m, fm, x)
    print_block(y, fy, c(x, m))

    ind <- unname(coef(fm)[x] * coef(fy)[m])
    cat("Indirect effect(s) of X on Y:\n")
    cat("              Effect\n")
    cat(sprintf("%-12s %9.4f\n", m, ind))
    if (stand == 1) {
      sind <- ind / sd(data[[y]])
      cat("Partially standardized (StandY) indirect effect(s) of X on Y:\n")
      cat("              Effect\n")
      cat(sprintf("%-12s %9.4f\n", m, sind))
    }
    invisible(matrix(1, 1, 1))
  }

  f <- process_for_diagram(
    d, x = "x", y = "y", m = "m", model = 4,
    standardized = TRUE, boot = 0, seed = 24680,
    process_fun = mock_process5, show_process_output = FALSE
  )

  expect_true(isTRUE(f$validation$ok))
  expect_equal(f$provenance$process_version, "5.0")
  expect_equal(f$provenance$standardization_source, "process5_scale_free")
  expect_equal(
    f$paths$std_estimate[f$paths$from == "x" & f$paths$to == "m"],
    round(unname(coef(lm(m ~ x, data = d))["x"]) / sd(d$m), 4),
    tolerance = 1e-4
  )
})


test_that("PROCESS 5 parser uses standYX for continuous focal X", {
  d <- data.frame(
    x = seq_len(20) / 10,
    m = seq_len(20) / 5 + rnorm(20, 0, .01),
    y = seq_len(20) / 4 + rnorm(20, 0, .01)
  )
  spec <- process_spec("x", "y", "m", model = 4)

  txt <- c(
    "**************** PROCESS Procedure for R Version 5.0 ******************",
    "Sample size: 20",
    "Outcome Variable: m",
    "Model:",
    "              coeff        se         t         p      LLCI      ULCI",
    "constant     0.1000    0.1000    1.0000    0.3000   -0.1000    0.3000",
    "x            1.0000    0.2000    5.0000    0.0000    0.6000    1.4000",
    "Scale-free and standardized measures of association:",
    "                 r        sr        pr   standYX    standY    standX",
    "x            0.5000    0.4000    0.4000    0.6100    1.2100    0.5100",
    "",
    "Outcome Variable: y",
    "Model:",
    "              coeff        se         t         p      LLCI      ULCI",
    "constant     0.2000    0.1000    2.0000    0.0500    0.0000    0.4000",
    "x            0.3000    0.1000    3.0000    0.0040    0.1000    0.5000",
    "m            0.5000    0.1000    5.0000    0.0000    0.3000    0.7000",
    "Scale-free and standardized measures of association:",
    "                 r        sr        pr   standYX    standY    standX",
    "x            0.3000    0.2000    0.2000    0.2100    0.4100    0.1600",
    "m            0.5000    0.4000    0.4000    0.6200    0.5200    0.7200",
    "",
    "Indirect effect(s) of X on Y:",
    "              Effect    BootSE  BootLLCI  BootULCI",
    "m             0.5000    0.1000    0.3000    0.7000",
    "Completely standardized (StandYX) indirect effect(s) of X on Y:",
    "              Effect    BootSE  BootLLCI  BootULCI",
    "m             0.3050    0.0600    0.1800    0.4300"
  )

  parsed <- processdiagram:::.pd_parse_process_output(
    txt, spec = spec, data = d, standardized = TRUE
  )
  expect_equal(parsed$paths$std_estimate[parsed$paths$from == "x" & parsed$paths$to == "m"], 0.61)
  expect_equal(parsed$paths$std_estimate[parsed$paths$from == "x" & parsed$paths$to == "y"], 0.21)
  expect_equal(parsed$paths$std_estimate[parsed$paths$from == "m"], 0.62)
  expect_equal(parsed$standardized_indirect$effect, 0.305)
})
