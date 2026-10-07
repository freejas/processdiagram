# Fit PROCESS-style path equations -----------------------------------------

.pd_complete_data <- function(data, nodes) {
  if (!is.data.frame(data)) data <- as.data.frame(data)
  missing_names <- setdiff(nodes, names(data))
  if (length(missing_names)) {
    .pd_stop("Variables not found in data: ", paste(missing_names, collapse = ", "))
  }
  for (nm in nodes) {
    if (!is.numeric(data[[nm]])) {
      .pd_stop("Initial version requires numeric variables. Non-numeric: ", nm)
    }
  }
  data[stats::complete.cases(data[, nodes, drop = FALSE]), , drop = FALSE]
}

.pd_fit_paths <- function(data, spec, conf = .95, standardized = TRUE,
                          process_digits = 4L) {
  d <- .pd_complete_data(data, spec$nodes)
  if (nrow(d) < 5L) .pd_stop("Too few complete cases remain.")
  adj <- spec$adjacency
  rows <- list()
  models <- list()
  id <- 0L

  binary_y <- .pd_is_binary_numeric(d[[spec$y]])

  for (i in 2:length(spec$nodes)) {
    outcome <- spec$nodes[i]
    pred_idx <- which(adj[i, ] == 1L)
    if (!length(pred_idx)) next
    predictors <- spec$nodes[pred_idx]

    is_binary_outcome <- identical(outcome, spec$y) && binary_y
    model_data <- d
    if (is_binary_outcome) {
      # PROCESS recodes the higher of the two observed Y values to 1 and the
      # lower value to 0 before fitting the logistic outcome model.
      ymax <- max(model_data[[outcome]], na.rm = TRUE)
      model_data[[outcome]] <- as.numeric(model_data[[outcome]] == ymax)
      fit <- stats::glm(
        .pd_formula(outcome, predictors),
        data = model_data,
        family = stats::binomial(link = "logit")
      )
      models[[outcome]] <- fit
      sm <- summary(fit)$coefficients
      zcrit <- stats::qnorm(1 - (1 - conf) / 2)
    } else {
      fit <- stats::lm(.pd_formula(outcome, predictors), data = model_data)
      models[[outcome]] <- fit
      sm <- summary(fit)$coefficients
      ci <- stats::confint(fit, level = conf)
    }

    for (pred in predictors) {
      id <- id + 1L
      est <- unname(stats::coef(fit)[pred])
      se <- unname(sm[pred, "Std. Error"])
      if (is_binary_outcome) {
        stat <- unname(sm[pred, "z value"])
        p <- unname(sm[pred, "Pr(>|z|)"])
        ll <- est - zcrit * se
        ul <- est + zcrit * se
      } else {
        stat <- unname(sm[pred, "t value"])
        p <- unname(sm[pred, "Pr(>|t|)"])
        ll <- unname(ci[pred, 1L])
        ul <- unname(ci[pred, 2L])
      }

      std_est <- NA_real_
      if (standardized) {
        pred_sd <- if (identical(pred, spec$x) && .pd_is_binary_numeric(d[[pred]])) {
          1
        } else {
          stats::sd(d[[pred]])
        }
        out_sd <- stats::sd(d[[outcome]])

        # PROCESS 4.1 formats the regression coefficient to its requested
        # display precision before calculating the standardized coefficient.
        # Matching that behavior keeps the native engine aligned with legacy
        # PROCESS output. process_for_diagram() refreshes these standardized
        # values from full-precision coefficients when validating PROCESS 5.0.
        est_for_std <- as.numeric(sprintf(
          paste0("%.", as.integer(process_digits), "f"), est
        ))
        std_est <- est_for_std * pred_sd / out_sd
      }

      rows[[id]] <- data.frame(
        id = paste0("p", id),
        from = pred,
        to = outcome,
        estimate = est,
        std_estimate = std_est,
        se = se,
        statistic = stat,
        p = p,
        llci = ll,
        ulci = ul,
        stringsAsFactors = FALSE
      )
    }
  }

  list(paths = do.call(rbind, rows), models = models, data = d)
}

.pd_directed_paths <- function(spec) {
  adj <- spec$adjacency
  n <- nrow(adj)
  output <- list()
  walk <- function(current, path) {
    if (current == n) {
      output[[length(output) + 1L]] <<- path
      return(invisible(NULL))
    }
    next_nodes <- which(adj[, current] == 1L)
    next_nodes <- next_nodes[next_nodes > current]
    for (nx in next_nodes) walk(nx, c(path, nx))
    invisible(NULL)
  }
  walk(1L, 1L)
  Filter(function(p) length(p) > 2L, output)
}

.pd_effect_for_index_path <- function(index_path, spec, path_table,
                                      column = "estimate") {
  vals <- numeric(length(index_path) - 1L)
  for (k in seq_along(vals)) {
    from <- spec$nodes[index_path[k]]
    to <- spec$nodes[index_path[k + 1L]]
    hit <- path_table$from == from & path_table$to == to
    if (sum(hit) != 1L) return(NA_real_)
    vals[k] <- path_table[[column]][hit]
  }
  prod(vals)
}

.pd_indirect_point <- function(spec, paths) {
  ipaths <- .pd_directed_paths(spec)
  if (!length(ipaths)) return(data.frame())
  out <- lapply(seq_along(ipaths), function(i) {
    p <- ipaths[[i]]
    data.frame(
      id = paste0("ind", i),
      path = paste(spec$nodes[p], collapse = " -> "),
      effect = .pd_effect_for_index_path(p, spec, paths, "estimate"),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

.pd_process_percentile_ci <- function(x, conf = .95) {
  x <- sort(x[is.finite(x)])
  b <- length(x)
  if (!b) return(c(se = NA_real_, llci = NA_real_, ulci = NA_real_))

  alpha <- 1 - conf
  lo <- round(b * alpha / 2)
  hi <- trunc(b * conf + b * alpha / 2) + 1L
  lo <- max(1L, lo)
  hi <- min(b, hi)

  c(
    se = stats::sd(x),
    llci = x[lo],
    ulci = x[hi]
  )
}

.pd_indirect_standardization_scale <- function(d, spec) {
  ysd <- stats::sd(d[[spec$y]])
  if (!is.finite(ysd) || ysd == 0) return(NA_real_)
  xmult <- if (.pd_is_binary_numeric(d[[spec$x]])) 1 else stats::sd(d[[spec$x]])
  xmult / ysd
}

.pd_boot_indirect <- function(data, spec, indirect_template, boot, conf,
                              seed = NULL, progress = FALSE,
                              standardized = TRUE) {
  if (!nrow(indirect_template)) {
    return(list(indirect = indirect_template, total = NULL,
                draws = NULL, std_draws = NULL))
  }

  d <- .pd_complete_data(data, spec$nodes)
  obs_scale <- if (standardized) .pd_indirect_standardization_scale(d, spec) else NA_real_
  indirect_template$std_effect <- if (standardized) {
    indirect_template$effect * obs_scale
  } else {
    NA_real_
  }

  if (boot <= 0L) {
    indirect_template$boot_se <- NA_real_
    indirect_template$boot_llci <- NA_real_
    indirect_template$boot_ulci <- NA_real_
    indirect_template$std_boot_se <- NA_real_
    indirect_template$std_boot_llci <- NA_real_
    indirect_template$std_boot_ulci <- NA_real_

    total <- data.frame(
      id = "total",
      path = "TOTAL INDIRECT",
      effect = sum(indirect_template$effect, na.rm = TRUE),
      boot_se = NA_real_,
      boot_llci = NA_real_,
      boot_ulci = NA_real_,
      std_effect = if (standardized) sum(indirect_template$effect, na.rm = TRUE) * obs_scale else NA_real_,
      std_boot_se = NA_real_,
      std_boot_llci = NA_real_,
      std_boot_ulci = NA_real_,
      stringsAsFactors = FALSE
    )
    return(list(indirect = indirect_template, total = total,
                draws = NULL, std_draws = NULL))
  }

  if (!is.null(seed)) set.seed(seed)

  ipaths <- .pd_directed_paths(spec)
  sims <- matrix(NA_real_, nrow = boot, ncol = length(ipaths))
  std_sims <- if (standardized) {
    matrix(NA_real_, nrow = boot, ncol = length(ipaths))
  } else {
    NULL
  }
  n <- nrow(d)

  good <- 0L
  attempts <- 0L
  max_attempts <- max(2L * boot, boot)

  while (good < boot && attempts < max_attempts) {
    attempts <- attempts + 1L
    idx <- as.integer(trunc(stats::runif(n) * n) + 1L)
    db <- d[idx, , drop = FALSE]

    one <- try(
      .pd_fit_paths(
        db,
        spec,
        conf = conf,
        standardized = FALSE
      )$paths,
      silent = TRUE
    )
    if (inherits(one, "try-error")) next

    vals <- vapply(
      ipaths,
      .pd_effect_for_index_path,
      numeric(1),
      spec = spec,
      path_table = one,
      column = "estimate"
    )
    if (any(!is.finite(vals))) next

    if (standardized) {
      bscale <- .pd_indirect_standardization_scale(db, spec)
      if (!is.finite(bscale)) next
    }

    good <- good + 1L
    sims[good, ] <- vals
    if (standardized) std_sims[good, ] <- vals * bscale

    if (progress && (good %% max(1L, floor(boot / 20L)) == 0L)) {
      message("Bootstrap: ", good, "/", boot)
    }
  }

  if (good < boot) {
    .pd_stop(
      "Only ", good, " usable bootstrap samples were obtained out of ",
      boot, " requested."
    )
  }

  stats_by_path <- t(apply(sims, 2L, .pd_process_percentile_ci, conf = conf))
  indirect_template$boot_se <- stats_by_path[, "se"]
  indirect_template$boot_llci <- stats_by_path[, "llci"]
  indirect_template$boot_ulci <- stats_by_path[, "ulci"]

  if (standardized) {
    std_stats <- t(apply(std_sims, 2L, .pd_process_percentile_ci, conf = conf))
    indirect_template$std_boot_se <- std_stats[, "se"]
    indirect_template$std_boot_llci <- std_stats[, "llci"]
    indirect_template$std_boot_ulci <- std_stats[, "ulci"]
  } else {
    indirect_template$std_boot_se <- NA_real_
    indirect_template$std_boot_llci <- NA_real_
    indirect_template$std_boot_ulci <- NA_real_
  }

  total_draws <- rowSums(sims)
  total_stats <- .pd_process_percentile_ci(total_draws, conf = conf)

  if (standardized) {
    std_total_draws <- rowSums(std_sims)
    std_total_stats <- .pd_process_percentile_ci(std_total_draws, conf = conf)
  } else {
    std_total_stats <- c(se = NA_real_, llci = NA_real_, ulci = NA_real_)
  }

  total_effect <- sum(indirect_template$effect, na.rm = TRUE)
  total <- data.frame(
    id = "total",
    path = "TOTAL INDIRECT",
    effect = total_effect,
    boot_se = unname(total_stats["se"]),
    boot_llci = unname(total_stats["llci"]),
    boot_ulci = unname(total_stats["ulci"]),
    std_effect = if (standardized) total_effect * obs_scale else NA_real_,
    std_boot_se = unname(std_total_stats["se"]),
    std_boot_llci = unname(std_total_stats["llci"]),
    std_boot_ulci = unname(std_total_stats["ulci"]),
    stringsAsFactors = FALSE
  )

  list(indirect = indirect_template, total = total,
       draws = sims, std_draws = std_sims)
}

#' Fit a PROCESS-style diagram model using the native processdiagram engine
#'
#' This independent implementation is useful for validation and for analyses that
#' do not need to call PROCESS itself. When reproducing a PROCESS analysis for a
#' figure, `process_for_diagram()` is the preferred workflow because the figure
#' then uses PROCESS's own displayed results as the authoritative values.
#'
#' @param data Data frame.
#' @param x,y,m Variable names.
#' @param model PROCESS model number (4, 6, or 81 initially).
#' @param bmatrix Optional custom b-matrix vector; overrides `model`.
#' @param standardized Whether to calculate PROCESS-style standardized coefficients.
#' @param boot Number of percentile-bootstrap samples for indirect effects. Use 0 to skip.
#' @param conf Confidence level as a proportion, e.g. `.95`.
#' @param seed Optional bootstrap seed. If omitted when `boot > 0`, a seed is
#'   generated once and stored in the returned object's provenance.
#' @param progress Print bootstrap progress.
#' @param process_digits Decimal places used in PROCESS-compatible standardization.
#' @return A `process_diagram_fit` object.
#' @seealso [process_for_diagram()], [import_process_save2()], [draw_process_diagram()]
#' @examples
#' fit <- fit_process_diagram(
#'   mtcars, x = "am", y = "mpg", m = "wt", model = 4,
#'   standardized = TRUE, boot = 0
#' )
#' fit$paths
#' indirect_effects(fit)
#' @export
fit_process_diagram <- function(data, x, y, m, model = NULL, bmatrix = NULL,
                                standardized = TRUE, boot = 0L, conf = .95,
                                seed = NULL, progress = FALSE,
                                process_digits = 4L) {
  boot <- as.integer(boot)
  seed_source <- NULL
  if (boot > 0L && is.null(seed)) {
    seed <- .pd_generate_seed()
    seed_source <- "generated by processdiagram"
  } else if (!is.null(seed)) {
    seed <- as.integer(abs(seed))[1L]
    seed_source <- "supplied by user"
  }

  spec <- process_spec(x = x, y = y, m = m, model = model, bmatrix = bmatrix)
  standardized_requested <- isTRUE(standardized)
  binary_y <- .pd_is_binary_numeric(data[[y]])
  if (binary_y && standardized_requested) {
    standardized <- FALSE
    warning(
      "PROCESS does not provide standardized coefficients when Y is dichotomous; ",
      "using unstandardized coefficients. Paths predicting Y are logistic ",
      "regression coefficients in log-odds units.",
      call. = FALSE
    )
  }
  fit <- .pd_fit_paths(
    data, spec, conf = conf, standardized = standardized,
    process_digits = process_digits
  )
  ind_raw <- .pd_indirect_point(spec, fit$paths)

  boot_result <- .pd_boot_indirect(
    fit$data,
    spec,
    ind_raw,
    boot = boot,
    conf = conf,
    seed = seed,
    progress = progress,
    standardized = standardized
  )

  indirect <- boot_result$indirect
  if (nrow(indirect)) indirect <- rbind(boot_result$total, indirect)

  out <- list(
    call = match.call(),
    spec = spec,
    paths = fit$paths,
    indirect = indirect,
    models = fit$models,
    n = nrow(fit$data),
    standardized = standardized,
    standardized_requested = standardized_requested,
    outcome_type = if (binary_y) "binary" else "continuous",
    conf = conf,
    boot = boot,
    provenance = .pd_provenance(
      engine = "processdiagram-native",
      seed = seed,
      seed_source = seed_source,
      extra = list(
        process_digits = as.integer(process_digits),
        standardized_requested = standardized_requested,
        standardized_effective = standardized,
        outcome_type = if (binary_y) "binary" else "continuous"
      )
    )
  )
  class(out) <- "process_diagram_fit"
  out
}

#' @export
print.process_diagram_fit <- function(x, ...) {
  cat("processdiagram fit\n")
  cat("  Engine:", if (!is.null(x$provenance$engine)) x$provenance$engine else "unknown", "\n")
  cat("  Model:", x$spec$model, "\n")
  cat("  N:", x$n, "\n")
  if (!is.null(x$provenance$seed)) cat("  Seed:", x$provenance$seed, "\n")
  if (!is.null(x$validation)) {
    cat("  PROCESS validation:", if (isTRUE(x$validation$ok)) "PASS" else "FAIL", "\n")
  }
  cat("\n")
  print(x$paths, row.names = FALSE)
  if (nrow(x$indirect)) {
    cat("\nIndirect effects:\n")
    print(x$indirect, row.names = FALSE)
  }
  invisible(x)
}

#' Extract indirect effects
#'
#' Extracts the total and specific indirect effects from a fitted or imported
#' processdiagram model.
#'
#' @param x A `process_diagram_fit` object.
#' @return The indirect-effect data frame stored in `x$indirect`.
#' @seealso [fit_process_diagram()], [process_for_diagram()], [import_process_save2()]
#' @examples
#' fit <- fit_process_diagram(
#'   mtcars, x = "am", y = "mpg", m = "wt", model = 4,
#'   standardized = TRUE, boot = 0
#' )
#' indirect_effects(fit)
#' @export
indirect_effects <- function(x) {
  if (!inherits(x, "process_diagram_fit")) {
    .pd_stop("x must be a process_diagram_fit object.")
  }
  x$indirect
}
