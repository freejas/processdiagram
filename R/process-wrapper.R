# PROCESS integration -------------------------------------------------------

.pd_find_process_function <- function(process_fun = NULL, env = parent.frame()) {
  if (!is.null(process_fun)) {
    if (!is.function(process_fun)) .pd_stop("process_fun must be a function.")
    return(process_fun)
  }
  if (exists("process", envir = env, inherits = TRUE)) {
    fun <- get("process", envir = env, inherits = TRUE)
    if (is.function(fun)) return(fun)
  }
  if (exists("process", envir = .GlobalEnv, inherits = FALSE)) {
    fun <- get("process", envir = .GlobalEnv, inherits = FALSE)
    if (is.function(fun)) return(fun)
  }
  .pd_stop(
    "Could not find a loaded process() function. Source PROCESS first, or supply process_fun=."
  )
}


.pd_run_process_capture <- function(process_fun, args, show_output = TRUE) {
  tf <- tempfile(fileext = ".txt")
  con <- file(tf, open = "wt")
  old_sinks <- sink.number(type = "output")
  sink(con, split = isTRUE(show_output))
  on.exit({
    while (sink.number(type = "output") > old_sinks) sink(type = "output")
    try(close(con), silent = TRUE)
    try(unlink(tf), silent = TRUE)
  }, add = TRUE)

  result <- do.call(process_fun, args)
  while (sink.number(type = "output") > old_sinks) sink(type = "output")
  close(con)
  output <- readLines(tf, warn = FALSE)
  unlink(tf)
  list(result = result, output = output)
}

.pd_process_digits_from_decimals <- function(decimals) {
  if (is.null(decimals) || !length(decimals) || !is.finite(decimals[1L])) return(4L)
  txt <- format(decimals[1L], scientific = FALSE, trim = TRUE)
  if (!grepl("\\.", txt)) return(0L)
  frac <- sub("^[^.]*\\.", "", txt)
  out <- suppressWarnings(as.integer(frac))
  if (is.na(out)) 4L else out
}

.pd_normalize_process_output <- function(lines) {
  trimws(gsub("\\r", "", lines))
}

.pd_process_version_from_output <- function(lines) {
  # PROCESS 4.x used "PROCESS for R Version ..." whereas PROCESS 5.0 uses
  # "PROCESS Procedure for R Version ...". Accept both banner forms.
  hit <- grep("PROCESS(?: Procedure)? for R Version", lines, value = TRUE, perl = TRUE)
  if (!length(hit)) return(NA_character_)
  sub(
    ".*PROCESS(?: Procedure)? for R Version\\s+([^[:space:]*]+).*",
    "\\1", hit[1L], perl = TRUE
  )
}

.pd_process_major <- function(version) {
  if (is.null(version) || !length(version) || is.na(version[1L])) return(NA_integer_)
  out <- suppressWarnings(as.integer(sub("^([0-9]+).*", "\\1", as.character(version[1L]))))
  if (is.na(out)) NA_integer_ else out
}

.pd_process_sample_size_from_output <- function(lines) {
  hit <- grep("^Sample size:", lines, value = TRUE)
  if (!length(hit)) return(NA_integer_)
  val <- suppressWarnings(as.integer(trimws(sub("^Sample size:\\s*", "", hit[1L]))))
  if (length(val) && is.finite(val)) val else NA_integer_
}

.pd_numeric_row <- function(line) {
  toks <- strsplit(trimws(line), "\\s+")[[1L]]
  if (!length(toks)) return(NULL)
  nums <- suppressWarnings(as.numeric(toks))
  first <- which(!is.na(nums))[1L]
  if (is.na(first)) return(NULL)
  list(
    label = if (first > 1L) paste(toks[seq_len(first - 1L)], collapse = " ") else "",
    values = nums[first:length(nums)]
  )
}

.pd_line_for_name <- function(lines, name) {
  z <- trimws(lines)
  exact <- which(z == name)
  if (length(exact)) return(exact[1L])
  pref <- paste0(name, " ")
  hit <- which(startsWith(z, pref))
  if (length(hit)) hit[1L] else NA_integer_
}

.pd_parse_process_paths <- function(lines, spec, data = NULL, standardized = TRUE) {
  lines <- .pd_normalize_process_output(lines)
  out_starts <- grep("^Outcome Variable:\\s*", lines)
  if (!length(out_starts)) .pd_stop("Could not locate PROCESS outcome-model sections.")
  out_names <- trimws(sub("^Outcome Variable:\\s*", "", lines[out_starts]))
  rows <- list()
  rid <- 0L
  x_binary <- !is.null(data) && .pd_is_binary_numeric(data[[spec$x]])
  saw_legacy_std <- FALSE
  saw_scale_free <- FALSE

  for (outcome in spec$nodes[-1L]) {
    which_out <- which(out_names == outcome)
    if (!length(which_out)) {
      .pd_stop("Could not locate PROCESS output for outcome variable '", outcome, "'.")
    }
    pos <- which_out[1L]
    start <- out_starts[pos]
    end <- if (pos < length(out_starts)) out_starts[pos + 1L] - 1L else length(lines)
    sec <- lines[start:end]

    model_idx <- which(sec == "Model:")[1L]
    if (is.na(model_idx)) .pd_stop("Could not locate regression table for outcome '", outcome, "'.")

    # PROCESS 4.x printed a one-column "Standardized coefficients" table.
    # PROCESS 5.0 replaced it with a six-column scale-free association table.
    legacy_std_idx <- grep("^Standardized coefficients:", sec)[1L]
    scale_idx <- grep("^Scale-free and standardized measures of association:", sec)[1L]
    std_candidates <- c(legacy_std_idx, scale_idx)
    std_candidates <- std_candidates[!is.na(std_candidates)]
    reg_end <- if (length(std_candidates)) min(std_candidates) - 1L else length(sec)
    reg_sec <- sec[model_idx:reg_end]

    outcome_idx <- match(outcome, spec$nodes)
    predictors <- spec$nodes[which(spec$adjacency[outcome_idx, ] == 1L)]

    for (pred in predictors) {
      line_idx <- .pd_line_for_name(reg_sec, pred)
      if (is.na(line_idx)) {
        .pd_stop("Could not parse PROCESS coefficient for '", pred, " -> ", outcome, "'.")
      }
      parsed <- .pd_numeric_row(reg_sec[line_idx])
      if (is.null(parsed) || length(parsed$values) < 6L) {
        .pd_stop("PROCESS coefficient row had an unexpected format for '", pred, " -> ", outcome, "'.")
      }

      std_est <- NA_real_
      if (standardized) {
        if (!is.na(legacy_std_idx)) {
          saw_legacy_std <- TRUE
          std_sec <- sec[legacy_std_idx:length(sec)]
          sidx <- .pd_line_for_name(std_sec, pred)
          if (is.na(sidx)) {
            .pd_stop("Could not parse PROCESS standardized coefficient for '", pred, " -> ", outcome, "'.")
          }
          sparsed <- .pd_numeric_row(std_sec[sidx])
          if (is.null(sparsed) || !length(sparsed$values)) {
            .pd_stop("Unexpected standardized-coefficient row for '", pred, " -> ", outcome, "'.")
          }
          std_est <- sparsed$values[1L]
        } else if (!is.na(scale_idx)) {
          saw_scale_free <- TRUE
          std_sec <- sec[scale_idx:length(sec)]
          sidx <- .pd_line_for_name(std_sec, pred)
          if (is.na(sidx)) {
            .pd_stop("Could not parse PROCESS 5 standardized measures for '", pred, " -> ", outcome, "'.")
          }
          sparsed <- .pd_numeric_row(std_sec[sidx])
          if (is.null(sparsed) || length(sparsed$values) < 6L) {
            .pd_stop("Unexpected PROCESS 5 scale-free row for '", pred, " -> ", outcome, "'.")
          }
          # PROCESS 4.x did not scale a dichotomous focal X by SD(X).  To keep
          # diagrams comparable across versions, use PROCESS 5's standY for
          # such X paths and standYX for all other predictors.
          std_est <- if (identical(pred, spec$x) && isTRUE(x_binary)) {
            sparsed$values[5L]  # standY
          } else {
            sparsed$values[4L]  # standYX
          }
        } else {
          .pd_stop(
            "PROCESS did not print a recognized standardized-coefficient table although standardized=TRUE."
          )
        }
      }

      rid <- rid + 1L
      rows[[rid]] <- data.frame(
        id = paste0("p", rid),
        from = pred,
        to = outcome,
        estimate = parsed$values[1L],
        std_estimate = std_est,
        se = parsed$values[2L],
        statistic = parsed$values[3L],
        p = parsed$values[4L],
        llci = parsed$values[5L],
        ulci = parsed$values[6L],
        stringsAsFactors = FALSE
      )
    }
  }
  out <- do.call(rbind, rows)
  attr(out, "standardization_style") <- if (saw_scale_free) {
    "process5_scale_free"
  } else if (saw_legacy_std) {
    "process4_legacy"
  } else {
    "none"
  }
  out
}

.pd_parse_process_indirect_table <- function(lines, heading_regex) {
  lines <- .pd_normalize_process_output(lines)
  h <- grep(heading_regex, lines)
  if (!length(h)) return(data.frame())
  i <- h[1L] + 1L
  rows <- list()
  started <- FALSE

  while (i <= length(lines)) {
    ln <- lines[i]
    if (!nzchar(ln)) {
      if (started) break
      i <- i + 1L
      next
    }
    if (startsWith(ln, "*****")) break
    parsed <- .pd_numeric_row(ln)

    # Once numeric table rows have begun, the first subsequent non-data line
    # marks the end of this table. This is important because PROCESS may print
    # the unstandardized and standardized indirect-effect tables back-to-back;
    # a parser must not accidentally absorb rows from the next table.
    if (started && (is.null(parsed) || !nzchar(parsed$label))) break

    if (!is.null(parsed) && nzchar(parsed$label)) {
      vals <- parsed$values
      if (length(vals) >= 1L) {
        started <- TRUE
        rows[[length(rows) + 1L]] <- data.frame(
          label = parsed$label,
          effect = vals[1L],
          boot_se = if (length(vals) >= 2L) vals[2L] else NA_real_,
          boot_llci = if (length(vals) >= 3L) vals[3L] else NA_real_,
          boot_ulci = if (length(vals) >= 4L) vals[4L] else NA_real_,
          stringsAsFactors = FALSE
        )
      }
    }
    i <- i + 1L
  }

  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

.pd_parse_process_output <- function(lines, spec, data, standardized = TRUE) {
  lines <- .pd_normalize_process_output(lines)
  paths <- .pd_parse_process_paths(lines, spec, data = data, standardized = standardized)
  standardization_style <- attr(paths, "standardization_style")
  attr(paths, "standardization_style") <- NULL
  raw_indirect <- .pd_parse_process_indirect_table(
    lines,
    "^Indirect effect\\(s\\) of X on Y:$"
  )

  std_indirect <- data.frame()
  if (standardized) {
    if (.pd_is_binary_numeric(data[[spec$x]])) {
      std_indirect <- .pd_parse_process_indirect_table(
        lines,
        "^Partially standardized( \\(StandY\\))? indirect effect\\(s\\) of X on Y:$"
      )
    } else {
      std_indirect <- .pd_parse_process_indirect_table(
        lines,
        "^Completely standardized( \\(StandYX\\))? indirect effect\\(s\\) of X on Y:$"
      )
      if (!nrow(std_indirect)) {
        std_indirect <- .pd_parse_process_indirect_table(
          lines,
          "^Partially standardized( \\(StandY\\))? indirect effect\\(s\\) of X on Y:$"
        )
      }
    }
  }

  list(
    paths = paths,
    indirect = raw_indirect,
    standardized_indirect = std_indirect,
    process_version = .pd_process_version_from_output(lines),
    standardization_style = standardization_style,
    n = .pd_process_sample_size_from_output(lines)
  )
}

.pd_refresh_native_standardization <- function(native, data, spec) {
  if (!nrow(native$paths)) return(native)
  out <- native
  for (i in seq_len(nrow(out$paths))) {
    pred <- out$paths$from[i]
    outcome <- out$paths$to[i]
    pred_sd <- if (identical(pred, spec$x) && .pd_is_binary_numeric(data[[pred]])) {
      1
    } else {
      stats::sd(data[[pred]])
    }
    out_sd <- stats::sd(data[[outcome]])
    out$paths$std_estimate[i] <- out$paths$estimate[i] * pred_sd / out_sd
  }
  out
}

.pd_auto_tolerance <- function(digits) {
  0.51 * 10^(-as.integer(digits)) + 1e-12
}

.pd_align_process_indirect <- function(native, process, tolerance) {
  if (!nrow(native) || !nrow(process)) return(integer())
  map <- rep(NA_integer_, nrow(native))

  native_total <- which(native$id == "total")
  process_total <- which(toupper(process$label) == "TOTAL")
  if (length(native_total) && length(process_total)) {
    map[native_total[1L]] <- process_total[1L]
  }

  ni <- setdiff(seq_len(nrow(native)), native_total)
  pi <- setdiff(seq_len(nrow(process)), process_total)

  # PROCESS omits a separate TOTAL row when there is only one indirect path,
  # because TOTAL and that one specific indirect effect are identical. Our
  # semantic object retains both concepts, so map both native rows to the same
  # PROCESS row in this special case.
  if (length(native_total) == 1L && !length(process_total) &&
      length(ni) == 1L && nrow(process) == 1L) {
    map[native_total] <- 1L
    map[ni] <- 1L
    return(map)
  }

  if (length(ni) != length(pi)) return(map)

  remaining <- pi
  for (i in ni) {
    if (!length(remaining)) break
    d <- abs(process$effect[remaining] - native$effect[i])
    j <- remaining[which.min(d)]
    if (is.finite(abs(process$effect[j] - native$effect[i])) &&
        abs(process$effect[j] - native$effect[i]) <= max(tolerance * 2, 1e-8)) {
      map[i] <- j
      remaining <- setdiff(remaining, j)
    }
  }
  map
}

.pd_validation_row <- function(component, item, field, native, process, tolerance) {
  diff <- abs(native - process)
  ok <- (is.na(native) && is.na(process)) ||
    (is.finite(diff) && diff <= tolerance)
  data.frame(
    component = component,
    item = item,
    field = field,
    native = native,
    process = process,
    abs_diff = diff,
    tolerance = tolerance,
    ok = ok,
    stringsAsFactors = FALSE
  )
}

.pd_validate_process_result <- function(native, parsed, digits = 4L,
                                        standardized = TRUE,
                                        validate_indirect = TRUE,
                                        tolerance = NULL) {
  tol <- if (is.null(tolerance)) .pd_auto_tolerance(digits) else as.numeric(tolerance)[1L]
  details <- list()
  k <- 0L

  fields <- c("estimate", "se", "statistic", "p", "llci", "ulci")
  if (standardized) fields <- c(fields, "std_estimate")

  for (i in seq_len(nrow(native$paths))) {
    nrowi <- native$paths[i, , drop = FALSE]
    hit <- parsed$paths$from == nrowi$from & parsed$paths$to == nrowi$to
    if (sum(hit) != 1L) {
      k <- k + 1L
      details[[k]] <- data.frame(
        component = "path", item = paste(nrowi$from, "->", nrowi$to),
        field = "row", native = NA_real_, process = NA_real_, abs_diff = Inf,
        tolerance = tol, ok = FALSE, stringsAsFactors = FALSE
      )
      next
    }
    prow <- parsed$paths[hit, , drop = FALSE]
    for (field in fields) {
      k <- k + 1L
      details[[k]] <- .pd_validation_row(
        "path", paste(nrowi$from, "->", nrowi$to), field,
        nrowi[[field]], prow[[field]], tol
      )
    }
  }

  indirect_map <- integer()
  if (validate_indirect && nrow(native$indirect)) {
    if (!nrow(parsed$indirect)) {
      k <- k + 1L
      details[[k]] <- data.frame(
        component = "indirect", item = "table", field = "present",
        native = NA_real_, process = NA_real_, abs_diff = Inf,
        tolerance = tol, ok = FALSE, stringsAsFactors = FALSE
      )
    } else {
      indirect_map <- .pd_align_process_indirect(native$indirect, parsed$indirect, tol)
      for (i in seq_len(nrow(native$indirect))) {
        j <- indirect_map[i]
        item <- native$indirect$path[i]
        if (is.na(j)) {
          k <- k + 1L
          details[[k]] <- data.frame(
            component = "indirect", item = item, field = "row",
            native = NA_real_, process = NA_real_, abs_diff = Inf,
            tolerance = tol, ok = FALSE, stringsAsFactors = FALSE
          )
        } else {
          for (field in c("effect", "boot_se", "boot_llci", "boot_ulci")) {
            # In the default paths-only validation, native bootstrap columns are
            # intentionally NA because PROCESS is the sole bootstrap engine.
            if (field != "effect" && is.na(native$indirect[[field]][i])) next
            k <- k + 1L
            details[[k]] <- .pd_validation_row(
              "indirect", item, field,
              native$indirect[[field]][i], parsed$indirect[[field]][j], tol
            )
          }

          if (standardized && nrow(parsed$standardized_indirect)) {
            sj <- match(parsed$indirect$label[j], parsed$standardized_indirect$label)
            if (!is.na(sj)) {
              std_fields <- c(
                std_effect = "effect",
                std_boot_se = "boot_se",
                std_boot_llci = "boot_llci",
                std_boot_ulci = "boot_ulci"
              )
              for (nf in names(std_fields)) {
                if (nf != "std_effect" && is.na(native$indirect[[nf]][i])) next
                k <- k + 1L
                details[[k]] <- .pd_validation_row(
                  "standardized indirect", item, nf,
                  native$indirect[[nf]][i],
                  parsed$standardized_indirect[[std_fields[[nf]]]][sj], tol
                )
              }
            }
          }
        }
      }
    }
  }

  detail_df <- if (length(details)) do.call(rbind, details) else data.frame()
  list(
    ok = nrow(detail_df) == 0L || all(detail_df$ok),
    tolerance = tol,
    details = detail_df,
    indirect_map = indirect_map
  )
}

.pd_apply_process_values <- function(native, parsed, validation, standardized = TRUE) {
  out <- native

  for (i in seq_len(nrow(out$paths))) {
    hit <- parsed$paths$from == out$paths$from[i] & parsed$paths$to == out$paths$to[i]
    if (sum(hit) != 1L) next
    for (field in c("estimate", "se", "statistic", "p", "llci", "ulci")) {
      out$paths[[field]][i] <- parsed$paths[[field]][hit]
    }
    if (standardized) out$paths$std_estimate[i] <- parsed$paths$std_estimate[hit]
  }

  if (nrow(out$indirect) && nrow(parsed$indirect)) {
    map <- validation$indirect_map
    if (!length(map)) {
      map <- .pd_align_process_indirect(
        out$indirect, parsed$indirect, validation$tolerance
      )
    }

    std_by_label <- NULL
    if (standardized && nrow(parsed$standardized_indirect)) {
      std_by_label <- parsed$standardized_indirect
    }

    for (i in seq_len(nrow(out$indirect))) {
      j <- map[i]
      if (is.na(j)) next
      out$indirect$effect[i] <- parsed$indirect$effect[j]
      out$indirect$boot_se[i] <- parsed$indirect$boot_se[j]
      out$indirect$boot_llci[i] <- parsed$indirect$boot_llci[j]
      out$indirect$boot_ulci[i] <- parsed$indirect$boot_ulci[j]

      if (!is.null(std_by_label)) {
        sj <- match(parsed$indirect$label[j], std_by_label$label)
        if (!is.na(sj)) {
          out$indirect$std_effect[i] <- std_by_label$effect[sj]
          out$indirect$std_boot_se[i] <- std_by_label$boot_se[sj]
          out$indirect$std_boot_llci[i] <- std_by_label$boot_llci[sj]
          out$indirect$std_boot_ulci[i] <- std_by_label$boot_ulci[sj]
        }
      }
    }
  }

  out
}

#' Run PROCESS and create a diagram-ready, reproducible fit
#'
#' This is the preferred workflow when the figure is intended to represent a
#' PROCESS analysis. The function calls the user's already-loaded `process()`
#' function with an explicit seed and `save = 2`, captures PROCESS's displayed
#' results, validates the supported path model against the native processdiagram
#' engine, and then uses PROCESS's own displayed values as the authoritative
#' values stored in the returned object.
#'
#' The PROCESS source code is not bundled with or redistributed by this package.
#' Obtain PROCESS separately from <https://afhayes.com/download.html>.
#'
#' @param data Data frame supplied to PROCESS.
#' @param x,y,m Variable names.
#' @param model PROCESS model number (4, 6, or 81 initially).
#' @param bmatrix Optional custom lower-triangular b-matrix vector.
#' @param standardized Whether to request `stand = 1` from PROCESS.
#' @param boot Number of PROCESS bootstrap samples.
#' @param conf Confidence level as a proportion, e.g. `.95`.
#' @param seed Optional explicit seed. If omitted, processdiagram generates one
#'   once, passes it to PROCESS, and stores it in the returned object.
#' @param process_fun Optional PROCESS function. Normally omitted after sourcing PROCESS.
#' @param process_args Named list of additional arguments passed to PROCESS. Core
#'   arguments controlled by this wrapper cannot be overridden.
#' @param validation One of `"paths"`, `"full"`, or `"none"`. `"paths"` is the
#'   default and performs a cheap independent refit of paths and indirect point
#'   estimates. `"full"` also repeats the bootstrap using the native engine.
#' @param tolerance Optional absolute comparison tolerance. By default it is
#'   derived from PROCESS's displayed decimal precision.
#' @param show_process_output Replay the captured PROCESS output to the console.
#' @param progress Show PROCESS bootstrap progress and, when `validation = "full"`, native bootstrap progress.
#' @param strict If TRUE, stop rather than returning an unvalidated object when
#'   parsing or numerical validation fails.
#' @return A `process_diagram_fit` object whose displayed figure values come from PROCESS.
#' @details
#' PROCESS for R 4.1 and 5.0 are supported for the path models currently handled
#' by this package. PROCESS 5.0 replaced the old one-column standardized-coefficient
#' table with a scale-free association table. processdiagram maps PROCESS 5's
#' `standY` value to paths from a dichotomous focal X (matching PROCESS 4.1 figure
#' semantics) and uses `standYX` for other standardized paths.
#'
#' When Y is dichotomous, PROCESS disables `stand = 1`. The returned object records
#' both the requested and effective standardization state in `$provenance`, and
#' diagram functions default to the effective state.
#' @seealso [fit_process_diagram()], [import_process_save2()], [save_process_diagram()]
#' @examples
#' \dontrun{
#' # First source PROCESS so that process() is available in the R session.
#' fit <- process_for_diagram(
#'   mtcars, x = "am", y = "mpg", m = "wt", model = 4,
#'   standardized = TRUE, boot = 5000
#' )
#' fit$validation$ok
#' fit$provenance$seed
#' plot(fit)
#' }
#' @export
process_for_diagram <- function(data, x, y, m, model = NULL, bmatrix = NULL,
                                standardized = TRUE, boot = 5000L, conf = .95,
                                seed = NULL, process_fun = NULL,
                                process_args = list(),
                                validation = c("paths", "full", "none"),
                                tolerance = NULL,
                                show_process_output = TRUE,
                                progress = FALSE, strict = TRUE) {
  validation <- match.arg(validation)
  process_fun <- .pd_find_process_function(process_fun, env = parent.frame())
  boot <- as.integer(boot)

  seed_source <- if (is.null(seed)) "generated by processdiagram" else "supplied by user"
  if (is.null(seed)) seed <- .pd_generate_seed()
  seed <- as.integer(abs(seed))[1L]

  if (!is.list(process_args) || (length(process_args) && is.null(names(process_args)))) {
    .pd_stop("process_args must be a named list.")
  }

  reserved <- c(
    "data", "x", "y", "m", "model", "bmatrix", "stand", "boot", "conf",
    "seed", "save", "progress", "outscreen"
  )
  bad <- intersect(names(process_args), reserved)
  if (length(bad)) {
    .pd_stop(
      "process_args cannot override wrapper-controlled arguments: ",
      paste(bad, collapse = ", ")
    )
  }

  proc_decimals <- if (!is.null(process_args$decimals)) process_args$decimals else 9.4
  process_digits <- .pd_process_digits_from_decimals(proc_decimals)

  proc_call <- list(
    data = data,
    x = x,
    y = y,
    m = m,
    stand = as.integer(isTRUE(standardized)),
    boot = boot,
    conf = conf * 100,
    seed = seed,
    save = 2,
    progress = as.integer(isTRUE(progress)),
    outscreen = 1
  )
  if (!is.null(model)) proc_call$model <- model
  if (!is.null(bmatrix)) proc_call$bmatrix <- bmatrix
  proc_call <- c(proc_call, process_args)

  captured <- .pd_run_process_capture(
    process_fun, proc_call, show_output = show_process_output
  )
  process_result <- captured$result
  process_output <- captured$output

  spec <- process_spec(x = x, y = y, m = m, model = model, bmatrix = bmatrix)
  d <- .pd_complete_data(data, spec$nodes)
  binary_y <- .pd_is_binary_numeric(d[[spec$y]])
  standardized_requested <- isTRUE(standardized)
  effective_standardized <- standardized_requested && !binary_y

  if (binary_y && standardized_requested) {
    message(
      "PROCESS disables stand=1 when Y is dichotomous. Using unstandardized ",
      "coefficients and indirect effects; paths predicting Y are logistic ",
      "regression coefficients in log-odds units."
    )
  }

  parsed <- try(
    .pd_parse_process_output(
      process_output, spec = spec, data = d, standardized = effective_standardized
    ),
    silent = TRUE
  )

  if (inherits(parsed, "try-error")) {
    msg <- paste0(
      "PROCESS ran, but processdiagram could not parse the captured output. ",
      "No figure should be treated as PROCESS-validated. Parser error: ",
      as.character(parsed)[1L]
    )
    if (strict) .pd_stop(msg)
    warning(msg, call. = FALSE)
    parsed <- NULL
  }

  native_boot <- if (identical(validation, "full")) boot else 0L
  native <- fit_process_diagram(
    data = data,
    x = x,
    y = y,
    m = m,
    model = model,
    bmatrix = bmatrix,
    standardized = effective_standardized,
    boot = native_boot,
    conf = conf,
    seed = seed,
    progress = progress,
    process_digits = process_digits
  )

  # PROCESS 5.0 reports standardized path measures using full-precision
  # coefficients in its scale-free association table. Match that convention
  # before performing numerical validation.
  if (!is.null(parsed) && effective_standardized &&
      identical(parsed$standardization_style, "process5_scale_free")) {
    native <- .pd_refresh_native_standardization(native, d, spec)
  }

  validation_result <- list(
    ok = NA,
    mode = validation,
    details = data.frame(),
    tolerance = if (is.null(tolerance)) .pd_auto_tolerance(process_digits) else tolerance,
    indirect_map = integer()
  )

  if (!identical(validation, "none") && !is.null(parsed)) {
    validation_result <- .pd_validate_process_result(
      native,
      parsed,
      digits = process_digits,
      standardized = effective_standardized,
      validate_indirect = TRUE,
      tolerance = tolerance
    )
    validation_result$mode <- validation

    if (!isTRUE(validation_result$ok)) {
      bad_rows <- validation_result$details[!validation_result$details$ok, , drop = FALSE]
      msg <- paste0(
        "PROCESS and processdiagram did not agree within tolerance. ",
        "Diagram generation is blocked in strict mode. First mismatch: ",
        if (nrow(bad_rows)) {
          paste0(
            bad_rows$component[1L], " / ", bad_rows$item[1L], " / ",
            bad_rows$field[1L], " (PROCESS=", bad_rows$process[1L],
            ", native=", bad_rows$native[1L], ")"
          )
        } else {
          "unknown"
        }
      )
      if (strict) .pd_stop(msg)
      warning(msg, call. = FALSE)
    }
  }

  if (!is.null(parsed)) {
    native <- .pd_apply_process_values(
      native, parsed, validation_result, standardized = effective_standardized
    )
  }

  native$process_result <- process_result
  native$process_output <- process_output
  native$validation <- validation_result
  native$provenance <- .pd_provenance(
    engine = "PROCESS",
    seed = seed,
    seed_source = seed_source,
    extra = list(
      process_version = if (!is.null(parsed)) parsed$process_version else NA_character_,
      process_digits = process_digits,
      process_result_save = 2L,
      values_source = "captured PROCESS output",
      standardization_source = if (!is.null(parsed)) parsed$standardization_style else NA_character_,
      validation_mode = validation,
      standardized_requested = standardized_requested,
      standardized_effective = effective_standardized,
      outcome_type = if (binary_y) "binary" else "continuous"
    )
  )

  if (!is.null(parsed) && is.finite(parsed$n)) native$n <- parsed$n
  native$standardized_requested <- standardized_requested
  native$standardized <- effective_standardized
  native$outcome_type <- if (binary_y) "binary" else "continuous"
  native$boot <- boot
  native
}
