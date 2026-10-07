# PROCESS save=2 importer --------------------------------------------------

.pd_save2_is_row_index <- function(x, n) {
  if (length(x) != n) return(FALSE)
  z <- suppressWarnings(as.integer(as.character(x)))
  all(!is.na(z)) && identical(z, seq_len(n))
}

.pd_save2_numeric_matrix <- function(source) {
  source_path <- NULL
  if (is.character(source) && length(source) == 1L && file.exists(source)) {
    source_path <- normalizePath(source, mustWork = TRUE)
    dat <- utils::read.csv(source, check.names = FALSE, stringsAsFactors = FALSE)
  } else if (is.matrix(source) || is.data.frame(source)) {
    dat <- as.data.frame(source, stringsAsFactors = FALSE)
  } else {
    .pd_stop("source must be a PROCESS save=2 CSV path, matrix, or data frame.")
  }

  if (!nrow(dat) || !ncol(dat)) .pd_stop("The save=2 source is empty.")

  # write.csv() writes row names by default. Drop that synthetic first column
  # when it is simply 1, 2, ..., n.
  if (ncol(dat) > 1L && .pd_save2_is_row_index(dat[[1L]], nrow(dat))) {
    dat <- dat[-1L]
  }

  converted <- vector("list", ncol(dat))
  keep <- logical(ncol(dat))
  for (j in seq_len(ncol(dat))) {
    raw <- trimws(as.character(dat[[j]]))
    blank <- is.na(raw) | raw == ""
    num <- suppressWarnings(as.numeric(raw))
    bad <- !blank & is.na(num)
    if (any(bad)) {
      .pd_stop(
        "Column ", j, " of the save=2 source contains nonnumeric values. ",
        "This importer expects the numeric matrix returned by PROCESS save=2."
      )
    }
    if (any(!blank)) {
      keep[j] <- TRUE
      converted[[j]] <- num
    }
  }

  if (!any(keep)) .pd_stop("No numeric PROCESS result columns were found.")
  mat <- as.matrix(as.data.frame(converted[keep], check.names = FALSE))
  storage.mode(mat) <- "double"

  # PROCESS resultm is normally at least six columns wide because regression
  # rows contain coefficient, SE, statistic, p, LLCI, and ULCI.
  if (ncol(mat) < 6L) {
    .pd_stop("The save=2 matrix has fewer than six numeric columns and is not a supported PROCESS result matrix.")
  }

  list(matrix = mat, source_path = source_path)
}

.pd_save2_is_padding <- function(x) {
  is.na(x) || (is.finite(x) && abs(x) >= 99990)
}

.pd_save2_value <- function(x) {
  if (.pd_save2_is_padding(x)) NA_real_ else as.numeric(x)
}

.pd_save2_strip_sentinel <- function(mat) {
  if (!nrow(mat)) return(mat)
  first <- mat[1L, , drop = TRUE]
  nonmissing <- first[is.finite(first)]
  if (length(nonmissing) && all(vapply(nonmissing, .pd_save2_is_padding, logical(1)))) {
    mat <- mat[-1L, , drop = FALSE]
  }
  mat
}



.pd_save2_extract_v5_boot_trailer <- function(mat, process_version = c("auto", "5.0", "4.1")) {
  process_version <- match.arg(process_version)
  out <- list(
    matrix = mat,
    detected = FALSE,
    values = c(badboot = NA_real_, singular = NA_real_, eiv = NA_real_)
  )
  if (identical(process_version, "4.1") || nrow(mat) < 1L || ncol(mat) < 3L) return(out)

  last <- mat[nrow(mat), , drop = TRUE]
  present <- which(!vapply(last, .pd_save2_is_padding, logical(1)))

  # PROCESS 5 appends t(cbind(badboot, singerc, eiverc)) to resultm whenever
  # boot > 0 and save estimates are requested. After PROCESS removes padding,
  # this is a final row with exactly three non-negative integer counts.
  # `which()` preserves names when the matrix has column names (as it will
  # after conversion through data.frame/read.csv). Comparing with `identical()`
  # therefore rejects a valid c(1, 2, 3) position vector solely because it is
  # named. Compare positions by value instead.
  if (length(present) != 3L || !all(unname(present) == seq_len(3L))) return(out)
  vals <- as.numeric(last[1:3])
  integerish <- all(is.finite(vals)) && all(vals >= 0) &&
    all(abs(vals - round(vals)) < 1e-8)
  if (!integerish) return(out)

  out$matrix <- mat[-nrow(mat), , drop = FALSE]
  out$detected <- TRUE
  out$values <- stats::setNames(vals, c("badboot", "singular", "eiv"))
  out
}

.pd_save2_schema_base <- function(spec, standardized = FALSE, x_binary = NA,
                                  format = c("common", "4.1", "5.0")) {
  format <- match.arg(format)
  pred_counts <- vapply(2:length(spec$nodes), function(i) {
    sum(spec$adjacency[i, ] == 1L)
  }, integer(1))

  if (!standardized) {
    # PROCESS 4.1 and 5.0 share the same supported unstandardized save=2 layout.
    equation_rows <- sum(2L + pred_counts)
  } else if (identical(format, "4.1")) {
    # 4.1: model summary + coefficient rows + one standardized-coefficient row
    # for each non-intercept predictor.
    equation_rows <- sum(2L + 2L * pred_counts)
  } else if (identical(format, "5.0")) {
    # 5.0: model summary + coefficient rows + the six-column scale-free/
    # standardized table + the three-column effect-size table.  Each of the
    # latter two tables has one row per non-intercept predictor.
    equation_rows <- sum(2L + 3L * pred_counts)
  } else {
    .pd_stop("Internal error: standardized save=2 parsing requires a PROCESS format.")
  }

  direct_rows <- as.integer(spec$adjacency[length(spec$nodes), 1L] == 1L)
  n_specific <- length(.pd_directed_paths(spec))
  indirect_rows <- n_specific + as.integer(length(spec$m) > 1L && n_specific > 0L)

  loops <- 1L
  if (standardized) {
    if (is.na(x_binary)) .pd_stop("Internal error: x_binary must be known for standardized save=2 parsing.")
    loops <- if (isTRUE(x_binary)) 2L else 3L
  }

  list(
    equation_rows = equation_rows,
    direct_rows = direct_rows,
    n_specific = n_specific,
    indirect_rows = indirect_rows,
    loops = loops
  )
}

.pd_save2_contrast_counts <- function(n_specific) {
  if (n_specific <= 1L) return(0L)
  unique(as.integer(c(0L, 1L, choose(n_specific, 2L))))
}

.pd_save2_schema_candidates <- function(spec) {
  modes <- list(
    list(format = "common", standardized = FALSE, x_binary = NA),
    list(format = "4.1", standardized = TRUE, x_binary = TRUE),
    list(format = "4.1", standardized = TRUE, x_binary = FALSE),
    list(format = "5.0", standardized = TRUE, x_binary = TRUE),
    list(format = "5.0", standardized = TRUE, x_binary = FALSE)
  )
  out <- list()
  oi <- 0L
  for (mode in modes) {
    base <- .pd_save2_schema_base(
      spec, mode$standardized, mode$x_binary, format = mode$format
    )
    for (cr in .pd_save2_contrast_counts(base$n_specific)) {
      oi <- oi + 1L
      out[[oi]] <- data.frame(
        format = mode$format,
        standardized = mode$standardized,
        x_binary = mode$x_binary,
        contrast_rows = cr,
        rows = base$equation_rows + base$direct_rows +
          (base$indirect_rows + cr) * base$loops,
        stringsAsFactors = FALSE
      )
    }
  }
  unique(do.call(rbind, out))
}

.pd_save2_standard_signature <- function(mat, spec) {
  # The first standardized block appears immediately after the first equation's
  # summary and coefficient rows.  PROCESS 4.1 stores a one-column block;
  # PROCESS 5.0 stores six association columns.  This provides a strong format
  # signature when row counts alone are ambiguous.
  first_outcome <- 2L
  pcount <- sum(spec$adjacency[first_outcome, ] == 1L)
  if (pcount < 1L) return(NA_character_)
  start <- 1L + 1L + (pcount + 1L)
  if (start > nrow(mat)) return(NA_character_)
  row <- mat[start, , drop = TRUE]
  usable <- vapply(seq_len(min(6L, length(row))), function(j) {
    !.pd_save2_is_padding(row[j])
  }, logical(1))
  if (length(usable) >= 6L && sum(usable[1:6]) >= 4L) return("5.0")
  if (length(usable) >= 1L && usable[1L] && sum(usable) == 1L) return("4.1")
  NA_character_
}

.pd_save2_detect_schema <- function(mat, spec, standardized = NULL,
                                    process_version = c("auto", "5.0", "4.1")) {
  process_version <- match.arg(process_version)
  n <- nrow(mat)
  candidates <- .pd_save2_schema_candidates(spec)

  if (!is.null(standardized)) {
    if (!is.logical(standardized) || length(standardized) != 1L || is.na(standardized)) {
      .pd_stop("standardized must be TRUE, FALSE, or NULL for automatic detection.")
    }
    candidates <- candidates[candidates$standardized == standardized, , drop = FALSE]
  }

  if (!identical(process_version, "auto")) {
    keep <- candidates$format %in% c("common", process_version)
    candidates <- candidates[keep, , drop = FALSE]
  }

  hit <- which(candidates$rows == n)
  if (length(hit) > 1L) {
    sig <- .pd_save2_standard_signature(mat, spec)
    if (!is.na(sig)) {
      h2 <- hit[candidates$format[hit] %in% c("common", sig)]
      if (length(h2)) hit <- h2
    }
  }

  if (length(hit) != 1L) {
    expected <- paste(
      vapply(seq_len(nrow(candidates)), function(i) {
        paste0(
          candidates$rows[i], " rows (",
          if (isTRUE(candidates$standardized[i])) {
            paste0("PROCESS ", candidates$format[i], ", standardized, ",
                   if (isTRUE(candidates$x_binary[i])) "binary X" else "continuous X")
          } else {
            "PROCESS 4.1/5.0-compatible unstandardized"
          },
          if (candidates$contrast_rows[i] > 0L) {
            paste0(", ", candidates$contrast_rows[i], " indirect-effect contrast row(s) per effect-size block")
          } else {
            ", no indirect-effect contrasts"
          },
          ")"
        )
      }, character(1)),
      collapse = "; "
    )
    .pd_stop(
      "The save=2 matrix has ", n, " data rows after removing the PROCESS sentinel row, ",
      "which does not uniquely match the supported PROCESS 4.1/5.0 layouts for the supplied model specification. ",
      "Expected one of: ", expected, ". ",
      "Check that x, y, m, model/bmatrix describe the original PROCESS call. ",
      "If necessary, set process_version = '5.0' or '4.1'."
    )
  }

  fmt <- candidates$format[hit]
  if (identical(fmt, "common")) {
    fmt <- if (identical(process_version, "auto")) "4.1/5.0" else process_version
  }

  list(
    format = fmt,
    standardized = candidates$standardized[hit],
    x_binary = if (candidates$standardized[hit]) candidates$x_binary[hit] else NA,
    contrast_rows = candidates$contrast_rows[hit]
  )
}


.pd_save2_parse_paths <- function(mat, spec, standardized, format = "4.1/5.0",
                                  x_binary = NA) {
  cursor <- 1L
  rows <- list()
  rid <- 0L
  first_summary <- NULL
  first_pred_count <- NULL

  for (outcome_idx in 2:length(spec$nodes)) {
    outcome <- spec$nodes[outcome_idx]
    pred_idx <- which(spec$adjacency[outcome_idx, ] == 1L)
    predictors <- spec$nodes[pred_idx]
    pcount <- length(predictors)

    if (cursor > nrow(mat)) .pd_stop("Unexpected end of save=2 matrix while reading model summary rows.")
    summary_row <- mat[cursor, , drop = TRUE]
    if (is.null(first_summary)) {
      first_summary <- summary_row
      first_pred_count <- pcount
    }
    cursor <- cursor + 1L

    ncoef <- pcount + 1L
    if ((cursor + ncoef - 1L) > nrow(mat)) {
      .pd_stop("Unexpected end of save=2 matrix while reading regression coefficients for '", outcome, "'.")
    }
    coef_block <- mat[cursor:(cursor + ncoef - 1L), , drop = FALSE]
    cursor <- cursor + ncoef

    std_values <- rep(NA_real_, pcount)
    if (standardized && pcount > 0L) {
      if (identical(format, "4.1")) {
        if ((cursor + pcount - 1L) > nrow(mat)) {
          .pd_stop("Unexpected end of PROCESS 4.1 save=2 matrix while reading standardized coefficients for '", outcome, "'.")
        }
        std_block <- mat[cursor:(cursor + pcount - 1L), , drop = FALSE]
        cursor <- cursor + pcount
        std_values <- vapply(seq_len(pcount), function(j) {
          .pd_save2_value(std_block[j, 1L])
        }, numeric(1))
      } else if (identical(format, "5.0")) {
        if ((cursor + (2L * pcount) - 1L) > nrow(mat)) {
          .pd_stop("Unexpected end of PROCESS 5.0 save=2 matrix while reading scale-free/standardized measures for '", outcome, "'.")
        }
        assoc_block <- mat[cursor:(cursor + pcount - 1L), , drop = FALSE]
        cursor <- cursor + pcount
        # PROCESS 5 also saves eta-sq / partial eta-sq / f-sq.  They are not
        # needed for path diagrams, but consuming the block is required to keep
        # the flattened matrix aligned.
        effect_block <- mat[cursor:(cursor + pcount - 1L), , drop = FALSE]
        cursor <- cursor + pcount

        for (j in seq_len(pcount)) {
          if (ncol(assoc_block) < 6L) {
            .pd_stop("PROCESS 5.0 standardized association rows must contain at least six columns.")
          }
          # Printed PROCESS 5 columns are r, sr, pr, standYX, standY, standX.
          # To retain PROCESS 4.1 figure semantics, use StandY for the focal X
          # when X is dichotomous; all continuous predictors use StandYX.
          col <- if (isTRUE(x_binary) && identical(predictors[j], spec$x)) 5L else 4L
          std_values[j] <- .pd_save2_value(assoc_block[j, col])
          if (is.na(std_values[j])) {
            .pd_stop("A PROCESS 5.0 standardized association row for '", predictors[j],
                     " -> ", outcome, "' is incomplete.")
          }
        }
      } else {
        .pd_stop("Could not determine which PROCESS standardized save=2 layout to parse.")
      }
    }

    for (j in seq_along(predictors)) {
      cr <- coef_block[j + 1L, , drop = TRUE] # first row is intercept
      vals <- vapply(seq_len(6L), function(k) .pd_save2_value(cr[k]), numeric(1))
      if (anyNA(vals)) {
        .pd_stop("A regression row for '", predictors[j], " -> ", outcome, "' is incomplete in the save=2 matrix.")
      }
      rid <- rid + 1L
      rows[[rid]] <- data.frame(
        id = paste0("p", rid),
        from = predictors[j],
        to = outcome,
        estimate = vals[1L],
        std_estimate = if (standardized) std_values[j] else NA_real_,
        se = vals[2L],
        statistic = vals[3L],
        p = vals[4L],
        llci = vals[5L],
        ulci = vals[6L],
        stringsAsFactors = FALSE
      )
    }
  }

  inferred_n <- NA_integer_
  if (!is.null(first_summary) && length(first_summary) >= 6L) {
    df2 <- .pd_save2_value(first_summary[6L])
    if (is.finite(df2)) inferred_n <- as.integer(round(df2 + first_pred_count + 1L))
  }

  list(paths = do.call(rbind, rows), cursor = cursor, n = inferred_n)
}

.pd_save2_indirect_skeleton <- function(spec, paths, standardized) {
  specific <- .pd_indirect_point(spec, paths)
  if (!nrow(specific)) return(data.frame())

  specific$boot_se <- NA_real_
  specific$boot_llci <- NA_real_
  specific$boot_ulci <- NA_real_
  specific$std_effect <- NA_real_
  specific$std_boot_se <- NA_real_
  specific$std_boot_llci <- NA_real_
  specific$std_boot_ulci <- NA_real_

  total <- data.frame(
    id = "total",
    path = "TOTAL INDIRECT",
    effect = sum(specific$effect, na.rm = TRUE),
    boot_se = NA_real_,
    boot_llci = NA_real_,
    boot_ulci = NA_real_,
    std_effect = NA_real_,
    std_boot_se = NA_real_,
    std_boot_llci = NA_real_,
    std_boot_ulci = NA_real_,
    stringsAsFactors = FALSE
  )
  rbind(total, specific)
}

.pd_save2_standardized_indirect_expected <- function(spec, paths) {
  if (!"std_estimate" %in% names(paths) || all(is.na(paths$std_estimate))) {
    return(data.frame())
  }
  ipaths <- .pd_directed_paths(spec)
  if (!length(ipaths)) return(data.frame())
  vals <- vapply(ipaths, function(p) {
    .pd_effect_for_index_path(p, spec, paths, column = "std_estimate")
  }, numeric(1))
  specific <- data.frame(
    id = paste0("ind", seq_along(ipaths)),
    path = vapply(ipaths, function(p) paste(spec$nodes[p], collapse = " -> "), character(1)),
    std_effect_expected = vals,
    stringsAsFactors = FALSE
  )
  total <- data.frame(
    id = "total",
    path = "TOTAL INDIRECT",
    std_effect_expected = sum(vals),
    stringsAsFactors = FALSE
  )
  rbind(total, specific)
}

.pd_save2_read_indirect_block <- function(mat, cursor, nrows, total_present,
                                           contrast_rows = 0L) {
  if (nrows == 0L) return(list(table = data.frame(), contrasts = data.frame(), cursor = cursor))
  total_rows <- nrows + contrast_rows
  if ((cursor + total_rows - 1L) > nrow(mat)) {
    .pd_stop("Unexpected end of save=2 matrix while reading indirect effects.")
  }
  block <- mat[cursor:(cursor + nrows - 1L), , drop = FALSE]
  labels <- if (total_present) {
    c("TOTAL", paste0("row", seq_len(nrows - 1L)))
  } else {
    paste0("row", seq_len(nrows))
  }
  out <- data.frame(
    label = labels,
    effect = vapply(seq_len(nrows), function(i) .pd_save2_value(block[i, 1L]), numeric(1)),
    boot_se = vapply(seq_len(nrows), function(i) .pd_save2_value(block[i, 2L]), numeric(1)),
    boot_llci = vapply(seq_len(nrows), function(i) .pd_save2_value(block[i, 3L]), numeric(1)),
    boot_ulci = vapply(seq_len(nrows), function(i) .pd_save2_value(block[i, 4L]), numeric(1)),
    stringsAsFactors = FALSE
  )

  contrast_tab <- data.frame()
  if (contrast_rows > 0L) {
    cstart <- cursor + nrows
    cblock <- mat[cstart:(cstart + contrast_rows - 1L), , drop = FALSE]
    contrast_tab <- data.frame(
      effect = vapply(seq_len(contrast_rows), function(i) .pd_save2_value(cblock[i, 1L]), numeric(1)),
      boot_se = vapply(seq_len(contrast_rows), function(i) .pd_save2_value(cblock[i, 2L]), numeric(1)),
      boot_llci = vapply(seq_len(contrast_rows), function(i) .pd_save2_value(cblock[i, 3L]), numeric(1)),
      boot_ulci = vapply(seq_len(contrast_rows), function(i) .pd_save2_value(cblock[i, 4L]), numeric(1)),
      stringsAsFactors = FALSE
    )
  }

  list(table = out, contrasts = contrast_tab, cursor = cursor + total_rows)
}

.pd_save2_attach_indirect <- function(skeleton, raw_tab, std_tab = data.frame(),
                                      std_expected = data.frame(), tolerance = 1e-4) {
  if (!nrow(skeleton)) return(list(indirect = skeleton, map = integer(), ok = TRUE, details = data.frame()))
  map <- .pd_align_process_indirect(skeleton, raw_tab, tolerance)
  details <- list()
  di <- 0L
  out <- skeleton

  for (i in seq_len(nrow(out))) {
    j <- map[i]
    if (is.na(j)) {
      di <- di + 1L
      details[[di]] <- data.frame(
        component = "indirect", item = out$path[i], field = "row",
        expected = out$effect[i], imported = NA_real_, abs_diff = Inf,
        tolerance = tolerance, ok = FALSE, stringsAsFactors = FALSE
      )
      next
    }

    expected <- out$effect[i]
    imported <- raw_tab$effect[j]
    diff <- abs(expected - imported)
    ok <- is.finite(diff) && diff <= tolerance
    di <- di + 1L
    details[[di]] <- data.frame(
      component = "indirect", item = out$path[i], field = "effect",
      expected = expected, imported = imported, abs_diff = diff,
      tolerance = tolerance, ok = ok, stringsAsFactors = FALSE
    )

    out$effect[i] <- imported
    out$boot_se[i] <- raw_tab$boot_se[j]
    out$boot_llci[i] <- raw_tab$boot_llci[j]
    out$boot_ulci[i] <- raw_tab$boot_ulci[j]

    if (nrow(std_tab) >= j) {
      out$std_effect[i] <- std_tab$effect[j]
      out$std_boot_se[i] <- std_tab$boot_se[j]
      out$std_boot_llci[i] <- std_tab$boot_llci[j]
      out$std_boot_ulci[i] <- std_tab$boot_ulci[j]

      if (nrow(std_expected)) {
        ei <- match(out$id[i], std_expected$id)
        if (!is.na(ei)) {
          expected_std <- std_expected$std_effect_expected[ei]
          imported_std <- std_tab$effect[j]
          sdiff <- abs(expected_std - imported_std)
          sok <- is.finite(sdiff) && sdiff <= tolerance
          di <- di + 1L
          details[[di]] <- data.frame(
            component = "indirect", item = out$path[i], field = "std_effect",
            expected = expected_std, imported = imported_std, abs_diff = sdiff,
            tolerance = tolerance, ok = sok, stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  detail_df <- if (length(details)) do.call(rbind, details) else data.frame()
  list(
    indirect = out,
    map = map,
    ok = !nrow(detail_df) || all(detail_df$ok),
    details = detail_df
  )
}

#' Import a PROCESS `save = 2` result
#'
#' Reconstructs a diagram-ready object from the flattened numeric matrix returned
#' by PROCESS with `save = 2`. It supports current PROCESS 5.0 files as well as
#' compatible PROCESS 4.1 files. It does not rerun PROCESS; the saved PROCESS
#' values and bootstrap intervals remain authoritative.
#'
#' The importer supports the PROCESS for R 4.1 and 5.0 `save = 2` layouts for
#' the mediation models supported by processdiagram (Models 4, 6, 81, and custom
#' b-matrices). PROCESS 5.0 standardized files are recognized from their saved
#' scale-free/standardized association blocks. Pairwise/custom indirect-effect
#' contrast rows are recognized and safely skipped. Additional result blocks such
#' as `describe`, `covcoeff`, `normal`, diagnostics, or moderation output are not
#' yet supported.
#'
#' @param source Path to a CSV written from a PROCESS `save = 2` result, or the
#'   matrix/data frame itself.
#' @param x,y,m Variable names from the original PROCESS call.
#' @param model PROCESS model number, or `NULL` when `bmatrix` is supplied.
#' @param bmatrix Optional custom lower-triangular b-matrix vector.
#' @param standardized `NULL` to auto-detect whether standardized result blocks
#'   are present, or TRUE/FALSE to require a particular layout.
#' @param process_version `"auto"` (recommended), `"5.0"`, or `"4.1"`.
#'   Standardized files are normally version-detected from the saved matrix.
#'   Unstandardized 4.1 and 5.0 layouts are structurally identical for the
#'   supported mediation workflows, so an explicit value is only needed when
#'   provenance must identify the originating PROCESS version.
#' @param outcome_type One of `"auto"`, `"continuous"`, or `"binary"`. A flat
#'   unstandardized save=2 matrix cannot reliably distinguish OLS from logistic Y,
#'   so specify `"binary"` for dichotomous-outcome analyses if you want the
#'   generated note/provenance to identify log-odds paths correctly.
#' @param conf Confidence level as a proportion. PROCESS defaults to `.95`.
#' @param tolerance Absolute tolerance used for the internal indirect-effect
#'   consistency check.
#' @param strict If TRUE, stop when imported indirect effects do not agree with
#'   products of the imported path coefficients or when the schema is unsupported.
#' @return A `process_diagram_fit` object populated from the original PROCESS file.
#' @details
#' The flattened `save = 2` format does not retain variable names, the PROCESS
#' model number, or the original bootstrap seed. Supply the original model
#' specification explicitly. For dichotomous-Y analyses, also set
#' `outcome_type = "binary"` so notes and provenance identify the final paths as
#' logistic log-odds coefficients.
#' @seealso [process_for_diagram()], [fit_process_diagram()], [figure_note()]
#' @examples
#' example_file <- system.file(
#'   "extdata", "process5_model4_save2.csv", package = "processdiagram"
#' )
#' imported <- import_process_save2(
#'   example_file, x = "X", y = "Y", m = "M", model = 4
#' )
#' imported$validation$ok
#' imported$paths
#' imported$provenance$process_version
#' @export
import_process_save2 <- function(source, x, y, m, model = NULL, bmatrix = NULL,
                                 standardized = NULL,
                                 process_version = c("auto", "5.0", "4.1"),
                                 outcome_type = c("auto", "continuous", "binary"),
                                 conf = .95, tolerance = 1e-4, strict = TRUE) {
  outcome_type <- match.arg(outcome_type)
  process_version <- match.arg(process_version)
  src <- .pd_save2_numeric_matrix(source)
  mat <- .pd_save2_strip_sentinel(src$matrix)

  # PROCESS 5 adds a final three-count bootstrap diagnostics row to save=2
  # estimate matrices when bootstrapping is used. Remove it before structural
  # schema detection; otherwise its extra row can make a binary-X file mimic a
  # different continuous-X/contrast layout. The trailer itself also identifies
  # the file as PROCESS 5.
  trailer <- .pd_save2_extract_v5_boot_trailer(mat, process_version)
  mat <- trailer$matrix
  detected_process_version <- if (isTRUE(trailer$detected)) "5.0" else process_version

  spec <- process_spec(x = x, y = y, m = m, model = model, bmatrix = bmatrix)
  schema <- .pd_save2_detect_schema(
    mat, spec, standardized = standardized, process_version = detected_process_version
  )

  if (identical(outcome_type, "binary") && isTRUE(schema$standardized)) {
    .pd_stop(
      "This save=2 matrix contains standardized result blocks, which is incompatible with a dichotomous Y in supported PROCESS 4.1/5.0 workflows."
    )
  }
  effective_outcome_type <- if (identical(outcome_type, "auto")) {
    if (isTRUE(schema$standardized)) "continuous" else "unknown"
  } else outcome_type

  parsed_paths <- .pd_save2_parse_paths(
    mat, spec, standardized = schema$standardized, format = schema$format,
    x_binary = schema$x_binary
  )
  paths <- parsed_paths$paths
  cursor <- parsed_paths$cursor

  # PROCESS appends one direct-effect row when X -> Y is freely estimated.
  direct_present <- spec$adjacency[length(spec$nodes), 1L] == 1L
  validation_details <- list()
  vd <- 0L
  if (direct_present) {
    if (cursor > nrow(mat)) .pd_stop("Unexpected end of save=2 matrix while reading the direct effect.")
    direct_row <- mat[cursor, , drop = TRUE]
    cursor <- cursor + 1L
    hit <- paths$from == spec$x & paths$to == spec$y
    if (sum(hit) == 1L) {
      imported_direct <- .pd_save2_value(direct_row[1L])
      diff <- abs(paths$estimate[hit] - imported_direct)
      vd <- vd + 1L
      validation_details[[vd]] <- data.frame(
        component = "direct", item = paste(spec$x, "->", spec$y), field = "effect",
        expected = paths$estimate[hit], imported = imported_direct,
        abs_diff = diff, tolerance = tolerance,
        ok = is.finite(diff) && diff <= tolerance,
        stringsAsFactors = FALSE
      )
    }
  }

  skeleton <- .pd_save2_indirect_skeleton(spec, paths, schema$standardized)
  n_specific <- nrow(skeleton) - as.integer(nrow(skeleton) > 0L)
  total_present <- length(spec$m) > 1L && n_specific > 0L
  table_rows <- n_specific + as.integer(total_present)

  raw_read <- .pd_save2_read_indirect_block(
    mat, cursor, table_rows, total_present, contrast_rows = schema$contrast_rows
  )
  raw_tab <- raw_read$table
  cursor <- raw_read$cursor

  std_tab <- data.frame()
  if (schema$standardized) {
    # PROCESS stores a partially standardized block as kk=2. For binary X this
    # is the requested standardized result. For continuous X PROCESS additionally
    # stores a completely standardized kk=3 block, which is the one it prints.
    partial_read <- .pd_save2_read_indirect_block(
      mat, cursor, table_rows, total_present, contrast_rows = schema$contrast_rows
    )
    partial_tab <- partial_read$table
    cursor <- partial_read$cursor
    if (isTRUE(schema$x_binary)) {
      std_tab <- partial_tab
    } else {
      complete_read <- .pd_save2_read_indirect_block(
        mat, cursor, table_rows, total_present, contrast_rows = schema$contrast_rows
      )
      std_tab <- complete_read$table
      cursor <- complete_read$cursor
    }
  }

  std_expected <- if (schema$standardized) {
    .pd_save2_standardized_indirect_expected(spec, paths)
  } else {
    data.frame()
  }

  attached <- .pd_save2_attach_indirect(
    skeleton, raw_tab, std_tab = std_tab, std_expected = std_expected,
    tolerance = tolerance
  )
  indirect <- attached$indirect

  if (nrow(attached$details)) {
    validation_details <- c(validation_details, split(attached$details, seq_len(nrow(attached$details))))
  }
  validation_df <- if (length(validation_details)) do.call(rbind, validation_details) else data.frame()
  validation_ok <- !nrow(validation_df) || all(validation_df$ok)

  if (cursor != (nrow(mat) + 1L)) {
    msg <- paste0(
      "The supported save=2 schema consumed ", cursor - 1L, " rows but the matrix contains ",
      nrow(mat), ". Extra result blocks are present and cannot yet be interpreted safely."
    )
    if (strict) .pd_stop(msg) else warning(msg, call. = FALSE)
    validation_ok <- FALSE
  }

  if (!validation_ok) {
    bad <- validation_df[!validation_df$ok, , drop = FALSE]
    msg <- paste0(
      "The PROCESS save=2 structure was parsed, but its internal consistency check failed; ",
      "the matrix does not uniquely match the supported layout. ",
      if (nrow(bad)) paste0("First mismatch: ", bad$component[1L], " / ", bad$item[1L],
                            " / ", bad$field[1L], " (imported=", bad$imported[1L],
                            ", expected=", bad$expected[1L], ").")
      else ""
    )
    if (strict) .pd_stop(msg) else warning(msg, call. = FALSE)
  }

  out <- list(
    call = match.call(),
    spec = spec,
    paths = paths,
    indirect = indirect,
    models = NULL,
    n = parsed_paths$n,
    standardized = isTRUE(schema$standardized),
    standardized_requested = NA,
    outcome_type = effective_outcome_type,
    conf = conf,
    boot = NA_integer_,
    process_result = src$matrix,
    validation = list(
      ok = validation_ok,
      mode = "save2-internal",
      details = validation_df,
      tolerance = tolerance
    ),
    provenance = .pd_provenance(
      engine = "PROCESS save=2",
      seed = NA_integer_,
      seed_source = "not stored in PROCESS save=2",
      extra = list(
        save2_format = paste0("PROCESS for R ", schema$format, "-compatible save=2 layout"),
        process_version = schema$format,
        source_file = src$source_path,
        values_source = "PROCESS save=2 matrix",
        validation_mode = "save2-internal",
        standardized_requested = NA,
        standardized_effective = isTRUE(schema$standardized),
        x_type = if (isTRUE(schema$standardized)) {
          if (isTRUE(schema$x_binary)) "binary" else "continuous"
        } else "unknown",
        outcome_type = effective_outcome_type,
        bootstrap_seed_available = FALSE,
        save2_process5_bootstrap_trailer = isTRUE(trailer$detected),
        save2_bootstrap_diagnostic_counts = if (isTRUE(trailer$detected)) trailer$values else NULL,
        save2_indirect_contrast_rows_per_block = schema$contrast_rows
      )
    )
  )
  class(out) <- "process_diagram_fit"
  out
}
