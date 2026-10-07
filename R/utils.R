# Internal utilities -------------------------------------------------------

.pd_stop <- function(...) stop(..., call. = FALSE)

.pd_quote_name <- function(x) {
  paste0("`", gsub("`", "", x, fixed = TRUE), "`")
}

.pd_formula <- function(response, predictors) {
  lhs <- .pd_quote_name(response)
  rhs <- paste(vapply(predictors, .pd_quote_name, character(1)), collapse = " + ")
  stats::as.formula(paste(lhs, "~", rhs), env = parent.frame())
}

.pd_is_binary_numeric <- function(x) {
  x <- x[!is.na(x)]
  is.numeric(x) && length(unique(x)) == 2L
}

.pd_generate_seed <- function() {
  # Same integer range PROCESS 4.1 and 5.0 use for an automatically generated seed.
  as.integer(trunc(stats::runif(1L, 1, 1000000)))
}

.pd_package_version <- function() {
  tryCatch(
    as.character(utils::packageVersion("processdiagram")),
    error = function(e) NA_character_
  )
}

.pd_provenance <- function(engine, seed = NULL, seed_source = NULL,
                           extra = list()) {
  base <- list(
    engine = engine,
    seed = seed,
    seed_source = seed_source,
    processdiagram_version = .pd_package_version(),
    R_version = R.version.string,
    RNG_kind = RNGkind()
  )
  c(base, extra)
}

.pd_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < .001) return("***")
  if (p < .01) return("**")
  if (p < .05) return("*")
  ""
}

.pd_fmt <- function(x, digits = 2, leading_zero = TRUE) {
  if (is.na(x)) return(NA_character_)
  out <- formatC(x, format = "f", digits = digits)
  if (!leading_zero) {
    out <- sub("^0\\.", ".", out)
    out <- sub("^-0\\.", "-.", out)
  }
  out
}

.pd_path_label <- function(estimate, p, digits = 2, stars = TRUE) {
  paste0(.pd_fmt(estimate, digits = digits), if (stars) .pd_stars(p) else "")
}
