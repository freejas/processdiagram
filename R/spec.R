#' Build a PROCESS-style path specification
#'
#' The node order is X, M1...Mk, Y. The adjacency matrix follows PROCESS's
#' lower-triangular b-matrix convention: rows are outcomes, columns are predictors.
#'
#' @param x Name of the X variable.
#' @param y Name of the Y variable.
#' @param m Character vector of mediator names.
#' @param model PROCESS model number. Initial support: 4, 6, and 81.
#' @param bmatrix Optional custom lower-triangular b-matrix vector. When supplied,
#'   it overrides `model`.
#' @return A `process_diagram_spec` object.
#' @seealso [process_for_diagram()], [fit_process_diagram()]
#' @examples
#' process_spec("X", "Y", "M", model = 4)
#' process_spec("X", "Y", c("M1", "M2"), model = 6)
#' process_spec(
#'   "X", "Y", c("M1", "M2", "M3"),
#'   bmatrix = c(1, 1, 0, 1, 0, 0, 1, 1, 1, 1)
#' )
#' @export
process_spec <- function(x, y, m, model = NULL, bmatrix = NULL) {
  if (length(x) != 1L || length(y) != 1L) .pd_stop("x and y must each be one variable name.")
  if (length(m) < 1L) .pd_stop("At least one mediator is required in this initial version.")
  nodes <- c(x, m, y)
  roles <- c("X", paste0("M", seq_along(m)), "Y")
  n <- length(nodes)
  adj <- matrix(0L, nrow = n, ncol = n, dimnames = list(nodes, nodes))

  if (!is.null(bmatrix)) {
    expected <- n * (n - 1L) / 2L
    if (length(bmatrix) != expected) {
      .pd_stop("bmatrix has length ", length(bmatrix), "; expected ", expected,
               " for ", n, " nodes (X + mediators + Y).")
    }
    k <- 1L
    for (i in 2:n) {
      for (j in 1:(i - 1L)) {
        adj[i, j] <- as.integer(bmatrix[k] != 0)
        k <- k + 1L
      }
    }
    model_id <- "custom"
  } else {
    if (is.null(model)) .pd_stop("Supply either model or bmatrix.")
    model <- as.integer(model)
    if (model == 4L) {
      # Parallel mediation: X -> every M; every M -> Y; X -> Y.
      adj[2:(n - 1L), 1L] <- 1L
      adj[n, 1L] <- 1L
      adj[n, 2:(n - 1L)] <- 1L
    } else if (model == 6L) {
      # Serial mediation: all earlier variables predict all later variables.
      for (i in 2:n) adj[i, 1:(i - 1L)] <- 1L
    } else if (model == 81L) {
      if (length(m) < 3L || length(m) > 6L) {
        .pd_stop("PROCESS model 81 requires 3 to 6 mediators.")
      }
      # Base mediation paths.
      adj[2:(n - 1L), 1L] <- 1L
      adj[n, 1L] <- 1L
      adj[n, 2:(n - 1L)] <- 1L
      # M1 predicts all later mediators (the additional model-81 paths).
      if (n > 3L) adj[3:(n - 1L), 2L] <- 1L
    } else {
      .pd_stop("Initial version supports model = 4, 6, or 81, or a custom bmatrix.")
    }
    model_id <- model
  }

  out <- list(
    x = x,
    y = y,
    m = m,
    nodes = nodes,
    roles = stats::setNames(roles, nodes),
    model = model_id,
    adjacency = adj,
    bmatrix = bmatrix
  )
  class(out) <- "process_diagram_spec"
  out
}
