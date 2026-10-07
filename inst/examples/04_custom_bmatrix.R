# Custom lower-triangular b-matrix using the base-R mtcars dataset.
# Source PROCESS first so that process() is available.

library(processdiagram)

fit <- process_for_diagram(
  mtcars,
  x = "am",
  y = "mpg",
  m = c("wt", "hp", "qsec"),
  bmatrix = c(1, 1, 0, 1, 0, 0, 1, 1, 1, 1),
  standardized = TRUE,
  boot = 5000
)

# When reordering mediators, variable-name labels are safest because M1/M2/M3
# always refer to the order in m, not to top/middle/bottom display positions.
labels <- c(
  am = "Transmission",
  wt = "Weight",
  hp = "Horsepower",
  qsec = "Quarter-mile time",
  mpg = "Fuel economy"
)

plot(
  fit,
  labels = labels,
  mediator_order = c("hp", "wt", "qsec"),
  show_indirect = FALSE
)
