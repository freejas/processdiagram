# Model 6 serial mediation using the base-R mtcars dataset.
# Source PROCESS first so that process() is available.

library(processdiagram)

fit <- process_for_diagram(
  mtcars,
  x = "am",
  y = "mpg",
  m = c("wt", "qsec"),
  model = 6,
  standardized = TRUE,
  boot = 5000
)

labels <- c(
  X = "Transmission\n(0 = automatic, 1 = manual)",
  M1 = "Weight",
  M2 = "Quarter-mile time",
  Y = "Fuel economy"
)

plot(fit, labels = labels, show_indirect = FALSE)
figure_note(fit, labels = labels)
