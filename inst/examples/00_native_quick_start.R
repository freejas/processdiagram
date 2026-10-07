# Fully runnable quick start using the base-R mtcars dataset.
# No PROCESS installation is required for this example.

library(processdiagram)

fit <- fit_process_diagram(
  mtcars,
  x = "am",
  y = "mpg",
  m = "wt",
  model = 4,
  standardized = TRUE,
  boot = 0
)

fit$paths
fit$indirect

labels <- c(
  X = "Transmission\n(0 = automatic, 1 = manual)",
  M1 = "Weight",
  Y = "Fuel economy"
)

plot(fit, labels = labels)
figure_note(fit, labels = labels)
