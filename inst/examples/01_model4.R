# Model 4 using the base-R mtcars dataset.
# Source PROCESS first so that process() is available.

library(processdiagram)

fit <- process_for_diagram(
  mtcars,
  x = "am",
  y = "mpg",
  m = "wt",
  model = 4,
  standardized = TRUE,
  boot = 5000
)

labels <- c(
  X = "Transmission\n(0 = automatic, 1 = manual)",
  M1 = "Weight",
  Y = "Fuel economy"
)

plot(fit, labels = labels)
figure_note(fit, labels = labels)
