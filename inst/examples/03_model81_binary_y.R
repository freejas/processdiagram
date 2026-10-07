# Model 81 with a dichotomous outcome using the base-R airquality dataset.
# EvenDay is created only to provide a simple reproducible binary outcome.
# Source PROCESS first so that process() is available.

library(processdiagram)

aq <- transform(
  airquality,
  EvenDay = as.integer(Day %% 2 == 0)
)

fit <- process_for_diagram(
  aq,
  x = "Month",
  y = "EvenDay",
  m = c("Temp", "Wind", "Solar.R"),
  model = 81,
  standardized = TRUE,
  boot = 5000
)

fit$provenance$standardized_requested
fit$provenance$standardized_effective
fit$provenance$outcome_type

labels <- c(
  X = "Month",
  M1 = "Temperature",
  M2 = "Wind",
  M3 = "Solar radiation",
  Y = "Even-numbered day\n(0 = No, 1 = Yes)"
)

plot(fit, labels = labels, show_indirect = FALSE)
figure_note(fit, labels = labels)
