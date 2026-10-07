# Fully runnable PROCESS-5 save=2 import example using a small save=2 fixture bundled
# with processdiagram. No external PROCESS CSV is required.

library(processdiagram)

example_file <- system.file(
  "extdata", "process5_model4_save2.csv", package = "processdiagram"
)

imported <- import_process_save2(
  example_file,
  x = "X",
  y = "Y",
  m = "M",
  model = 4
)

imported$validation$ok
imported$paths
imported$indirect
imported$provenance
plot(imported, labels = c(X = "Predictor", M1 = "Mediator", Y = "Outcome"))
