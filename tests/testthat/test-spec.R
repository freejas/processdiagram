test_that("model 4 builds parallel mediation paths", {
  s <- process_spec("x", "y", c("m1", "m2"), model = 4)
  expect_equal(s$adjacency["m1", "x"], 1)
  expect_equal(s$adjacency["m2", "x"], 1)
  expect_equal(s$adjacency["y", "m1"], 1)
  expect_equal(s$adjacency["y", "m2"], 1)
  expect_equal(s$adjacency["m2", "m1"], 0)
})

test_that("model 6 is fully serial", {
  s <- process_spec("x", "y", c("m1", "m2"), model = 6)
  expect_equal(sum(s$adjacency), 6)
  expect_equal(s$adjacency["m2", "m1"], 1)
})

test_that("model 81 adds M1 to later mediator paths", {
  s <- process_spec("x", "y", c("m1", "m2", "m3"), model = 81)
  expect_equal(s$adjacency["m2", "m1"], 1)
  expect_equal(s$adjacency["m3", "m1"], 1)
  expect_equal(s$adjacency["m3", "m2"], 0)
})

test_that("custom bmatrix follows PROCESS lower triangle order", {
  b <- c(1, 1, 0, 1, 0, 0, 1, 1, 1, 1)
  s <- process_spec("x", "y", c("m1", "m2", "m3"), bmatrix = b)
  expect_equal(s$adjacency["m1", "x"], 1)
  expect_equal(s$adjacency["m2", "m1"], 0)
  expect_equal(s$adjacency["m3", "m2"], 0)
  expect_equal(s$adjacency["y", "m1"], 1)
  expect_equal(s$adjacency["y", "m2"], 1)
  expect_equal(s$adjacency["y", "m3"], 1)
})
