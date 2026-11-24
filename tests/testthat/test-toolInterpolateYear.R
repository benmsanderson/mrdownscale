test_that("a missing year is interpolated between the reported years around it", {
  x <- new.magpie("A.1", c(2020, 2025, 2030), c("a", "b"), fill = 0)
  x[, 2020, ] <- 10
  x[, 2025, ] <- 20
  x[, 2030, ] <- 0
  out <- toolInterpolateYear(x, 2024)
  expect_equal(getYears(out, as.integer = TRUE), c(2020, 2024, 2025, 2030))
  expect_equal(as.vector(out[, 2024, "a"]), 18)
  expect_equal(out[, c(2020, 2025, 2030), ], x)
  expect_identical(toolInterpolateYear(x, 2025), x)
  expect_error(toolInterpolateYear(x, 2035))
})
