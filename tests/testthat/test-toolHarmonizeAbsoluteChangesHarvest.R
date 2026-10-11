landFixture <- function() {
  categories <- c("crop_rainfed", "pastr", "primf", "secdf", "primn", "secdn", "urban")
  xTarget <- new.magpie("A", c(2020, 2025, 2030), categories, fill = 0)
  xTarget[, , ] <- c(rep(100, 3), rep(50, 3), rep(60, 3), rep(40, 3), rep(30, 3), rep(20, 3), rep(0, 3))
  xInput <- new.magpie("A", c(2020, 2025, 2050), categories, fill = 0)
  # the input's own levels differ from the target; only its changes should count
  xInput[, , ] <- c(130, 130, 150, 40, 40, 40, 70, 70, 60, 20, 20, 20, 20, 20, 10, 20, 20, 20, 0, 0, 0)
  list(xInput = xInput, xTarget = xTarget)
}

test_that("the input's changes are applied to the target's level", {
  f <- landFixture()
  out <- toolHarmonizeAbsoluteChangesHarvest(f$xInput, f$xTarget, c(2025, 2050))
  expect_equal(as.vector(out[, 2050, "crop_rainfed"]), 120)
  expect_equal(as.vector(out[, 2050, "pastr"]), 50)
  expect_equal(as.vector(dimSums(out[, 2050, c("primf", "secdf")], 3)), 90)
  expect_equal(as.vector(out[, c(2020, 2025), ]), as.vector(f$xTarget[, c(2020, 2025), ]))
})

test_that("primary forest follows harvest, keeping the forest total", {
  f <- landFixture()
  harvest <- new.magpie("A", c(2025, 2050), "primf", fill = 1)
  out <- toolHarmonizeAbsoluteChangesHarvest(f$xInput, f$xTarget, c(2025, 2050), primfHarvest = harvest)
  expect_equal(as.vector(dimSums(out[, 2050, c("primf", "secdf")], 3)), 90)
  # 25 years of 1 Mha/yr at a falling primary fraction, plus a share of the 10 Mha net loss
  expect_lt(as.vector(out[, 2050, "primf"]), 60 - 10 * 0.6)
  expect_gt(as.vector(out[, 2050, "primf"]), 60 - 25 - 10)
})

test_that("data without forest are faded", {
  xTarget <- new.magpie("A", c(2020, 2025, 2030), "bioh", fill = 10)
  xInput <- new.magpie("A", c(2020, 2025, 2030, 2050), "bioh", fill = 20)
  out <- toolHarmonizeAbsoluteChangesHarvest(xInput, xTarget, c(2025, 2050))
  expect_equal(out, toolHarmonizeFade(xInput, xTarget, c(2025, 2050)))
})
