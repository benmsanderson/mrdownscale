forestOf <- function(values, years) {
  x <- new.magpie("A.1", years, NULL, fill = 0)
  x[, , ] <- values
  x
}

test_that("primary forest loses the harvest its share of the wood implies", {
  years <- c(2025, 2030, 2035)
  forest <- forestOf(100, years)
  primf0 <- new.magpie("A.1", 2025, "primf", fill = 60)
  harvest <- new.magpie("A.1", years, "primf", fill = 2)

  primf <- toolPrimfFromHarvest(forest, primf0, harvest)

  # 2025-2030: 60 percent primary, the historical share, 2 Mha per year
  expect_equal(as.vector(primf[, 2030, ]), 50)
  # 2030-2035: now 50 percent primary, so 50/60 of the wood from primary
  expect_equal(as.vector(primf[, 2035, ]), 50 - 5 * 2 * (0.5 / 0.6))
})

test_that("a net loss of forest takes primary forest in proportion", {
  years <- c(2025, 2030)
  forest <- forestOf(c(100, 80), years)
  primf0 <- new.magpie("A.1", 2025, "primf", fill = 60)
  noHarvest <- new.magpie("A.1", years, "primf", fill = 0)

  primf <- toolPrimfFromHarvest(forest, primf0, noHarvest)
  expect_equal(as.vector(primf[, 2030, ]), 60 - 20 * 0.6)
})

test_that("primary forest neither grows back nor exceeds forest", {
  years <- c(2025, 2030, 2040)
  growing <- forestOf(c(100, 120, 150), years)
  primf0 <- new.magpie("A.1", 2025, "primf", fill = 60)
  noHarvest <- new.magpie("A.1", years, "primf", fill = 0)
  expect_equal(as.vector(toolPrimfFromHarvest(growing, primf0, noHarvest)), c(60, 60, 60))

  collapsing <- forestOf(c(100, 30, 0), years)
  heavy <- new.magpie("A.1", years, "primf", fill = 50)
  primf <- toolPrimfFromHarvest(collapsing, primf0, heavy)
  expect_true(all(primf >= 0))
  expect_true(all(as.vector(primf) <= c(100, 30, 0)))
})
