weightedFixture <- function() {
  cells <- c("1.1", "1.2", "1.3", "2.1", "2.2", "2.3")
  categories <- c("crop", "primn", "secdn")
  xTarget <- new.magpie(cells, 2020, categories, sets = c("x", "y", "year", "data"))
  xTarget[, , "crop"] <- c(1, 0, 2, 3, 1, 0)
  xTarget[, , "primn"] <- c(4, 6, 2, 1, 2, 5)
  xTarget[, , "secdn"] <- c(1, 0, 1, 0, 1, 1)
  mapping <- data.frame(cell = cells, lowRes = rep(c("A", "B"), each = 3))
  xTargetLowRes <- toolAggregate(xTarget, mapping, from = "cell", to = "lowRes")
  x <- new.magpie(c("A", "B"), c(2020, 2025, 2030), categories)
  x[, 2020, ] <- xTargetLowRes
  x["A", 2025, ] <- c(5, 10, 2)
  x["A", 2030, ] <- c(8, 7, 2)
  x["B", 2025, ] <- c(4, 9, 1)
  x["B", 2030, ] <- c(6, 6, 2)
  list(x = x, xTarget = xTarget, xTargetLowRes = xTargetLowRes, mapping = mapping)
}

test_that("with equal weights it is interpolate2", {
  f <- weightedFixture()
  weight <- new.magpie(getItems(f$xTarget, 1), NULL, NULL, fill = 1)
  classic <- toolDownscaleMagpieClassic(f$x, f$xTarget, f$xTargetLowRes, f$mapping)
  weighted <- toolDownscaleWeighted(f$x, f$xTarget, f$xTargetLowRes, f$mapping, weight)
  expect_equal(as.vector(weighted), as.vector(classic[, getYears(weighted), getItems(weighted, 3)]),
               tolerance = 1e-10)
})

test_that("cells without weight keep their natural land, and regional totals are kept", {
  f <- weightedFixture()
  weight <- new.magpie(getItems(f$xTarget, 1), NULL, NULL, fill = 1)
  weight["1.2", , ] <- 0                       # e.g. desert: potential biomass zero
  out <- toolDownscaleWeighted(f$x, f$xTarget, f$xTargetLowRes, f$mapping, weight)
  expect_equal(as.vector(out["1.2", , "primn"]), rep(6, 3))
  expect_equal(as.vector(out["1.2", , "crop"]), rep(0, 3))
  regional <- toolAggregate(out, f$mapping, from = "cell", to = "lowRes")
  expect_equal(as.vector(regional[, c(2025, 2030), ]), as.vector(f$x[, c(2025, 2030), getItems(out, 3)]),
               tolerance = 1e-8)
  expect_true(all(out >= -1e-12))
})

test_that("weighted shares take the requested total, capped at each cell's stock", {
  stock <- c(4, 6, 2)
  shares <- toolWeightedShares(stock, c(2, 0, 1), 0.3)
  expect_equal(sum(shares * stock), 0.3 * sum(stock))
  expect_equal(shares[2], 0)
  expect_equal(shares[1] / shares[3], 2)
  # more than the weighted cells hold: they give everything, the rest comes evenly from the others
  shares <- toolWeightedShares(stock, c(1, 0, 0), 0.5)
  expect_equal(shares[1], 1)
  expect_equal(sum(shares * stock), 0.5 * sum(stock))
  expect_equal(toolWeightedShares(stock, c(0, 0, 0), 0.2), rep(0.2, 3))
})
