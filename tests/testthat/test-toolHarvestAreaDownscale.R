cellsOf <- function(values, years, item = "wood_harvest_area.secmf") {
  x <- new.magpie(paste0("c", seq_along(values[[1]])), years, item, fill = 0)
  for (i in seq_along(values)) x[, i, ] <- values[[i]]
  x
}
resmap <- data.frame(cell = paste0("c", 1:4), lowRes = c("A", "A", "A", "B"))

test_that("harvest goes where the target harvested, in proportion to its intensity", {
  maxHarvest <- cellsOf(list(c(10, 10, 10, 5)), 2025)
  intensity <- cellsOf(list(c(0.3, 0.1, 0, 0.2)), NULL)
  harvest <- new.magpie(c("A", "B"), 2025, "wood_harvest_area.secmf", fill = c(2, 1))

  out <- toolHarvestAreaDownscale(harvest, resmap, maxHarvest, intensity)
  # region A: weights 3, 1, 0; the cell that never harvested gets none
  expect_equal(as.vector(out[paste0("c", 1:3), , ]), c(1.5, 0.5, 0))
  expect_equal(as.vector(out["c4", , ]), 1)
})

test_that("harvest follows land as it changes", {
  maxHarvest <- cellsOf(list(c(10, 10, 10, 5), c(5, 10, 10, 5)), c(2025, 2030))
  intensity <- cellsOf(list(c(0.1, 0.1, 0, 0.2)), NULL)
  harvest <- new.magpie(c("A", "B"), c(2025, 2030), "wood_harvest_area.secmf", fill = 1)

  out <- toolHarvestAreaDownscale(harvest, resmap, maxHarvest, intensity)
  expect_equal(as.vector(out[c("c1", "c2"), 2030, ]), c(1 / 3, 2 / 3))
})

test_that("a region without history falls back to land, and caps pass harvest on", {
  maxHarvest <- cellsOf(list(c(1, 10, 10, 5)), 2025)
  noHistory <- cellsOf(list(c(0, 0, 0, 0)), NULL)
  harvest <- new.magpie(c("A", "B"), 2025, "wood_harvest_area.secmf", fill = c(3, 1))
  out <- toolHarvestAreaDownscale(harvest, resmap, maxHarvest, noHistory)
  expect_equal(as.vector(out[paste0("c", 1:3), , ]), c(1, 10, 10) * 3 / 21)

  # the only cell that harvested holds 1; the rest goes to the region's other land
  intensity <- cellsOf(list(c(0.5, 0, 0, 0.2)), NULL)
  out <- toolHarvestAreaDownscale(harvest, resmap, maxHarvest, intensity)
  expect_equal(as.vector(out["c1", , ]), 1)
  expect_equal(sum(out[paste0("c", 1:3), , ]), 3)
  expect_true(all(out <= maxHarvest))
})

test_that("secyf and secmf share their secdf", {
  items <- c("wood_harvest_area.secyf", "wood_harvest_area.secmf")
  maxHarvest <- new.magpie(paste0("c", 1:4), 2025, items, fill = c(2, 10, 10, 5))
  intensity <- new.magpie(paste0("c", 1:4), NULL, items, fill = c(1, 0, 0, 1))
  harvest <- new.magpie(c("A", "B"), 2025, items, fill = c(2, 1, 2, 1))
  out <- toolHarvestAreaDownscale(harvest, resmap, maxHarvest, intensity)
  expect_lte(sum(out["c1", , ]), 2 + 1e-9)
  expect_equal(as.vector(dimSums(out[paste0("c", 1:3), , ], dim = 1)), c(2, 2))
})

test_that("uncapped, the land only weights (for harvest carbon)", {
  maxHarvest <- cellsOf(list(c(1, 10, 10, 5)), 2025)
  intensity <- cellsOf(list(c(4, 1, 0, 1)), NULL)
  carbon <- new.magpie(c("A", "B"), 2025, "wood_harvest_area.secmf", fill = c(140, 7))
  out <- toolHarvestAreaDownscale(carbon, resmap, maxHarvest, intensity, capped = FALSE)
  expect_equal(as.vector(out[paste0("c", 1:4), , ]), c(40, 100, 0, 7))
})
