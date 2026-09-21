cellData <- function() {
  categories <- c("primf", "secdf", "pltns", "primn", "secdn", "c3ann")
  x <- new.magpie(c("F.1", "N.2", "L.3"), c(2025, 2050), categories, fill = 10)
  x[, , "pltns"] <- 0
  # F, forest-potential: cropland abandoned to secondary non-forest
  x["F.1", 2050, "c3ann"] <- 4
  x["F.1", 2050, "secdn"] <- 16
  # N, not forest-potential: cropland afforested as plantation
  x["N.2", 2050, "c3ann"] <- 5
  x["N.2", 2050, "pltns"] <- 5
  # L, not forest-potential: secondary forest cleared for cropland
  x["L.3", 2050, "secdf"] <- 6
  x["L.3", 2050, "c3ann"] <- 14
  return(x)
}
potential <- new.magpie(c("F.1", "N.2", "L.3"), NULL, NULL, fill = c(1, 0, 0))

test_that("land abandoned on a forest-potential cell regrows as secondary forest", {
  x <- toolSecondaryLandRule(cellData(), potential, baseYear = 2025)

  expect_equal(as.vector(x["F.1", 2050, c("secdf", "secdn")]), c(16, 10))
})

test_that("afforestation where forest cannot grow becomes secondary non-forest", {
  x <- toolSecondaryLandRule(cellData(), potential, baseYear = 2025)

  expect_equal(as.vector(x["N.2", 2050, c("secdf", "pltns", "secdn")]), c(10, 0, 15))
})

test_that("losses are left alone", {
  x <- toolSecondaryLandRule(cellData(), potential, baseYear = 2025)

  expect_equal(x["L.3", , ], cellData()["L.3", , ])
})

test_that("each cell's area is kept, and primary land, cropland and the base year untouched", {
  input <- cellData()
  x <- toolSecondaryLandRule(input, potential, baseYear = 2025)

  expect_equal(dimSums(x, dim = 3), dimSums(input, dim = 3))
  expect_equal(x[, , c("primf", "primn", "c3ann")], input[, , c("primf", "primn", "c3ann")])
  expect_equal(x[, 2025, ], input[, 2025, ])
})
