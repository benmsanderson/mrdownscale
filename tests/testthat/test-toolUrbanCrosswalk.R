urbanInput <- function(urban) {
  years <- c(2025, 2050, 2100)
  x <- new.magpie("A.1", years, c("urban", "primn", "secdn", "c3ann"), fill = 0)
  x[, , "urban"] <- urban
  x[, , "primn"] <- 60
  x[, , "secdn"] <- 20
  x[, , "c3ann"] <- 100 - urban
  x
}
urbanTarget <- function() {
  x <- new.magpie("A.1", 2025, c("urban", "primn", "secdn", "c3ann"), fill = 0)
  x[, , "urban"] <- 8
  x
}

test_that("a model that reports no urban land keeps the target's", {
  y <- toolUrbanCrosswalk(urbanInput(c(0, 0, 0)), urbanTarget(), year = 2025)
  expect_equal(as.vector(y[, , "urban"]), c(8, 8, 8))
  # taken from non-forested natural land, by its composition (60:20)
  expect_equal(as.vector(y[, , "primn"]), rep(60 - 6, 3))
  expect_equal(as.vector(y[, , "secdn"]), rep(20 - 2, 3))
})

test_that("the input's urban change is kept, on the target's level", {
  x <- urbanInput(c(5, 7, 10))
  y <- toolUrbanCrosswalk(x, urbanTarget(), year = 2025)
  expect_equal(as.vector(y[, , "urban"]), c(8, 10, 13))
  # total area unchanged in every year
  expect_equal(as.vector(dimSums(y, dim = 3)), as.vector(dimSums(x, dim = 3)))
})

test_that("urban above the target's level goes back to natural land", {
  y <- toolUrbanCrosswalk(urbanInput(c(12, 12, 12)), urbanTarget(), year = 2025)
  expect_equal(as.vector(y[, , "urban"]), c(8, 8, 8))
  expect_equal(as.vector(y[, , "primn"] + y[, , "secdn"]), rep(84, 3))
})

test_that("urban keeps its level when natural land runs short", {
  x <- urbanInput(c(0, 0, 0))
  # natural land shrinks to almost nothing by the last year
  x[, 2100, "primn"] <- 1
  x[, 2100, "secdn"] <- 0
  x[, 2100, "c3ann"] <- 100 + 79
  y <- toolUrbanCrosswalk(x, urbanTarget(), year = 2025)
  expect_equal(as.vector(y[, , "urban"]), c(8, 8, 8))
  expect_equal(as.vector(dimSums(y, dim = 3)), as.vector(dimSums(x, dim = 3)))
  expect_true(all(y >= 0))
})
