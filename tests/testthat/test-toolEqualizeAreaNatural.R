areaFixture <- function() {
  categories <- c("crop", "pastr", "primf", "secdf", "primn", "secdn")
  x <- new.magpie(c("A", "B"), c(2020, 2050), categories, fill = 0)
  x["A", 2020, ] <- c(100, 50, 40, 20, 60, 30)
  x["A", 2050, ] <- c(150, 50, 30, 20, 40, 10)
  x["B", 2020, ] <- c(10, 10, 5, 5, 5, 5)
  x["B", 2050, ] <- c(10, 10, 5, 5, 5, 5)
  y <- new.magpie(c("A", "B"), 2020, categories, fill = 0)
  y["A", , ] <- c(100, 50, 40, 20, 60, 30) * 0.8
  y["B", , ] <- c(12, 12, 6, 6, 6, 6)
  list(x = x, y = y)
}

test_that("managed land and every change are kept, totals match the target", {
  f <- areaFixture()
  out <- toolEqualizeAreaNatural(f$x, f$y)
  expect_equal(as.vector(out[, , c("crop", "pastr")]), as.vector(f$x[, , c("crop", "pastr")]))
  expect_equal(as.vector(out[, 2050, ] - out[, 2020, ]), as.vector(f$x[, 2050, ] - f$x[, 2020, ]))
  expect_equal(as.vector(dimSums(out[, 2020, ], dim = 3)), as.vector(dimSums(f$y, dim = 3)))
  expect_true(all(out >= 0))
})

test_that("a difference natural land cannot absorb falls back to scaling", {
  f <- areaFixture()
  f$y["A", , ] <- f$y["A", , ] * 0.5
  out <- suppressWarnings(toolEqualizeAreaNatural(f$x, f$y))
  expect_equal(as.vector(dimSums(out[, 2020, ], dim = 3)), as.vector(dimSums(f$y, dim = 3)), tolerance = 1e-8)
  ratio <- as.vector(out["A", , "crop"] / f$x["A", , "crop"])
  expect_equal(ratio[1], ratio[2])
  expect_lt(ratio[1], 1)
})
