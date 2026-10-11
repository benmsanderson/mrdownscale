test_that("primn declines only with conversion, by its share of the loss", {
  x <- new.magpie("A", c(2025, 2030, 2035, 2040), c("crop", "primn", "secdn"), fill = 0)
  # the input relabels primn as secdn in 2030 without any change in natural land,
  # then natural land shrinks by 30 and grows by 10
  x[, , "crop"] <- c(10, 10, 40, 30)
  x[, , "primn"] <- c(60, 20, 10, 10)
  x[, , "secdn"] <- c(30, 70, 50, 60)
  out <- toolPrimnFromConversion(x, 2025)
  natural <- as.vector(dimSums(out[, , c("primn", "secdn")], dim = 3))
  expect_equal(natural, as.vector(dimSums(x[, , c("primn", "secdn")], dim = 3)))
  expect_equal(as.vector(out[, , "primn"]), c(60, 60, 40, 40))
  expect_equal(as.vector(out[, , "crop"]), as.vector(x[, , "crop"]))
})

test_that("years up to the harmonization start are unchanged", {
  x <- new.magpie("A", c(2020, 2025, 2030), c("primn", "secdn"), fill = 0)
  x[, , "primn"] <- c(70, 60, 20)
  x[, , "secdn"] <- c(20, 30, 70)
  out <- toolPrimnFromConversion(x, 2025)
  expect_equal(as.vector(out[, c(2020, 2025), ]), as.vector(x[, c(2020, 2025), ]))
  expect_equal(as.vector(out[, 2030, "primn"]), 60)
})

test_that("harvest from primary non-forest land moves it to secdn", {
  x <- new.magpie("A", c(2025, 2030), c("primn", "secdn"), fill = 0)
  x[, , "primn"] <- c(60, 60)
  x[, , "secdn"] <- c(30, 30)
  harvest <- new.magpie("A", c(2025, 2030), "primn", fill = 2)
  out <- toolPrimnFromConversion(x, 2025, harvest)
  expect_equal(as.vector(out[, 2030, "primn"]), 50)
  expect_equal(as.vector(out[, 2030, "secdn"]), 40)
})
