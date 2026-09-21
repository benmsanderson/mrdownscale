test_that("primary forest's extrapolated decline does not depend on the output time step", {
  categories <- c("primf", "secdf")
  targetYears <- c(2015:2025, 2030, 2040)
  xTarget <- new.magpie("A.1", targetYears, categories, fill = 0)
  # historical primary forest loss of 2 Mha per year, 100 in 2015 to 80 in 2025
  xTarget[, 2015:2025, "primf"] <- 100 - 2 * (0:10)
  xTarget[, c(2030, 2040), "primf"] <- c(70, 50)
  xTarget[, , "secdf"] <- 200

  # the input reports every 5 years, then every 10, as IAMs and MAgPIE do
  xInput <- new.magpie("A.1", c(2025, 2030, 2040, 2050, 2060), categories, fill = 0)
  xInput[, , "primf"] <- 80
  xInput[, , "secdf"] <- 200

  x <- toolHarmonizeFadeForest(xInput, xTarget, harmonizationPeriod = c(2025, 2050))

  # 2 Mha per year from 80 in 2025, however far apart the reported years are
  expect_equal(as.vector(x[, c(2030, 2040, 2050, 2060), "primf"]), c(70, 50, 30, 10))
  # the forest total is the harmonized one, whatever the split
  expect_equal(as.vector(dimSums(x[, c(2050, 2060), categories], dim = 3)), c(280, 280))
})
