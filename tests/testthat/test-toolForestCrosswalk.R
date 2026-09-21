landData <- function(values, years = c(2025, 2050, 2100)) {
  categories <- c("primf", "secdf", "pltns", "primn", "secdn")
  x <- new.magpie(c("A.1", "B.2"), years, categories, fill = 0)
  for (category in names(values)) {
    x[, , category] <- values[[category]]
  }
  return(x)
}

# region A: input has 10 Mha more forest than the target, region B 5 Mha less
input <- landData(list(primf = 20, secdf = 30, pltns = 10, primn = 20, secdn = 20))
target <- landData(list(primf = 20, secdf = 30, pltns = 0, primn = 20, secdn = 20))
target["B.2", , "secdf"] <- 45  # A: 50 of forest against 60; B: 65 against 60

test_that("surplus forest moves to non-forest, primary to primary, secondary to secondary", {
  x <- toolForestCrosswalk(input, target, year = 2025)

  # A's 10 Mha surplus splits as its forest does, 20:30 primary to secondary
  expect_equal(as.vector(x["A.1", 2025, c("primf", "secdf", "primn", "secdn")]), c(16, 24, 24, 26))
})

test_that("a forest shortfall comes back from non-forest the same way", {
  x <- toolForestCrosswalk(input, target, year = 2025)

  # B is 5 Mha short, drawn from non-forest 20:20 primary to secondary
  expect_equal(as.vector(x["B.2", 2025, c("primf", "secdf", "primn", "secdn")]), c(22.5, 32.5, 17.5, 17.5))
})

test_that("every category keeps its change over time, so primary land cannot expand", {
  changing <- input
  changing[, 2050, "primf"] <- 18  # primary forest loss
  changing[, 2050, "primn"] <- 22
  changing[, 2050, "secdf"] <- 25  # secondary forest loss
  changing[, 2050, "secdn"] <- 25

  x <- toolForestCrosswalk(changing, target, year = 2025)

  change <- function(y, category) as.vector(y[, 2050, category] - y[, 2025, category])
  for (category in c("primf", "secdf", "primn", "secdn")) {
    expect_equal(change(x, category), change(changing, category))
  }
  expect_true(all(change(x, "primf") <= 0), all(change(x, "primn") <= 0))
})

test_that("plantations and total area are untouched", {
  x <- toolForestCrosswalk(input, target, year = 2025)

  expect_equal(x[, , "pltns"], input[, , "pltns"])
  expect_equal(dimSums(x, dim = 3), dimSums(input, dim = 3))
})

test_that("the shift never takes more than a category holds", {
  sparse <- input
  sparse["A.1", 2100, "secdf"] <- 4  # less secondary forest than its 6 Mha share
  sparse["A.1", 2100, "secdn"] <- 46

  expect_warning(x <- toolForestCrosswalk(sparse, target, year = 2025), "limited by available area")
  expect_equal(as.vector(x["A.1", 2100, "secdf"]), 0)
  expect_true(all(x >= 0))
  expect_equal(dimSums(x, dim = 3), dimSums(sparse, dim = 3))
})
