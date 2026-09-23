iamcData <- function(..., regions = c("R1", "R2"), years = c(2020, 2050)) {
  values <- list(...)
  out <- expand.grid(Region = regions, Year = years, Variable = names(values),
                     stringsAsFactors = FALSE)
  out$Value <- unlist(values, use.names = FALSE)[match(out$Variable, names(values))]
  return(out)
}

# a model that partitions its land tree cleanly: forest parts add up to the
# forest total, and the categories add up to Land Cover
clean <- iamcData(Land_Cover = 100,
                  Land_Cover_Cropland = 20,
                  Land_Cover_Cropland_Energy_Crops = 5,
                  Land_Cover_Pasture = 25,
                  Land_Cover_Forest = 40,
                  Land_Cover_Forest_Primary = 10,
                  Land_Cover_Forest_Secondary = 25,
                  Land_Cover_Forest_Planted = 5,
                  Land_Cover_Built_Up_Area = 5,
                  Land_Cover_Other_Natural = 10)

test_that("a clean land tree is partitioned as reported", {
  x <- toolIAMCLandCategories(clean)

  expect_setequal(getItems(x, dim = 3),
                  c("Land_Cover_Forest_Primary", "Land_Cover_Forest_Secondary",
                    "Land_Cover_Forest_Planted", "Land_Cover_Pasture",
                    "Land_Cover_Built_Up_Area", "Land_Cover_Cropland_Energy_Crops",
                    "Land_Cover_Cropland_Other", "Land_Cover_Other_Natural"))

  # every category is passed through untouched, and they add up to Land Cover
  expect_equal(as.vector(x[, 2020, "Land_Cover_Forest_Primary"]), c(10, 10))
  expect_equal(as.vector(x[, 2020, "Land_Cover_Cropland_Other"]), c(15, 15))
  expect_equal(as.vector(x[, 2020, "Land_Cover_Other_Natural"]), c(10, 10))
  expect_equal(as.vector(dimSums(x, dim = 3)), rep(100, 4))
})

test_that("area Land Cover does not account for goes to other natural land", {
  # a model reporting Other Land beside the rest: 10 of the 100 is unaccounted
  withOther <- clean
  withOther$Value[withOther$Variable == "Land_Cover_Other_Natural"] <- 5
  withOther$Value[withOther$Variable == "Land_Cover_Pasture"] <- 20

  expect_warning(x <- toolIAMCLandCategories(withOther), "miss the Land_Cover total")

  expect_equal(as.vector(x[, 2020, "Land_Cover_Other_Natural"]), c(15, 15))
  expect_equal(as.vector(dimSums(x, dim = 3)), rep(100, 4))
})

test_that("forest parts are rescaled to the reported forest total", {
  # parts sum to 40.2 against a total of 40, within tolerance
  drift <- clean
  drift$Value[drift$Variable == "Land_Cover_Forest_Secondary"] <- 25.2

  x <- toolIAMCLandCategories(drift)

  expect_equal(as.vector(dimSums(x[, , c("Land_Cover_Forest_Primary",
                                         "Land_Cover_Forest_Secondary",
                                         "Land_Cover_Forest_Planted")], dim = 3)),
               rep(40, 4))
  expect_equal(as.vector(dimSums(x, dim = 3)), rep(100, 4))
})

test_that("a nested forest split is set aside, keeping the forest total", {
  # AIM reports secondary forest equal to its forest total, primary inside it
  nested <- clean
  nested$Value[nested$Variable == "Land_Cover_Forest_Secondary"] <- 40

  expect_warning(x <- toolIAMCLandCategories(nested), "does not add up to the forest total")

  # the total is kept; primary is left to harmonization, which derives it from
  # the target whatever the input said
  expect_equal(as.vector(dimSums(x[, 2020, c("Land_Cover_Forest_Primary", "Land_Cover_Forest_Secondary",
                                             "Land_Cover_Forest_Planted")], dim = 3)), c(40, 40))
  expect_equal(as.vector(x[, 2020, "Land_Cover_Forest_Primary"]), c(0, 0))
  expect_equal(as.vector(x[, 2020, "Land_Cover_Forest_Planted"]), c(5, 5))
  expect_equal(as.vector(dimSums(x, dim = 3)), rep(100, 4))
})

test_that("a model reporting no forest split keeps its forest total", {
  # GCAM reports Land Cover|Forest and nothing below it
  noSplit <- clean[!clean$Variable %in% c("Land_Cover_Forest_Primary", "Land_Cover_Forest_Secondary",
                                          "Land_Cover_Forest_Planted"), ]

  expect_message(x <- toolIAMCLandCategories(noSplit), "no forest split is reported")

  expect_equal(as.vector(x[, 2020, "Land_Cover_Forest_Secondary"]), c(40, 40))
  expect_equal(as.vector(x[, 2020, "Land_Cover_Forest_Primary"]), c(0, 0))
  expect_equal(as.vector(dimSums(x, dim = 3)), rep(100, 4))
})

test_that("categories that are not reported are filled with zeros", {
  sparse <- clean[!clean$Variable %in% c("Land_Cover_Built_Up_Area",
                                         "Land_Cover_Cropland_Energy_Crops"), ]

  # the unreported built-up area is not lost: it lands in other natural land,
  # and the shortfall against Land Cover is reported
  expect_warning(x <- toolIAMCLandCategories(sparse), "miss the Land_Cover total")

  expect_equal(as.vector(x[, 2020, "Land_Cover_Built_Up_Area"]), c(0, 0))
  expect_equal(as.vector(x[, 2020, "Land_Cover_Cropland_Energy_Crops"]), c(0, 0))
  # cropland is then all "other", and the built-up area lands in other natural
  expect_equal(as.vector(x[, 2020, "Land_Cover_Cropland_Other"]), c(20, 20))
  expect_equal(as.vector(x[, 2020, "Land_Cover_Other_Natural"]), c(15, 15))
  expect_equal(as.vector(dimSums(x, dim = 3)), rep(100, 4))
})

test_that("a missing required variable is an error", {
  expect_error(toolIAMCLandCategories(clean[clean$Variable != "Land_Cover_Forest", ]),
               "Missing required variables")
})

test_that("only years reporting every variable are kept", {
  # a third year where just Land Cover is reported
  extra <- rbind(clean, data.frame(Region = c("R1", "R2"), Year = 2100,
                                   Variable = "Land_Cover", Value = 100))

  x <- toolIAMCLandCategories(extra)

  expect_identical(getYears(x, as.integer = TRUE), c(2020L, 2050L))
})

test_that("an increase in primary forest is moved into secondary forest", {
  # primary forest ticks up by rounding between the two reported years
  rounding <- clean
  rounding$Value[rounding$Variable == "Land_Cover_Forest_Primary" & rounding$Year == 2050] <- 10.05
  rounding$Value[rounding$Variable == "Land_Cover_Forest_Secondary" & rounding$Year == 2050] <- 24.95

  x <- toolIAMCLandCategories(rounding)

  expect_equal(as.vector(x[, 2050, "Land_Cover_Forest_Primary"]), c(10, 10))
  expect_equal(as.vector(x[, 2050, "Land_Cover_Forest_Secondary"]), c(25, 25))
  expect_equal(as.vector(dimSums(x, dim = 3)), rep(100, 4))
})

test_that("a drifting land total is held at its first year", {
  # IMAGE's reported Land Cover moves by 0.42 Mha over time; the downscaler
  # refuses a stock that is not constant
  drifting <- clean
  drifting$Value[drifting$Variable == "Land_Cover" & drifting$Year == 2050] <- 100.5

  expect_message(x <- toolIAMCLandCategories(drifting), "drifts by up to")

  expect_equal(as.vector(dimSums(x, dim = 3)), rep(100, 4))
  # the difference goes where the rest of the unaccounted area goes
  expect_equal(as.vector(x[, 2050, "Land_Cover_Other_Natural"]),
               as.vector(x[, 2020, "Land_Cover_Other_Natural"]))
})
