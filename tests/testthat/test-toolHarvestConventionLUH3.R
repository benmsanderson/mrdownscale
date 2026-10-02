test_that("the primary-to-secondary flow moves once, through harvest", {
  items <- c("primf_harv", "primf_bioh", "secdf_harv", "secdf_bioh",
             "primn_harv", "primn_bioh", "secnf_harv", "secnf_bioh",
             "primf_to_secdf", "primf_to_c3ann", "secdf_to_c3ann", "primn_to_secdn")
  x <- new.magpie(c("A.1", "B.2"), 2030, items, fill = 0)
  # cell A: 0.10 of primary forest lost, 0.08 of it to secondary, 0.02 cleared
  # for crops; the harvest area is the whole decline, as mrdownscale writes it
  x["A.1", , "primf_harv"] <- 0.10
  x["A.1", , "primf_bioh"] <- 100
  x["A.1", , "secdf_harv"] <- 0.02
  x["A.1", , "secdf_bioh"] <- 10
  x["A.1", , "primf_to_secdf"] <- 0.08
  x["A.1", , "primf_to_c3ann"] <- 0.02
  # cell B: primary non-forest harvest with no primary non-forest lost at all
  x["B.2", , "primn_harv"] <- 0.05
  x["B.2", , "primn_bioh"] <- 40

  y <- toolHarvestConventionLUH3(x)

  expect_false(any(c("primf_to_secdf", "primn_to_secdn") %in% getItems(y, dim = 3)))
  # the harvest area is kept, with all its carbon: the cell's clearing for
  # crops is routed through secondary forest, which the harvest just created
  expect_equal(as.vector(y["A.1", , "primf_harv"]), 0.10)
  expect_equal(as.vector(y["A.1", , "primf_bioh"]), 100)
  expect_equal(as.vector(y["A.1", , "primf_to_c3ann"]), 0)
  expect_equal(as.vector(y["A.1", , "secdf_to_c3ann"]), 0.02)
  expect_equal(as.vector(y["A.1", , c("secdf_harv", "secdf_bioh")]), c(0.02, 10))
  # harvest area no primary land lost backs goes to the secondary source
  expect_equal(as.vector(y["B.2", , c("primn_harv", "primn_bioh")]), c(0, 0))
  expect_equal(as.vector(y["B.2", , c("secnf_harv", "secnf_bioh")]), c(0.05, 40))

  # total harvested carbon is unchanged
  bioh <- grep("_bioh$", items, value = TRUE)
  expect_equal(sum(y[, , bioh]), sum(x[, , bioh]))
  # LUH3's closure: primary loss = its transitions out + its harvest area,
  # which is what the transitions alone said before
  lossBefore <- as.vector(x["A.1", , "primf_to_secdf"] + x["A.1", , "primf_to_c3ann"])
  lossAfter <- as.vector(y["A.1", , "primf_to_c3ann"] + y["A.1", , "primf_harv"])
  expect_equal(lossAfter, lossBefore)
  # and every other state changes as before: crops gain the same, secondary
  # forest gains the harvest and loses what is cleared from it
  expect_equal(as.vector(y["A.1", , "primf_to_c3ann"] + y["A.1", , "secdf_to_c3ann"]),
               as.vector(x["A.1", , "primf_to_c3ann"] + x["A.1", , "secdf_to_c3ann"]))
  expect_equal(as.vector(y["A.1", , "primf_harv"] - y["A.1", , "secdf_to_c3ann"]),
               as.vector(x["A.1", , "primf_to_secdf"] - x["A.1", , "secdf_to_c3ann"]))
})

test_that("only conversions the harvest area needs are routed through secondary land", {
  items <- c("primf_harv", "primf_bioh", "secdf_harv", "secdf_bioh",
             "primf_to_secdf", "primf_to_c3ann", "primf_to_pastr", "secdf_to_c3ann")
  x <- new.magpie("A.1", 2030, items, fill = 0)
  # harvest 0.05; 0.02 flowed to secondary, 0.06 cleared for crops, 0.02 for pasture
  x[, , "primf_harv"] <- 0.05
  x[, , "primf_bioh"] <- 50
  x[, , "primf_to_secdf"] <- 0.02
  x[, , "primf_to_c3ann"] <- 0.06
  x[, , "primf_to_pastr"] <- 0.02
  y <- toolHarvestConventionLUH3(x)
  # 0.03 routed, in proportion: 0.0225 of the crop clearing, 0.0075 of the pasture
  expect_equal(as.vector(y[, , "primf_harv"]), 0.05)
  expect_equal(as.vector(y[, , "primf_bioh"]), 50)
  expect_equal(as.vector(y[, , c("primf_to_c3ann", "primf_to_pastr")]), c(0.0375, 0.0125))
  expect_equal(as.vector(y[, , c("secdf_to_c3ann", "secdf_to_pastr")]), c(0.0225, 0.0075))
})

test_that("data without gross transitions pass through unchanged", {
  x <- new.magpie("A.1", 2030, c("primf_harv", "primf_bioh", "secdf_harv", "secdf_bioh"), fill = 1)
  expect_identical(toolHarvestConventionLUH3(x), x)
})
