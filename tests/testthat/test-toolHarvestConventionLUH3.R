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
  # harvest area no primary land lost backs, with no other cell to take its
  # carbon, goes to the secondary source
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

test_that("harvest carbon no decline backs moves to the region's uncovered decline", {
  items <- c("primf_harv", "primf_bioh", "secdf_harv", "secdf_bioh",
             "primf_to_secdf", "primf_to_c3ann", "primn_harv", "primn_bioh",
             "secnf_harv", "secnf_bioh", "primn_to_secdn")
  x <- new.magpie(c("A.1", "A.2", "B.3"), 2030, items, fill = 0)
  # A.1 carries harvest (0.05, 50 kg C) but loses no primary forest; A.2 loses
  # 0.04 to crops with no harvest; B.3, another region, loses 0.03 likewise
  x["A.1", , "primf_harv"] <- 0.05
  x["A.1", , "primf_bioh"] <- 50
  x["A.2", , "primf_to_c3ann"] <- 0.04
  x["B.3", , "primf_to_c3ann"] <- 0.03
  y <- toolHarvestConventionLUH3(x, region = c("A", "A", "B"))
  # harvest area is each cell's decline; conversions go through secondary forest
  expect_equal(as.vector(y[, , "primf_harv"]), c(0, 0.04, 0.03))
  expect_equal(as.vector(y[, , "primf_to_c3ann"]), c(0, 0, 0))
  expect_equal(as.vector(y[, , "secdf_to_c3ann"]), c(0, 0.04, 0.03))
  # A.1's carbon moves to A.2 at its own density (1000 per unit area), as far as
  # A.2's decline reaches; the rest stays in A.1, as secondary harvest
  expect_equal(as.vector(y[, , "primf_bioh"]), c(0, 40, 0))
  expect_equal(as.vector(y[, , c("secdf_harv", "secdf_bioh")]), c(0.01, 0, 0, 10, 0, 0))
  # nothing crosses regions, and total carbon is unchanged
  expect_equal(sum(y[, , c("primf_bioh", "secdf_bioh")]), 50)
})

test_that("data without gross transitions pass through unchanged", {
  x <- new.magpie("A.1", 2030, c("primf_harv", "primf_bioh", "secdf_harv", "secdf_bioh"), fill = 1)
  expect_identical(toolHarvestConventionLUH3(x), x)
})
