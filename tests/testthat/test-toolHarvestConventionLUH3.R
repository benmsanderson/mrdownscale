test_that("the primary-to-secondary flow moves once, through harvest", {
  items <- c("primf_harv", "primf_bioh", "secdf_harv", "secdf_bioh",
             "primn_harv", "primn_bioh", "secnf_harv", "secnf_bioh",
             "primf_to_secdf", "primf_to_c3ann", "primn_to_secdn")
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
  # the harvest area is exactly the flow, and carries the carbon it backs
  expect_equal(as.vector(y["A.1", , "primf_harv"]), 0.08)
  expect_equal(as.vector(y["A.1", , "primf_bioh"]), 80)
  # what the flow does not back is harvested from secondary forest instead
  expect_equal(as.vector(y["A.1", , "secdf_harv"]), 0.04)
  expect_equal(as.vector(y["A.1", , "secdf_bioh"]), 30)
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
})

test_that("data without gross transitions pass through unchanged", {
  x <- new.magpie("A.1", 2030, c("primf_harv", "primf_bioh", "secdf_harv", "secdf_bioh"), fill = 1)
  expect_identical(toolHarvestConventionLUH3(x), x)
})

test_that("primary clearing goes through secondary where secondary gains", {
  items <- c("primf_harv", "primf_bioh", "secdf_harv", "secdf_bioh",
             "primf_to_secdf", "primf_to_c3ann", "secdf_to_c3ann")
  cell <- function(secdfToCrop) {
    x <- new.magpie("A.1", 2030, items, fill = 0)
    # 0.10 of primary lost: 0.06 to secondary, 0.04 to crops; harvest area 0.10
    x[, , "primf_harv"] <- 0.10
    x[, , "primf_bioh"] <- 100
    x[, , "primf_to_secdf"] <- 0.06
    x[, , "primf_to_c3ann"] <- 0.04
    x[, , "secdf_to_c3ann"] <- secdfToCrop
    x
  }
  net <- function(y, state) {
    items <- getItems(y, dim = 3)
    into <- grep(paste0("_to_", state, "$"), items, value = TRUE)
    from <- grep(paste0("^", state, "_to_"), items, value = TRUE)
    sum(y[, , into]) - sum(y[, , from])
  }

  # secondary gains (0.06 in, nothing out): the clearing is re-routed
  gaining <- toolHarvestConventionLUH3(cell(0))
  expect_equal(as.vector(gaining[, , "primf_to_c3ann"]), 0)
  expect_equal(as.vector(gaining[, , "secdf_to_c3ann"]), 0.04)
  expect_equal(as.vector(gaining[, , "primf_harv"]), 0.10)
  expect_equal(as.vector(gaining[, , "primf_bioh"]), 100)
  # crops gain what they gained; secondary's net change is its harvest gain
  # minus its losses, the same as before
  expect_equal(net(gaining, "c3ann"), 0.04)
  expect_equal(net(gaining, "secdf") + as.vector(gaining[, , "primf_harv"]), 0.06)

  # secondary loses (0.06 in, 0.08 out): left as clearing of primary
  losing <- toolHarvestConventionLUH3(cell(0.08))
  expect_equal(as.vector(losing[, , "primf_to_c3ann"]), 0.04)
  expect_equal(as.vector(losing[, , "primf_harv"]), 0.06)
})

test_that("re-routing never exceeds the harvest area the cell carries", {
  x <- new.magpie("A.1", 2030, c("primf_harv", "primf_bioh", "secdf_harv", "secdf_bioh",
                                 "primf_to_secdf", "primf_to_c3ann", "secdf_to_c3ann"), fill = 0)
  x[, , "primf_harv"] <- 0.07
  x[, , "primf_bioh"] <- 70
  x[, , "primf_to_secdf"] <- 0.06
  x[, , "primf_to_c3ann"] <- 0.04
  y <- toolHarvestConventionLUH3(x)
  expect_equal(as.vector(y[, , "primf_harv"]), 0.07)
  expect_equal(as.vector(y[, , "primf_to_c3ann"]), 0.03)
  expect_equal(as.vector(y[, , "secdf_to_c3ann"]), 0.01)
})
