test_that("fertilizer on a crop type without area is dropped", {
  fertilizer <- new.magpie("A", 2030, c("fertilizer.c3ann", "fertilizer.c4per"), fill = 1)
  land <- new.magpie("A", 2030, c("c3ann_rainfed", "c3ann_irrigated", "c4per_rainfed", "c4per_irrigated"), fill = 0)
  land[, , "c3ann_rainfed"] <- 2
  out <- suppressMessages(toolFertilizerKgPerHa(fertilizer, land))
  expect_equal(as.vector(out[, , "c3ann"]), 500)
  expect_equal(as.vector(out[, , "c4per"]), 0)
})
