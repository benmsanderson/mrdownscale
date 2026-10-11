test_that("the harvest density is the last historical year's, pooled only where that year has none", {
  sources <- c("primf", "secyf")
  bioh <- new.magpie("R", c(2000, 2024), paste0("bioh.", sources), fill = 0)
  area <- new.magpie("R", c(2000, 2024), paste0("wood_harvest_area.", sources), fill = 0)
  # secondary young forest: density falls from 4 to 2 as its area grows
  bioh["R", 2000, "bioh.secyf"] <- 40
  area["R", 2000, "wood_harvest_area.secyf"] <- 10
  bioh["R", 2024, "bioh.secyf"] <- 60
  area["R", 2024, "wood_harvest_area.secyf"] <- 30
  # primary forest harvested only in 2000
  bioh["R", 2000, "bioh.primf"] <- 90
  area["R", 2000, "wood_harvest_area.primf"] <- 1
  d <- toolHarvestDensity(bioh, area)
  expect_equal(as.vector(d["R", , "wood_harvest_area.secyf"]), 2)   # pooled would give 2.5
  expect_equal(as.vector(d["R", , "wood_harvest_area.primf"]), 90)  # no 2024 harvest: pooled
  expect_equal(getItems(d, dim = 3), paste0("wood_harvest_area.", sources))
})
