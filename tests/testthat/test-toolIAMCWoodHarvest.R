harvestHistory <- function(weight = c(primf = 40, secmf = 60), area = c(primf = 2, secmf = 2)) {
  w <- new.magpie(c("A.1", "B.2"), 2025, names(weight), fill = 0)
  a <- new.magpie(c("A.1", "B.2"), 2025, names(area), fill = 0)
  for (s in names(weight)) w[, , s] <- weight[[s]]
  for (s in names(area)) a[, , s] <- area[[s]]
  list(weight = w, area = a)
}

demandOf <- function(values, years = c(2025, 2050)) {
  d <- new.magpie(c("A.1", "B.2"), years, "roundwood", fill = 0)
  for (k in seq_along(years)) d[, years[k], ] <- values[k]
  d
}

volumeOf <- function(value = 10) {
  v <- new.magpie(c("A.1", "B.2"), 2025, "roundwood", fill = value)
  v
}

test_that("the calibration year reproduces the target's harvest", {
  h <- harvestHistory()

  out <- toolIAMCWoodHarvest(demandOf(c(10, 10)), h$weight, h$area, volumeOf(10))

  # 100 kg C of harvest against a reported 10, so 10 kg C per unit reported
  expect_equal(as.vector(out$weight["A.1", 2025, c("primf", "secmf")]), c(40, 60))
  expect_equal(as.vector(out$area["A.1", 2025, c("primf", "secmf")]), c(2, 2))
})

test_that("the scenario supplies the trajectory", {
  h <- harvestHistory()

  out <- toolIAMCWoodHarvest(demandOf(c(10, 15)), h$weight, h$area, volumeOf(10))

  # reported harvest rises by half, so harvest does, keeping the source shares
  expect_equal(as.vector(out$weight["A.1", 2050, c("primf", "secmf")]), c(60, 90))
  expect_equal(as.vector(out$area["A.1", 2050, c("primf", "secmf")]), c(3, 3))
})

test_that("area follows each source's own carbon per hectare", {
  # secmf yields twice the carbon per hectare that primf does
  h <- harvestHistory(weight = c(primf = 50, secmf = 50), area = c(primf = 5, secmf = 2.5))

  out <- toolIAMCWoodHarvest(demandOf(c(10, 20)), h$weight, h$area, volumeOf(10))

  expect_equal(as.vector(out$area["A.1", 2050, c("primf", "secmf")]), c(10, 5))
})

test_that("a source never harvested historically stays unharvested", {
  h <- harvestHistory(weight = c(primf = 0, secmf = 100), area = c(primf = 0, secmf = 4))

  out <- toolIAMCWoodHarvest(demandOf(c(10, 10)), h$weight, h$area, volumeOf(10))

  expect_equal(as.vector(out$weight[, , "primf"]), rep(0, 4))
  expect_equal(as.vector(out$area[, , "primf"]), rep(0, 4))
})

test_that("a region with no history or no demand gets no harvest", {
  h <- harvestHistory()
  h$weight["B.2", , ] <- 0
  h$area["B.2", , ] <- 0
  demand <- demandOf(c(10, 10))
  demand["A.1", 2050, ] <- 0

  out <- toolIAMCWoodHarvest(demand, h$weight, h$area, volumeOf(10))

  expect_equal(as.vector(out$weight["B.2", , ]), rep(0, 4))
  expect_equal(as.vector(out$weight["A.1", 2050, ]), c(0, 0))
  expect_true(all(out$weight >= 0), all(out$area >= 0))
})

test_that("a region reporting too little harvest takes the global factor", {
  h <- harvestHistory()
  volume <- volumeOf(10)
  volume["B.2", , ] <- 0.001  # B reports next to nothing against the same history

  expect_message(out <- toolIAMCWoodHarvest(demandOf(c(10, 10)), h$weight, h$area, volume),
                 "report too little harvest")

  # B's own factor would be 100000 per unit; the global one is 100/10.001
  globalFactor <- 200 / 10.001
  expect_equal(as.vector(dimSums(out$weight["B.2", 2025, ], dim = 3)), 10 * globalFactor)
  # A still calibrates against its own history
  expect_equal(as.vector(dimSums(out$weight["A.1", 2025, ], dim = 3)), 100)
})
