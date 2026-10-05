test_that("planted forest takes its share from the nearest consistent year", {
  years <- c(2080, 2090, 2100)
  forest <- new.magpie(c("A", "B"), years, "forest", fill = 0)
  forest["A", , ] <- c(125.8, 128.5, 131.1)
  forest["B", , ] <- 10
  planted <- new.magpie(c("A", "B"), years, "planted", fill = 0)
  planted["A", , ] <- c(46.6, 49.2, 108.5)   # MESSAGE India+: the 2100 jump
  planted["B", , ] <- 2
  parts <- forest
  parts["A", 2100, ] <- 17.7 + 61.8 + 108.5   # parts exceed the total by 57 Mha
  parts["B", , ] <- 15                          # B never adds up: left as reported
  x <- suppressMessages(toolPlantedFromConsistentYears(planted, forest, parts))
  expect_equal(as.vector(x["A", 2100, ]), 49.2 / 128.5 * 131.1)
  expect_equal(as.vector(x["A", c(2080, 2090), ]), c(46.6, 49.2))
  expect_equal(as.vector(x["B", , ]), c(2, 2, 2))
})
