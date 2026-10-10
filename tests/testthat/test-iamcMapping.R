test_that("IAMC energy crops map to dedicated bioenergy only", {
  # common-definitions: Land Cover|Cropland|Energy Crops is cropland for
  # second-generation bioenergy crops (short rotation grasses and trees)
  mapping <- utils::read.csv2(system.file("extdata", "referenceMappings", "iamc.csv", package = "mrdownscale"))
  energy <- mapping$reference[mapping$data == "Land_Cover_Cropland_Energy_Crops"]
  expect_setequal(energy, c("betr, irrigated", "betr, rainfed", "begr, irrigated", "begr, rainfed"))
  firstGeneration <- grep("biofuel_1st_gen", mapping$reference, value = TRUE)
  expect_length(firstGeneration, 10)
  expect_true(all(mapping$data[mapping$reference %in% firstGeneration] == "Land_Cover_Cropland_Other"))
})
