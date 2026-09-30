test_that("the time axis is written in days, which a 365-day calendar allows", {
  path <- withr::local_tempfile(fileext = ".nc")
  time <- ncdf4::ncdim_def("time", units = "years since 1970-01-01 0:0:0", vals = c(50, 55, 130))
  v <- ncdf4::ncvar_def("x", units = "1", dim = time)
  nc <- ncdf4::nc_create(path, v)
  ncdf4::ncatt_put(nc, "time", "calendar", "365_day")
  toolTimeAxis(nc)
  ncdf4::nc_close(nc)

  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))
  expect_identical(ncdf4::ncatt_get(nc, "time", "units")$value, "days since 1970-01-01 0:0:0")
  # 2020, 2025 and 2100 on a 365-day calendar
  expect_equal(as.vector(ncdf4::ncvar_get(nc, "time")), c(50, 55, 130) * 365)
})
