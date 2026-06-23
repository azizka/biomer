test_that("biomes_get returns a 31-layer SpatRaster", {
  skip_if_no_raster()
  layers <- biomes_get()
  expect_s4_class(layers, "SpatRaster")
  expect_equal(terra::nlyr(layers), 31)
})

test_that("biomes_get rejects extra arguments", {
  expect_error(biomes_get(foo = 1), "does not take any arguments")
})
