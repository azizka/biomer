test_that("biomes_tab works on single-layer ID output (issue #65)", {
  df <- data.frame(
    decimalLongitude = c(10.1, -60.5, 35.5, 100.2),
    decimalLatitude  = c(50.0,  -3.2,  -1.2,  20.3)
  )
  layers <- biomes_get()

  out <- suppressWarnings(suppressMessages(
    biomes_classify(x = df, biome = layers[[1]], value = "ID")
  ))

  tab <- biomes_tab(out, value = "ID")
  expect_s3_class(tab, "data.frame")
  expect_equal(names(tab), c("layer", "biome", "n"))
})

test_that("biomes_tab works on multi-layer name output", {
  df <- data.frame(
    decimalLongitude = c(10.1, -60.5, 35.5, 100.2),
    decimalLatitude  = c(50.0,  -3.2,  -1.2,  20.3)
  )
  layers <- biomes_get()

  out <- suppressWarnings(suppressMessages(
    biomes_classify(x = df, biome = layers[[c(1, 2)]], value = "name")
  ))

  tab <- biomes_tab(out, value = "names")
  expect_equal(names(tab), c("layer", "biome", "n"))
  expect_true(length(unique(tab$layer)) >= 1)
})

test_that("biomes_tab errors when required columns are missing", {
  bad <- data.frame(foo = 1:3, bar = 4:6)
  expect_error(biomes_tab(bad, value = "names"), "_name")
  expect_error(biomes_tab(bad, value = "ID"),    "_value")
})
