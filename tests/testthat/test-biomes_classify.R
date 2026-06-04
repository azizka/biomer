make_df <- function() {
  data.frame(
    decimalLongitude = c(10.1, -60.5, 35.5, 100.2),
    decimalLatitude  = c(50.0,  -3.2,  -1.2,  20.3),
    species          = c("spA", "spB", "spC", "spD")
  )
}

test_that("biomes_classify appends one _name column per layer (value='name')", {
  df     <- make_df()
  layers <- biomes_get()

  out <- suppressWarnings(suppressMessages(
    biomes_classify(x = df, biome = layers[[1]], value = "name")
  ))

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), nrow(df))
  # default append = TRUE: original columns kept + one appended column
  expect_equal(ncol(out), ncol(df) + 1)
  expect_true(all(names(df) %in% names(out)))
  expect_match(names(out)[ncol(out)], "_name$")
})

test_that("biomes_classify with append=FALSE returns only classification columns", {
  df     <- make_df()
  layers <- biomes_get()

  out <- suppressWarnings(suppressMessages(
    biomes_classify(x = df, biome = layers[[1]], value = "name",
                    append = FALSE)
  ))

  expect_equal(nrow(out), nrow(df))
  expect_equal(ncol(out), 1)
  expect_match(names(out)[1], "_name$")
})

test_that("biomes_classify with value='ID' returns *_value columns", {
  df     <- make_df()
  layers <- biomes_get()

  out <- suppressWarnings(suppressMessages(
    biomes_classify(x = df, biome = layers[[c(1, 2)]], value = "ID",
                    append = FALSE)
  ))

  expect_equal(nrow(out), nrow(df))
  expect_equal(ncol(out), 2)
  expect_true(all(grepl("_value$", names(out))))
})

test_that("biomes_classify with value='both' interleaves _value and _name", {
  df     <- make_df()
  layers <- biomes_get()

  out <- suppressWarnings(suppressMessages(
    biomes_classify(x = df, biome = layers[[c(1, 2)]], value = "both",
                    append = FALSE)
  ))

  expect_equal(ncol(out), 4)
  expect_match(names(out)[1], "_value$")
  expect_match(names(out)[2], "_name$")
  expect_match(names(out)[3], "_value$")
  expect_match(names(out)[4], "_name$")
})

test_that("biomes_classify replaces unknown azonal values with a fallback name", {
  df     <- make_df()
  layers <- biomes_get()

  out <- suppressWarnings(suppressMessages(
    biomes_classify(x = df, biome = layers[[1:3]], value = "name",
                    append = FALSE)
  ))

  # No NA in name columns: azonal classes fall back to a placeholder
  # (the only NA possible here is for points outside any cell).
  all_names <- unlist(out)
  expect_true(
    !any(is.na(all_names)) ||
      all(grepl("^azonal", all_names[is.na(all_names) == FALSE]))
  )
})

test_that("biomes_classify validates 'value'", {
  df <- make_df()
  expect_error(
    suppressWarnings(biomes_classify(x = df, value = "bogus")),
    "value"
  )
})
