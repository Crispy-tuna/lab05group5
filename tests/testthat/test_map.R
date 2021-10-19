library(ggmap)
test_that("Wrong zoom level, zoom_l hsould be [9,12]", {
  expect_error(openmap_by_name("stockholm", 8, "toner"))
  expect_error(openmap_by_name("stockholm", 13, "toner"))
})

test_that("Can not find the map type,try (terrain, toner or watercolor)", {
  expect_error(openmap_by_name("stockholm", 10, "watermap"))
  expect_error(openmap_by_name("stockholm", 10, "weathermap"))
})

test_that("Missing parameter", {
  expect_error(openmap_by_name("stockholm", "toner"))
})

test_that("Zoom level is greater than 8", {
  expect_that(get_infor("stockholm", 11, "toner")$name,
              equals("stockholm"))
})

test_that("Zoom level is less than 13", {
  a = get_infor("stockholm", 11, "toner")
  expect_true(a$zoom_l < 13)
})

test_that("Zoom level is numeric", {
  expect_true(is.numeric(get_infor("stockholm", 11, "toner")$zoom_l))
})

test_that("name is character", {
  expect_true(is.character(get_infor("stockholm", 11, "toner")$name))
})

test_that("map_type is character", {
  expect_true(is.character(get_infor("stockholm", 11, "toner")$map_type))
})

test_that("zoom_l is numeric", {
  expect_that(get_infor("stockholm", 11, "toner")$zoom_l,equals(11))
})


test_that("return a list", {
  expect_type(get_infor("stockholm", 11, "toner"), "list")
})

