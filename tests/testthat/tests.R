
# load data
filename <- system.file("extdata", "earthquakes-2020-10-19_15-21-05_+0300.tsv", package = "earthquake")
earthquakes <- read_delim(filename, delim = "\t")

# tests
test_that("eq_location_clean", {
  s <- eq_location_clean("ITALY: VERONA")
  expect_equal(s, "Verona")
})

test_that("eq_country_clean", {
  s <- eq_country_clean("ITALY: VERONA")
  expect_equal(s, "Italy")
})

test_that("eq_clean_data returns a data frame", {
  clean_data <- eq_clean_data(earthquakes)
  expect_is(clean_data, "data.frame")
})

test_that("geom_timline is a geom", {
  clean_data <- eq_clean_data(earthquakes)
  geom <- ggplot(clean_data,aes(x=DATE,y = COUNTRY, color = TOTAL_DEATHS, size = TOTAL_DEATHS)) +
    geom_timeline(alpha=.5)
  expect_is(geom, "ggplot")
})

test_that("geom_timline w/ geom_timeline_label is a geom", {
  clean_data <- eq_clean_data(earthquakes)
  geom <- ggplot(clean_data,aes(x=DATE,y = COUNTRY, color = TOTAL_DEATHS, size = TOTAL_DEATHS)) +
    geom_timeline(alpha=.5) +
    geom_timeline_label(aes(label = LOCATION_NAME, n_max = 5))
  expect_is(geom, "ggplot")
})

test_that("eq_create_label creates label", {
  clean_data <- eq_clean_data(earthquakes)
  popup_text <- eq_create_label(clean_data)
  expect_that(class(popup_text), equals("character") )
})

test_that("eq_map creates leaflet", {
  clean_data <- eq_clean_data(earthquakes)
  x <- eq_map(clean_data, annot_col = "COUNTRY")
  expect_is(x, "leaflet")
})


