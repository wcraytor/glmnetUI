# Ported from earthUI: path composition, country schema, and naming helpers
# for the shared regProj model. Pure computation — no filesystem writes except
# the explicit create=TRUE case, which uses a tempdir.

test_that("country_schema returns expected admin levels", {
  expect_identical(country_schema("us"), c("state", "county", "city"))
  expect_identical(country_schema("it"),
                   c("regione", "provincia", "comune"))
  expect_identical(country_schema("sg"), "planning_area")
  # Unknown country -> generic 2-level fallback
  expect_identical(country_schema("xx"), c("region", "city"))
  # Case-insensitive
  expect_identical(country_schema("US"), country_schema("us"))
})

test_that("country_choices returns a non-empty named vector", {
  ch <- country_choices()
  expect_true(length(ch) >= 24L)
  expect_true(all(grepl("^[a-z]{2}$", unname(ch))))
  expect_true(any(unname(ch) == "us"))
  expect_true(any(unname(ch) == "sg"))
})

test_that("os_detect returns a recognized OS", {
  expect_true(os_detect() %in% c("mac", "ubuntu", "win11", "unknown"))
})

test_that("regproj_flat_segment validates inputs", {
  expect_error(regproj_flat_segment("usa", c("ca"), "p"), "ISO")
  expect_error(regproj_flat_segment("us", c("ca", "Bad_Code"), "p"),
               "no underscores")
  expect_error(regproj_flat_segment("us", c("ca", "081", "burlin"),
                                     "Bad Name"),
               "must match")
  expect_error(regproj_flat_segment("us", c("ca", "081", "burlin"),
                                     paste(rep("x", 25), collapse = "")),
               "24 chars")
})

test_that("regproj_parse_flat rejects malformed input", {
  expect_null(regproj_parse_flat(""))
  expect_null(regproj_parse_flat(NULL))
  expect_null(regproj_parse_flat("us"))            # no project name
  expect_null(regproj_parse_flat("us_ca_081"))     # missing project (US needs 3 admin)
})

test_that("regproj_path composes the expected path", {
  in_path  <- regproj_path("appr", "us", c("ca", "081", "burlin"),
                            "lakemerritt", os = "mac",
                            in_or_out = "in",
                            root = "/tmp/regproj_test")
  expect_match(in_path, "appr/us_ca_081_burlin_lakemerritt/mac_in$")

  out_path <- regproj_path("appr", "us", c("ca", "081", "burlin"),
                            "lakemerritt", os = "mac",
                            in_or_out = "out", method = "glmnet",
                            root = "/tmp/regproj_test")
  expect_match(out_path, "appr/us_ca_081_burlin_lakemerritt/mac_out_glmnet$")
})

test_that("regproj_path validates purpose, country, level depth, OS", {
  expect_error(regproj_path("foo", "us", c("ca", "081", "burlin"),
                             "p", root = tempdir()), "purpose")
  expect_error(regproj_path("appr", "us", c("ca"), "p",
                             root = tempdir()), "must have 3 entries")
  expect_error(regproj_path("appr", "us", c("ca", "081", "burlin"),
                             "p", os = "freebsd", root = tempdir()),
               "os")
})

test_that("regproj_path with create=TRUE makes the directory", {
  root <- tempfile("regproj_path_create_")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  out <- regproj_path("appr", "us", c("ca", "081", "burlin"),
                      "create_test", os = "mac", in_or_out = "in",
                      root = root, create = TRUE)
  expect_true(dir.exists(out))
})

test_that("city_abbreviation strips non-alphanumerics + truncates to 6", {
  expect_equal(city_abbreviation("Oakland"),               "oaklan")
  expect_equal(city_abbreviation("Carmel-by-the-Sea"),     "carmel")
  expect_equal(city_abbreviation("Half Moon Bay"),         "halfmo")
  expect_equal(city_abbreviation("South San Francisco"),   "souths")
})

test_that("city_abbreviation appends _N for collisions", {
  used <- character(0)
  c1 <- city_abbreviation("Oakland", used);              used <- c(used, c1)
  c2 <- city_abbreviation("Oakville", used);             used <- c(used, c2)
  c3 <- city_abbreviation("Oak Hills", used);            used <- c(used, c3)
  expect_equal(c1, "oaklan")
  expect_equal(c2, "oakvil")
  expect_equal(c3, "oakhil")
  # Now true collision: another "Oakland" lookalike
  c4 <- city_abbreviation("Oaklands", used)
  expect_equal(c4, "oaklan_1")
})

test_that("city_abbreviation handles all-non-alpha names", {
  expect_match(city_abbreviation("---"), "^city")
  expect_equal(city_abbreviation("123 Main"), "123mai")
})
