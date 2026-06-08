# Project-name auto-stamping + flat-segment round-trip across countries.
# The new-project naming must work for every political naming convention, i.e.
# any admin-level depth returned by country_schema(). These tests exercise the
# country-agnostic helper and the encode/decode round-trip.

test_that("regproj_new_project_name validates the typed name and prepends a stamp", {
  when <- as.POSIXct("2026-06-04 13:15:00", tz = "UTC")

  expect_equal(regproj_new_project_name("myproj", when), "20260604-131500_myproj")
  expect_equal(regproj_new_project_name("a", when),      "20260604-131500_a")
  expect_equal(regproj_new_project_name("ab-cd_12", when),
               "20260604-131500_ab-cd_12")

  # 8 chars is the max; 9 is rejected.
  expect_silent(regproj_new_project_name("abcd1234", when))      # 8 ok
  expect_error(regproj_new_project_name("abcd12345", when),
               "8 characters")                                    # 9 too long
  # Empty / illegal characters rejected.
  expect_error(regproj_new_project_name("", when))
  expect_error(regproj_new_project_name("  ", when))
  expect_error(regproj_new_project_name("bad name", when))        # space
  expect_error(regproj_new_project_name("accént", when))     # non-ASCII
})

test_that("stamp format is YYYYMMDD-HHMMSS and always 15 chars before the name", {
  when <- as.POSIXct("2026-12-31 09:05:07", tz = "UTC")
  out  <- regproj_new_project_name("p", when)
  expect_match(out, "^[0-9]{8}-[0-9]{6}_p$")
  expect_equal(nchar(sub("_.*$", "", out)), 15L)
})

test_that("flat segment + parse round-trips across foreign naming conventions", {
  when <- as.POSIXct("2026-06-04 13:15:00", tz = "UTC")
  proj <- regproj_new_project_name("p1", when)  # "20260604-131500_p1"

  # One case per shipped convention, with the correct admin-level DEPTH.
  cases <- list(
    us = c("ca", "081", "burlin"),       # state / county / city
    gb = c("eng", "ldn", "soho"),        # region / district / place
    fr = c("idf", "75", "paris"),        # region / departement / commune
    de = c("by", "m", "schwab"),         # bundesland / kreis / gemeinde
    it = c("laz", "rm", "roma"),         # regione / provincia / comune
    jp = c("tokyo", "shibuya", "ebisu"), # prefecture / ward / district
    sg = c("downtwn")                    # single planning_area
  )

  for (cc in names(cases)) {
    levels <- cases[[cc]]
    # The test fixture must match the country's real schema depth.
    expect_equal(length(levels), length(country_schema(cc)), info = cc)

    flat <- regproj_flat_segment(cc, levels, proj)

    # Folder shape: <country>_<levels...>_<stamp>_<name>
    expect_equal(flat,
                 paste(c(cc, levels, proj), collapse = "_"), info = cc)

    parsed <- regproj_parse_flat(flat)
    expect_false(is.null(parsed), info = cc)
    expect_equal(parsed$country,      cc,     info = cc)
    expect_equal(parsed$levels,       levels, info = cc)
    # The timestamp + typed name survive as the project_name segment.
    expect_equal(parsed$project_name, proj,   info = cc)
  }
})

test_that("unknown country falls back to region/city and still round-trips", {
  when <- as.POSIXct("2026-06-04 13:15:00", tz = "UTC")
  proj <- regproj_new_project_name("x", when)

  expect_equal(country_schema("zz"), c("region", "city"))
  flat   <- regproj_flat_segment("zz", c("reg1", "city1"), proj)
  parsed <- regproj_parse_flat(flat)

  expect_equal(parsed$country,      "zz")
  expect_equal(parsed$levels,       c("reg1", "city1"))
  expect_equal(parsed$project_name, proj)
})

test_that("a stamped 8-char name stays within the flat-segment length limit", {
  when <- as.POSIXct("2026-06-04 13:15:00", tz = "UTC")
  proj <- regproj_new_project_name("abcd1234", when)  # 15 + 1 + 8 = 24 chars
  expect_equal(nchar(proj), 24L)
  # regproj_flat_segment caps the project segment at 24 chars — must not error.
  expect_silent(regproj_flat_segment("us", c("ca", "081", "burlin"), proj))
})
