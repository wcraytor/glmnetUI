skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

# glmnetUI-specific coverage: register_project() (the projects.sqlite upsert
# helper that earthUI does not have), plus is_project_dir() and the shipped
# reference loader. Roots are tempdirs so the real ~/regProj is never touched.

new_temp_root <- function() {
  d <- tempfile("regproj_reg_test_")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

# Read a single project row back from projects.sqlite.
read_project_row <- function(root, flat) {
  con <- regproj_projects_db_connect(root)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbGetQuery(con,
    "SELECT * FROM projects WHERE flat_segment = ?", params = list(flat))
}

test_that("register_project inserts a project row with parsed geo codes", {
  root <- new_temp_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  flat <- "us_ca_081_burlin_proj1"
  proj_path <- file.path(root, "appr", flat)
  dir.create(proj_path, recursive = TRUE)

  ret <- register_project(proj_path, purpose = "appr", country = "us",
                          levels = c("ca", "081", "burlin"),
                          project_name = "proj1",
                          last_file = "data.csv", root = root)
  expect_identical(ret, flat)

  row <- read_project_row(root, flat)
  expect_equal(nrow(row), 1L)
  expect_equal(row$purpose, "appr")
  expect_equal(row$country, "us")
  expect_equal(row$state,   "ca")
  expect_equal(row$county,  "081")
  expect_equal(row$city,    "burlin")
  expect_equal(row$project_name, "proj1")
  expect_equal(row$last_file, "data.csv")
})

test_that("register_project upserts (no duplicate row) and preserves created_at", {
  root <- new_temp_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  flat <- "us_tx_201_houstn_p"
  proj_path <- file.path(root, "gen", flat)
  dir.create(proj_path, recursive = TRUE)

  register_project(proj_path, purpose = "gen", country = "us",
                   levels = c("tx", "201", "houstn"), project_name = "p",
                   last_file = "first.csv", root = root)
  created1 <- read_project_row(root, flat)$created_at

  # Re-register with a NULL last_file: COALESCE keeps the prior value, and the
  # created_at is preserved while last_used_at is refreshed.
  register_project(proj_path, purpose = "gen", country = "us",
                   levels = c("tx", "201", "houstn"), project_name = "p",
                   last_file = NULL, root = root)
  row <- read_project_row(root, flat)
  expect_equal(nrow(row), 1L)                    # upsert, not a second row
  expect_equal(row$created_at, created1)         # created_at preserved
  expect_equal(row$last_file, "first.csv")       # COALESCE kept prior last_file
  expect_gte(row$last_used_at, row$created_at)   # last_used refreshed
})

test_that("register_project handles short level vectors (NA geo codes)", {
  root <- new_temp_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  flat <- "sg_tanjong_demo"
  proj_path <- file.path(root, "appr", flat)
  dir.create(proj_path, recursive = TRUE)

  register_project(proj_path, purpose = "appr", country = "sg",
                   levels = "tanjong", project_name = "demo", root = root)
  row <- read_project_row(root, flat)
  expect_equal(row$state, "tanjong")
  expect_true(is.na(row$county))
  expect_true(is.na(row$city))
})

test_that("is_project_dir recognizes valid project leaves only", {
  root <- new_temp_root()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  good <- file.path(root, "appr", "us_ca_081_burlin_proj")
  dir.create(good, recursive = TRUE)

  expect_true(is_project_dir(good, root = root))
  # Wrong depth: the purpose dir itself is not a project leaf
  expect_false(is_project_dir(file.path(root, "appr"), root = root))
  # Unrecognized purpose segment
  bad_purpose <- file.path(root, "nope", "us_ca_081_burlin_proj")
  dir.create(bad_purpose, recursive = TRUE)
  expect_false(is_project_dir(bad_purpose, root = root))
  # Non-parseable flat segment
  not_proj <- file.path(root, "appr", "not-a-project")
  dir.create(not_proj, recursive = TRUE)
  expect_false(is_project_dir(not_proj, root = root))
  # Nonexistent path
  expect_false(is_project_dir(file.path(root, "appr", "missing"), root = root))
})

test_that("regproj_reference loads shipped reference data", {
  ref <- regproj_reference()
  expect_type(ref, "list")
  expect_true(all(c("countries", "states", "counties") %in% names(ref)))
  expect_true(length(ref$states) > 0L)
})
