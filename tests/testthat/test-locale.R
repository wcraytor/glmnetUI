# --- Locale country presets and formatting ---

# ===================================================================
# locale_country_presets_
# ===================================================================

test_that("locale_country_presets_ returns a named list", {
  presets <- glmnetUI:::locale_country_presets_()
  expect_true(is.list(presets))
  expect_true(length(presets) > 0)
  expect_true(all(nzchar(names(presets))))
})

test_that("every preset has required fields", {
  presets <- glmnetUI:::locale_country_presets_()
  required <- c("csv_sep", "csv_dec", "big_mark", "dec_mark",
                 "date_fmt", "paper")
  for (code in names(presets)) {
    p <- presets[[code]]
    for (field in required) {
      expect_true(field %in% names(p),
                  info = paste("Country", code, "missing field", field))
    }
  }
})

test_that("csv_sep is comma or semicolon", {
  presets <- glmnetUI:::locale_country_presets_()
  for (code in names(presets)) {
    expect_true(presets[[code]]$csv_sep %in% c(",", ";"),
                info = paste("Country", code))
  }
})

test_that("csv_dec is period or comma", {
  presets <- glmnetUI:::locale_country_presets_()
  for (code in names(presets)) {
    expect_true(presets[[code]]$csv_dec %in% c(".", ","),
                info = paste("Country", code))
  }
})

test_that("csv_sep and csv_dec never collide", {
  presets <- glmnetUI:::locale_country_presets_()
  for (code in names(presets)) {
    p <- presets[[code]]
    expect_false(p$csv_sep == p$csv_dec,
                 info = paste("Country", code,
                              "has csv_sep == csv_dec:", p$csv_sep))
  }
})

test_that("big_mark and dec_mark never collide", {
  presets <- glmnetUI:::locale_country_presets_()
  for (code in names(presets)) {
    p <- presets[[code]]
    expect_false(p$big_mark == p$dec_mark,
                 info = paste("Country", code,
                              "has big_mark == dec_mark:", p$big_mark))
  }
})

test_that("date_fmt is one of mdy, dmy, ymd", {
  presets <- glmnetUI:::locale_country_presets_()
  for (code in names(presets)) {
    expect_true(presets[[code]]$date_fmt %in% c("mdy", "dmy", "ymd"),
                info = paste("Country", code))
  }
})

test_that("paper is letter or a4", {
  presets <- glmnetUI:::locale_country_presets_()
  for (code in names(presets)) {
    expect_true(presets[[code]]$paper %in% c("letter", "a4"),
                info = paste("Country", code))
  }
})

# --- Specific country formatting expectations ---

test_that("US uses comma CSV, period decimal, MDY dates, letter paper", {
  presets <- glmnetUI:::locale_country_presets_()
  us <- presets[["us"]]
  expect_equal(us$csv_sep, ",")
  expect_equal(us$csv_dec, ".")
  expect_equal(us$big_mark, ",")
  expect_equal(us$dec_mark, ".")
  expect_equal(us$date_fmt, "mdy")
  expect_equal(us$paper, "letter")
})

test_that("Germany uses semicolon CSV, comma decimal, period big_mark", {
  presets <- glmnetUI:::locale_country_presets_()
  de <- presets[["de"]]
  expect_equal(de$csv_sep, ";")
  expect_equal(de$csv_dec, ",")
  expect_equal(de$big_mark, ".")
  expect_equal(de$dec_mark, ",")
  expect_equal(de$date_fmt, "dmy")
  expect_equal(de$paper, "a4")
})

test_that("France uses semicolon CSV, comma decimal, space big_mark", {
  presets <- glmnetUI:::locale_country_presets_()
  fr <- presets[["fr"]]
  expect_equal(fr$csv_sep, ";")
  expect_equal(fr$csv_dec, ",")
  expect_equal(fr$big_mark, " ")
  expect_equal(fr$dec_mark, ",")
})

test_that("Switzerland uses apostrophe big_mark", {
  presets <- glmnetUI:::locale_country_presets_()
  ch <- presets[["ch"]]
  expect_equal(ch$big_mark, "'")
  expect_equal(ch$dec_mark, ",")
})

test_that("UK uses comma CSV, period decimal, DMY dates, A4 paper", {
  presets <- glmnetUI:::locale_country_presets_()
  gb <- presets[["gb"]]
  expect_equal(gb$csv_sep, ",")
  expect_equal(gb$csv_dec, ".")
  expect_equal(gb$date_fmt, "dmy")
  expect_equal(gb$paper, "a4")
})

test_that("Japan uses comma CSV, period decimal, YMD dates, A4 paper", {
  presets <- glmnetUI:::locale_country_presets_()
  jp <- presets[["jp"]]
  expect_equal(jp$csv_sep, ",")
  expect_equal(jp$csv_dec, ".")
  expect_equal(jp$date_fmt, "ymd")
  expect_equal(jp$paper, "a4")
})

test_that("Canada uses comma CSV, YMD dates, letter paper", {
  presets <- glmnetUI:::locale_country_presets_()
  ca <- presets[["ca"]]
  expect_equal(ca$csv_sep, ",")
  expect_equal(ca$date_fmt, "ymd")
  expect_equal(ca$paper, "letter")
})

test_that("Mexico uses comma CSV, DMY dates, letter paper", {
  presets <- glmnetUI:::locale_country_presets_()
  mx <- presets[["mx"]]
  expect_equal(mx$csv_sep, ",")
  expect_equal(mx$date_fmt, "dmy")
  expect_equal(mx$paper, "letter")
})

test_that("Brazil uses semicolon CSV, comma decimal, DMY dates", {
  presets <- glmnetUI:::locale_country_presets_()
  br <- presets[["br"]]
  expect_equal(br$csv_sep, ";")
  expect_equal(br$csv_dec, ",")
  expect_equal(br$date_fmt, "dmy")
})

test_that("Sweden uses semicolon CSV, YMD dates", {
  presets <- glmnetUI:::locale_country_presets_()
  se <- presets[["se"]]
  expect_equal(se$csv_sep, ";")
  expect_equal(se$date_fmt, "ymd")
})

test_that("South Korea uses comma CSV, YMD dates", {
  presets <- glmnetUI:::locale_country_presets_()
  kr <- presets[["kr"]]
  expect_equal(kr$csv_sep, ",")
  expect_equal(kr$date_fmt, "ymd")
})

# ===================================================================
# locale_country_choices_
# ===================================================================

test_that("locale_country_choices_ returns named character vector", {
  choices <- glmnetUI:::locale_country_choices_()
  expect_true(is.character(choices))
  expect_true(length(choices) > 0)
  # Names are display labels, values are country codes
  expect_true(all(nzchar(names(choices))))
  expect_true(all(nzchar(choices)))
})

test_that("every preset country has a display name", {
  presets <- glmnetUI:::locale_country_presets_()
  choices <- glmnetUI:::locale_country_choices_()
  for (code in names(presets)) {
    expect_true(code %in% choices,
                info = paste("Country code", code, "missing from choices"))
  }
})

test_that("every choice code has a preset", {
  presets <- glmnetUI:::locale_country_presets_()
  choices <- glmnetUI:::locale_country_choices_()
  for (code in choices) {
    expect_true(code %in% names(presets),
                info = paste("Choice code", code, "missing from presets"))
  }
})

# ===================================================================
# set_locale_ / get_locale_
# ===================================================================

test_that("set_locale_ sets US defaults correctly", {
  glmnetUI:::set_locale_("us")
  loc <- glmnetUI:::get_locale_()
  expect_equal(loc$csv_sep, ",")
  expect_equal(loc$csv_dec, ".")
  expect_equal(loc$big_mark, ",")
  expect_equal(loc$dec_mark, ".")
  expect_equal(loc$date_fmt, "mdy")
  expect_equal(loc$paper, "letter")
})

test_that("set_locale_ sets German defaults correctly", {
  glmnetUI:::set_locale_("de")
  loc <- glmnetUI:::get_locale_()
  expect_equal(loc$csv_sep, ";")
  expect_equal(loc$csv_dec, ",")
  expect_equal(loc$big_mark, ".")
  expect_equal(loc$dec_mark, ",")
  expect_equal(loc$date_fmt, "dmy")
  expect_equal(loc$paper, "a4")
})

test_that("set_locale_ sets French defaults correctly", {
  glmnetUI:::set_locale_("fr")
  loc <- glmnetUI:::get_locale_()
  expect_equal(loc$csv_sep, ";")
  expect_equal(loc$big_mark, " ")
  expect_equal(loc$dec_mark, ",")
})

test_that("set_locale_ sets Swiss defaults correctly", {
  glmnetUI:::set_locale_("ch")
  loc <- glmnetUI:::get_locale_()
  expect_equal(loc$big_mark, "'")
})

test_that("set_locale_ allows parameter overrides", {
  glmnetUI:::set_locale_("us", csv_sep = ";", big_mark = " ")
  loc <- glmnetUI:::get_locale_()
  expect_equal(loc$csv_sep, ";")   # overridden
  expect_equal(loc$big_mark, " ")  # overridden
  expect_equal(loc$csv_dec, ".")   # from US preset
  expect_equal(loc$dec_mark, ".")  # from US preset
})

test_that("set_locale_ falls back to US for unknown country", {
  glmnetUI:::set_locale_("xx")
  loc <- glmnetUI:::get_locale_()
  expect_equal(loc$csv_sep, ",")
  expect_equal(loc$csv_dec, ".")
  expect_equal(loc$big_mark, ",")
  expect_equal(loc$dec_mark, ".")
})

test_that("set_locale_ works for all known countries", {
  presets <- glmnetUI:::locale_country_presets_()
  for (code in names(presets)) {
    glmnetUI:::set_locale_(code)
    loc <- glmnetUI:::get_locale_()
    p <- presets[[code]]
    expect_equal(loc$csv_sep, p$csv_sep,
                 info = paste("Country", code, "csv_sep"))
    expect_equal(loc$csv_dec, p$csv_dec,
                 info = paste("Country", code, "csv_dec"))
    expect_equal(loc$big_mark, p$big_mark,
                 info = paste("Country", code, "big_mark"))
    expect_equal(loc$dec_mark, p$dec_mark,
                 info = paste("Country", code, "dec_mark"))
    expect_equal(loc$date_fmt, p$date_fmt,
                 info = paste("Country", code, "date_fmt"))
    expect_equal(loc$paper, p$paper,
                 info = paste("Country", code, "paper"))
  }
  # Restore US default
  glmnetUI:::set_locale_("us")
})

# ===================================================================
# Convenience accessors
# ===================================================================

test_that("locale_csv_sep_ returns current csv_sep", {
  glmnetUI:::set_locale_("de")
  expect_equal(glmnetUI:::locale_csv_sep_(), ";")
  glmnetUI:::set_locale_("us")
  expect_equal(glmnetUI:::locale_csv_sep_(), ",")
})

test_that("locale_csv_dec_ returns current csv_dec", {
  glmnetUI:::set_locale_("de")
  expect_equal(glmnetUI:::locale_csv_dec_(), ",")
  glmnetUI:::set_locale_("us")
  expect_equal(glmnetUI:::locale_csv_dec_(), ".")
})

test_that("locale_big_mark_ returns current big_mark", {
  glmnetUI:::set_locale_("fr")
  expect_equal(glmnetUI:::locale_big_mark_(), " ")
  glmnetUI:::set_locale_("ch")
  expect_equal(glmnetUI:::locale_big_mark_(), "'")
  glmnetUI:::set_locale_("de")
  expect_equal(glmnetUI:::locale_big_mark_(), ".")
  glmnetUI:::set_locale_("us")
  expect_equal(glmnetUI:::locale_big_mark_(), ",")
})

test_that("locale_dec_mark_ returns current dec_mark", {
  glmnetUI:::set_locale_("de")
  expect_equal(glmnetUI:::locale_dec_mark_(), ",")
  glmnetUI:::set_locale_("us")
  expect_equal(glmnetUI:::locale_dec_mark_(), ".")
})

test_that("locale_paper_ returns current paper", {
  glmnetUI:::set_locale_("us")
  expect_equal(glmnetUI:::locale_paper_(), "letter")
  glmnetUI:::set_locale_("de")
  expect_equal(glmnetUI:::locale_paper_(), "a4")
  glmnetUI:::set_locale_("us")
})

# ===================================================================
# locale_date_formats_
# ===================================================================

test_that("locale_date_formats_ returns MDY-first for US", {
  glmnetUI:::set_locale_("us")
  fmts <- glmnetUI:::locale_date_formats_()
  expect_true(is.character(fmts))
  expect_true(length(fmts) > 0)
  # First format should be MDY
  expect_match(fmts[1], "%m")
})

test_that("locale_date_formats_ returns DMY-first for UK", {
  glmnetUI:::set_locale_("gb")
  fmts <- glmnetUI:::locale_date_formats_()
  expect_match(fmts[1], "%d")
  glmnetUI:::set_locale_("us")
})

test_that("locale_date_formats_ returns YMD-first for Japan", {
  glmnetUI:::set_locale_("jp")
  fmts <- glmnetUI:::locale_date_formats_()
  expect_match(fmts[1], "%Y")
  glmnetUI:::set_locale_("us")
})

test_that("locale_date_formats_ returns YMD-first for Canada", {
  glmnetUI:::set_locale_("ca")
  fmts <- glmnetUI:::locale_date_formats_()
  expect_match(fmts[1], "%Y")
  glmnetUI:::set_locale_("us")
})

test_that("locale_date_formats_ returns YMD-first for Sweden", {
  glmnetUI:::set_locale_("se")
  fmts <- glmnetUI:::locale_date_formats_()
  expect_match(fmts[1], "%Y")
  glmnetUI:::set_locale_("us")
})

test_that("locale_date_formats_ returns DMY-first for Germany", {
  glmnetUI:::set_locale_("de")
  fmts <- glmnetUI:::locale_date_formats_()
  expect_match(fmts[1], "%d")
  glmnetUI:::set_locale_("us")
})

test_that("locale_date_formats_ always includes ISO format", {
  for (code in c("us", "de", "gb", "jp", "br")) {
    glmnetUI:::set_locale_(code)
    fmts <- glmnetUI:::locale_date_formats_()
    expect_true("%Y-%m-%d" %in% fmts,
                info = paste("Country", code, "missing ISO format"))
  }
  glmnetUI:::set_locale_("us")
})

test_that("locale_date_formats_ has no duplicates", {
  for (code in c("us", "de", "gb", "jp")) {
    glmnetUI:::set_locale_(code)
    fmts <- glmnetUI:::locale_date_formats_()
    expect_equal(length(fmts), length(unique(fmts)),
                 info = paste("Country", code, "has duplicate formats"))
  }
  glmnetUI:::set_locale_("us")
})

# ===================================================================
# CSV import with locale-specific separators
# ===================================================================

test_that("US locale reads comma-separated CSV correctly", {
  glmnetUI:::set_locale_("us")
  tmp <- tempfile(fileext = ".csv")
  writeLines("a,b,c\n1,2,3\n4,5,6", tmp)
  df <- utils::read.csv(tmp, sep = glmnetUI:::locale_csv_sep_(),
                         dec = glmnetUI:::locale_csv_dec_())
  expect_equal(nrow(df), 2)
  expect_equal(ncol(df), 3)
  expect_equal(df$a[1], 1)
  unlink(tmp)
})

test_that("German locale reads semicolon-separated CSV correctly", {
  glmnetUI:::set_locale_("de")
  tmp <- tempfile(fileext = ".csv")
  writeLines("a;b;c\n1,5;2,3;3,7\n4,1;5,2;6,8", tmp)
  df <- utils::read.csv(tmp, sep = glmnetUI:::locale_csv_sep_(),
                         dec = glmnetUI:::locale_csv_dec_())
  expect_equal(nrow(df), 2)
  expect_equal(ncol(df), 3)
  expect_equal(df$a[1], 1.5)
  expect_equal(df$b[2], 5.2)
  unlink(tmp)
  glmnetUI:::set_locale_("us")
})

test_that("French locale reads semicolon CSV with space big_mark", {
  glmnetUI:::set_locale_("fr")
  tmp <- tempfile(fileext = ".csv")
  writeLines("prix;quantite\n1000,50;100\n2000,75;200", tmp)
  df <- utils::read.csv(tmp, sep = glmnetUI:::locale_csv_sep_(),
                         dec = glmnetUI:::locale_csv_dec_())
  expect_equal(nrow(df), 2)
  expect_equal(df$prix[1], 1000.50)
  unlink(tmp)
  glmnetUI:::set_locale_("us")
})

test_that("Brazilian locale reads semicolon CSV with comma decimal", {
  glmnetUI:::set_locale_("br")
  tmp <- tempfile(fileext = ".csv")
  writeLines("valor;taxa\n1234,56;7,89\n9876,54;3,21", tmp)
  df <- utils::read.csv(tmp, sep = glmnetUI:::locale_csv_sep_(),
                         dec = glmnetUI:::locale_csv_dec_())
  expect_equal(nrow(df), 2)
  expect_equal(df$valor[1], 1234.56)
  expect_equal(df$taxa[2], 3.21)
  unlink(tmp)
  glmnetUI:::set_locale_("us")
})

# ===================================================================
# Number formatting with locale big_mark / dec_mark
# ===================================================================

test_that("formatC uses US big_mark correctly", {
  glmnetUI:::set_locale_("us")
  result <- formatC(1234567.89, format = "f", digits = 2,
                    big.mark = glmnetUI:::locale_big_mark_(),
                    decimal.mark = glmnetUI:::locale_dec_mark_())
  expect_equal(result, "1,234,567.89")
})

test_that("formatC uses German formatting correctly", {
  glmnetUI:::set_locale_("de")
  result <- formatC(1234567.89, format = "f", digits = 2,
                    big.mark = glmnetUI:::locale_big_mark_(),
                    decimal.mark = glmnetUI:::locale_dec_mark_())
  expect_equal(result, "1.234.567,89")
  glmnetUI:::set_locale_("us")
})

test_that("formatC uses French formatting correctly", {
  glmnetUI:::set_locale_("fr")
  result <- formatC(1234567.89, format = "f", digits = 2,
                    big.mark = glmnetUI:::locale_big_mark_(),
                    decimal.mark = glmnetUI:::locale_dec_mark_())
  expect_equal(result, "1 234 567,89")
  glmnetUI:::set_locale_("us")
})

test_that("formatC uses Swiss formatting correctly", {
  glmnetUI:::set_locale_("ch")
  result <- formatC(1234567.89, format = "f", digits = 2,
                    big.mark = glmnetUI:::locale_big_mark_(),
                    decimal.mark = glmnetUI:::locale_dec_mark_())
  expect_equal(result, "1'234'567,89")
  glmnetUI:::set_locale_("us")
})

# ===================================================================
# Grouping: comma-decimal countries vs period-decimal countries
# ===================================================================

test_that("comma-decimal countries all use semicolon CSV separator", {
  presets <- glmnetUI:::locale_country_presets_()
  comma_dec <- names(presets)[vapply(presets, function(p) p$csv_dec == ",",
                                     logical(1))]
  for (code in comma_dec) {
    expect_equal(presets[[code]]$csv_sep, ";",
                 info = paste("Country", code,
                              "uses comma decimal but not semicolon CSV sep"))
  }
})

test_that("period-decimal countries all use comma CSV separator", {
  presets <- glmnetUI:::locale_country_presets_()
  period_dec <- names(presets)[vapply(presets, function(p) p$csv_dec == ".",
                                      logical(1))]
  for (code in period_dec) {
    expect_equal(presets[[code]]$csv_sep, ",",
                 info = paste("Country", code,
                              "uses period decimal but not comma CSV sep"))
  }
})

# ===================================================================
# Paper size grouping
# ===================================================================

test_that("only US, Canada, Mexico use letter paper", {
  presets <- glmnetUI:::locale_country_presets_()
  letter_countries <- names(presets)[vapply(presets,
    function(p) p$paper == "letter", logical(1))]
  expect_true(all(letter_countries %in% c("us", "ca", "mx")))
})

test_that("all European and Asian countries use A4", {
  presets <- glmnetUI:::locale_country_presets_()
  a4_countries <- names(presets)[vapply(presets,
    function(p) p$paper == "a4", logical(1))]
  # At minimum these should be A4
  expect_true("gb" %in% a4_countries)
  expect_true("de" %in% a4_countries)
  expect_true("fr" %in% a4_countries)
  expect_true("jp" %in% a4_countries)
  expect_true("au" %in% a4_countries)
})

# ===================================================================
# Additional country-specific convention tests (matching earthUI)
# ===================================================================

test_that("Finland uses space thousands, semicolon CSV, comma decimal", {
  glmnetUI:::set_locale_("fi")
  expect_equal(glmnetUI:::locale_big_mark_(), " ")
  expect_equal(glmnetUI:::locale_csv_sep_(), ";")
  expect_equal(glmnetUI:::locale_csv_dec_(), ",")
  glmnetUI:::set_locale_("us")
})

test_that("Lithuania uses ISO date format (ymd)", {
  glmnetUI:::set_locale_("lt")
  fmts <- glmnetUI:::locale_date_formats_()
  expect_equal(fmts[1], "%Y-%m-%d")
  glmnetUI:::set_locale_("us")
})

test_that("Ukraine uses space thousands, comma decimal, dmy dates", {
  glmnetUI:::set_locale_("ua")
  expect_equal(glmnetUI:::locale_big_mark_(), " ")
  expect_equal(glmnetUI:::locale_csv_dec_(), ",")
  fmts <- glmnetUI:::locale_date_formats_()
  expect_equal(fmts[1], "%d/%m/%Y")
  glmnetUI:::set_locale_("us")
})

test_that("Russia uses space thousands, semicolon CSV", {
  glmnetUI:::set_locale_("ru")
  expect_equal(glmnetUI:::locale_big_mark_(), " ")
  expect_equal(glmnetUI:::locale_csv_sep_(), ";")
  glmnetUI:::set_locale_("us")
})

test_that("Turkey uses dot thousands, comma decimal", {
  glmnetUI:::set_locale_("tr")
  expect_equal(glmnetUI:::locale_big_mark_(), ".")
  expect_equal(glmnetUI:::locale_csv_dec_(), ",")
  glmnetUI:::set_locale_("us")
})

test_that("Estonia uses space thousands, semicolon CSV, dmy dates", {
  glmnetUI:::set_locale_("ee")
  expect_equal(glmnetUI:::locale_big_mark_(), " ")
  expect_equal(glmnetUI:::locale_csv_sep_(), ";")
  fmts <- glmnetUI:::locale_date_formats_()
  expect_equal(fmts[1], "%d/%m/%Y")
  glmnetUI:::set_locale_("us")
})

test_that("Latvia uses space thousands, dmy dates", {
  glmnetUI:::set_locale_("lv")
  expect_equal(glmnetUI:::locale_big_mark_(), " ")
  fmts <- glmnetUI:::locale_date_formats_()
  expect_equal(fmts[1], "%d/%m/%Y")
  glmnetUI:::set_locale_("us")
})

test_that("Poland uses space thousands, semicolon CSV", {
  glmnetUI:::set_locale_("pl")
  expect_equal(glmnetUI:::locale_big_mark_(), " ")
  expect_equal(glmnetUI:::locale_csv_sep_(), ";")
  glmnetUI:::set_locale_("us")
})

test_that("Czech Republic uses space thousands, semicolon CSV", {
  glmnetUI:::set_locale_("cz")
  expect_equal(glmnetUI:::locale_big_mark_(), " ")
  expect_equal(glmnetUI:::locale_csv_sep_(), ";")
  glmnetUI:::set_locale_("us")
})

test_that("Denmark uses dot thousands, semicolon CSV", {
  glmnetUI:::set_locale_("dk")
  expect_equal(glmnetUI:::locale_big_mark_(), ".")
  expect_equal(glmnetUI:::locale_csv_sep_(), ";")
  glmnetUI:::set_locale_("us")
})

test_that("Norway uses space thousands, semicolon CSV, dmy dates", {
  glmnetUI:::set_locale_("no")
  expect_equal(glmnetUI:::locale_big_mark_(), " ")
  expect_equal(glmnetUI:::locale_csv_sep_(), ";")
  expect_equal(glmnetUI:::locale_dec_mark_(), ",")
  glmnetUI:::set_locale_("us")
})

test_that("Italy uses dot thousands, semicolon CSV, comma decimal", {
  glmnetUI:::set_locale_("it")
  expect_equal(glmnetUI:::locale_big_mark_(), ".")
  expect_equal(glmnetUI:::locale_csv_sep_(), ";")
  expect_equal(glmnetUI:::locale_csv_dec_(), ",")
  glmnetUI:::set_locale_("us")
})

test_that("Spain uses dot thousands, semicolon CSV, comma decimal", {
  glmnetUI:::set_locale_("es")
  expect_equal(glmnetUI:::locale_big_mark_(), ".")
  expect_equal(glmnetUI:::locale_csv_sep_(), ";")
  expect_equal(glmnetUI:::locale_csv_dec_(), ",")
  glmnetUI:::set_locale_("us")
})

test_that("Portugal uses dot thousands, semicolon CSV", {
  glmnetUI:::set_locale_("pt")
  expect_equal(glmnetUI:::locale_big_mark_(), ".")
  expect_equal(glmnetUI:::locale_csv_sep_(), ";")
  glmnetUI:::set_locale_("us")
})

test_that("Belgium uses dot thousands, semicolon CSV", {
  glmnetUI:::set_locale_("be")
  expect_equal(glmnetUI:::locale_big_mark_(), ".")
  expect_equal(glmnetUI:::locale_csv_sep_(), ";")
  glmnetUI:::set_locale_("us")
})

test_that("Netherlands uses dot thousands, semicolon CSV", {
  glmnetUI:::set_locale_("nl")
  expect_equal(glmnetUI:::locale_big_mark_(), ".")
  expect_equal(glmnetUI:::locale_csv_sep_(), ";")
  glmnetUI:::set_locale_("us")
})

test_that("Austria uses dot thousands, semicolon CSV", {
  glmnetUI:::set_locale_("at")
  expect_equal(glmnetUI:::locale_big_mark_(), ".")
  expect_equal(glmnetUI:::locale_csv_sep_(), ";")
  glmnetUI:::set_locale_("us")
})

test_that("Australia uses comma thousands, comma CSV, dmy dates", {
  glmnetUI:::set_locale_("au")
  expect_equal(glmnetUI:::locale_big_mark_(), ",")
  expect_equal(glmnetUI:::locale_csv_sep_(), ",")
  expect_equal(glmnetUI:::locale_paper_(), "a4")
  fmts <- glmnetUI:::locale_date_formats_()
  expect_equal(fmts[1], "%d/%m/%Y")
  glmnetUI:::set_locale_("us")
})

test_that("New Zealand uses comma thousands, dmy dates, A4 paper", {
  glmnetUI:::set_locale_("nz")
  expect_equal(glmnetUI:::locale_big_mark_(), ",")
  expect_equal(glmnetUI:::locale_paper_(), "a4")
  fmts <- glmnetUI:::locale_date_formats_()
  expect_equal(fmts[1], "%d/%m/%Y")
  glmnetUI:::set_locale_("us")
})

test_that("Ireland uses comma thousands, dmy dates, A4 paper", {
  glmnetUI:::set_locale_("ie")
  expect_equal(glmnetUI:::locale_big_mark_(), ",")
  expect_equal(glmnetUI:::locale_csv_sep_(), ",")
  expect_equal(glmnetUI:::locale_paper_(), "a4")
  glmnetUI:::set_locale_("us")
})

# ===================================================================
# Finnish number formatting
# ===================================================================

test_that("formatC uses Finnish formatting correctly", {
  glmnetUI:::set_locale_("fi")
  result <- formatC(1234567.89, format = "f", digits = 2,
                    big.mark = glmnetUI:::locale_big_mark_(),
                    decimal.mark = glmnetUI:::locale_dec_mark_())
  expect_equal(result, "1 234 567,89")
  glmnetUI:::set_locale_("us")
})

# ===================================================================
# Slope label formatting with locale (contrib_slope_label_)
# ===================================================================

test_that("contrib_auto_digits_ returns 0 for large ranges", {
  expect_equal(glmnetUI:::contrib_auto_digits_(c(100, 300)), 0L)
  expect_equal(glmnetUI:::contrib_auto_digits_(c(1000, 5000)), 0L)
})

test_that("contrib_auto_digits_ returns 1 for medium ranges", {
  expect_equal(glmnetUI:::contrib_auto_digits_(c(10, 50)), 1L)
})

test_that("contrib_auto_digits_ returns 2 for small ranges", {
  expect_equal(glmnetUI:::contrib_auto_digits_(c(1, 5)), 2L)
})

test_that("contrib_auto_digits_ returns 3 for tiny ranges", {
  expect_equal(glmnetUI:::contrib_auto_digits_(c(0.1, 0.5)), 3L)
})

test_that("contrib_auto_digits_ returns 0 for insufficient data", {
  expect_equal(glmnetUI:::contrib_auto_digits_(c(5)), 0L)
  expect_equal(glmnetUI:::contrib_auto_digits_(numeric(0)), 0L)
})

test_that("contrib_auto_digits_ handles non-finite values", {
  # Inf/-Inf/NA/NaN are filtered out; c(100, 200) has range 100 -> 0L
  expect_equal(glmnetUI:::contrib_auto_digits_(c(Inf, -Inf, 100, 200)), 0L)
  expect_equal(glmnetUI:::contrib_auto_digits_(c(NA, NaN, 100, 200)), 0L)
  # With smaller remaining range
  expect_equal(glmnetUI:::contrib_auto_digits_(c(Inf, NA, 10, 50)), 1L)
})

test_that("slope label uses US formatting", {
  glmnetUI:::set_locale_("us")
  result <- glmnetUI:::contrib_slope_label_(1234, c(200000, 500000))
  # Large range -> digits = 0 -> /unit
  expect_match(result, "\\+1,234\\.00/unit")
})

test_that("slope label uses German formatting", {
  glmnetUI:::set_locale_("de")
  result <- glmnetUI:::contrib_slope_label_(1234, c(200000, 500000))
  # German: dot thousands, comma decimal
  expect_match(result, "\\+1\\.234,00/unit")
  glmnetUI:::set_locale_("us")
})

test_that("slope label uses French formatting (space thousands)", {
  glmnetUI:::set_locale_("fr")
  result <- glmnetUI:::contrib_slope_label_(1234, c(200000, 500000))
  expect_match(result, "\\+1 234,00/unit")
  glmnetUI:::set_locale_("us")
})

test_that("slope label uses Swiss formatting (apostrophe thousands)", {
  glmnetUI:::set_locale_("ch")
  result <- glmnetUI:::contrib_slope_label_(1234, c(200000, 500000))
  expect_match(result, "\\+1'234,00/unit")
  glmnetUI:::set_locale_("us")
})

test_that("slope label shows negative sign for negative slopes", {
  glmnetUI:::set_locale_("us")
  result <- glmnetUI:::contrib_slope_label_(-500, c(100, 300))
  expect_match(result, "^-")
})

test_that("slope label shows positive sign for positive slopes", {
  glmnetUI:::set_locale_("us")
  result <- glmnetUI:::contrib_slope_label_(500, c(100, 300))
  expect_match(result, "^\\+")
})

test_that("slope label uses /unit for large x-axis range", {
  glmnetUI:::set_locale_("us")
  result <- glmnetUI:::contrib_slope_label_(100, c(1000, 5000))
  expect_match(result, "/unit$")
})

test_that("slope label uses scaled units for small x-axis range", {
  glmnetUI:::set_locale_("us")
  # Range 1-5 -> digits=2 -> unit_size=0.01
  result <- glmnetUI:::contrib_slope_label_(100, c(1, 5))
  expect_match(result, "/0\\.01$")
})

test_that("slope label uses scaled units for tiny x-axis range", {
  glmnetUI:::set_locale_("us")
  # Range 0.1-0.5 -> digits=3 -> unit_size=0.001
  result <- glmnetUI:::contrib_slope_label_(100, c(0.1, 0.5))
  expect_match(result, "/0\\.001$")
})

test_that("slope label with German locale and small range", {
  glmnetUI:::set_locale_("de")
  # Range 1-5 -> digits=2 -> unit_size=0.01
  result <- glmnetUI:::contrib_slope_label_(100, c(1, 5))
  # German: comma decimal in the slope value
  expect_match(result, ",")  # decimal comma
  expect_match(result, "/0\\.01$")  # unit label is always period-formatted
  glmnetUI:::set_locale_("us")
})

# Restore US locale as default at end of test file
glmnetUI:::set_locale_("us")
