# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(cepiweek)

test_that("les exemples historiques (dmy/ymd) donnent les memes indices", {
  k = data.frame(
    num = c(1, 2, 3),
    date = c("15-01-2024", "12/02/2025", "2026-08-01")
  )
  res = cepiweek(k, col_date = "date")

  attendu = as.integer(floor(as.numeric(
    as.Date(c("2024-01-15", "2025-02-12", "2026-08-01")) - as.Date("2024-01-15")
  ) / 7) + 1)

  expect_equal(res$cepiweek, attendu)
  expect_s3_class(res$date, "Date")
})

test_that("les semaines sont alignees sur les lundis", {
  k = data.frame(date = c("2024-01-15", "2024-01-21", "2024-01-22"))
  res = cepiweek(k, col_date = "date")
  expect_equal(res$cepiweek, c(1L, 1L, 2L))
})

test_that("le format mdy est pris en compte", {
  k2 = data.frame(date = c("01/15/2024", "02-12-2025", "08/01/2026"))
  res = cepiweek(k2, col_date = "date", format = "mdy")
  expect_equal(res$date, as.Date(c("2024-01-15", "2025-02-12", "2026-08-01")))
  expect_equal(res$cepiweek[1], 1L)
})

test_that("les NA et valeurs vides donnent NA sans erreur ni avertissement", {
  k = data.frame(date = c("2024-01-15", NA, "", "NA", "2024-01-22"))
  expect_no_warning(res <- cepiweek(k, col_date = "date"))
  expect_equal(res$cepiweek, c(1L, NA, NA, NA, 2L))
  expect_true(all(is.na(res$date[c(2, 3, 4)])))
})

test_that("une date non convertible donne NA avec un avertissement", {
  k = data.frame(date = c("2024-01-15", "pas une date", "2024-01-22"))
  expect_warning(res <- cepiweek(k, col_date = "date"), "non convertible")
  expect_equal(res$cepiweek, c(1L, NA, 2L))
  expect_no_warning(cepiweek(k, col_date = "date", quiet = TRUE))
})

test_that("une colonne Date ou POSIXct est acceptee", {
  d = as.Date(c("2024-01-15", NA, "2024-01-29"))
  res_date = cepiweek(data.frame(date = d), col_date = "date")
  expect_equal(res_date$cepiweek, c(1L, NA, 3L))

  p = as.POSIXct(c("2024-01-15 10:30:00", "2024-01-29 23:59:00"), tz = "UTC")
  res_posix = cepiweek(data.frame(date = p), col_date = "date")
  expect_equal(res_posix$cepiweek, c(1L, 3L))
})

test_that("les dates avec heure sont acceptees", {
  k = data.frame(date = c("2024-01-15 10:30:00", "22-01-2024 08:00"))
  res = cepiweek(k, col_date = "date")
  expect_equal(res$cepiweek, c(1L, 2L))
})

test_that("aucune date valide donne NA partout sans erreur", {
  k = data.frame(date = c(NA_character_, NA_character_))
  expect_warning(res <- cepiweek(k, col_date = "date"), "Aucune date valide")
  expect_equal(res$cepiweek, c(NA_integer_, NA_integer_))

  k_abc = data.frame(date = c("abc", "def"))
  res_abc = cepiweek(k_abc, col_date = "date", quiet = TRUE)
  expect_equal(res_abc$cepiweek, c(NA_integer_, NA_integer_))
})

test_that("start est utilise, y compris sous forme de texte", {
  k = data.frame(date = c("2024-01-22", "2024-01-29"))
  res = cepiweek(k, col_date = "date", start = "15-01-2024")
  expect_equal(res$cepiweek, c(2L, 3L))

  expect_warning(
    cepiweek(k, col_date = "date", start = as.Date("2024-02-05")),
    "superieure"
  )
  expect_error(cepiweek(k, col_date = "date", start = "xyz"), "start")
})

test_that("les arguments invalides produisent une erreur claire", {
  expect_error(cepiweek(1:3, col_date = "date"), "data.frame")
  expect_error(cepiweek(data.frame(a = 1), col_date = "date"), "col_date")
})
