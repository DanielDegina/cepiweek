## Test environments
* Local: Windows 11 x64, R 4.6.0 (ucrt)
* win-builder: R Under development (unstable) (2026-10-10 r90659 ucrt),
  Windows Server 2022 x64

## R CMD check results
0 errors | 0 warnings | 0 notes

## Changes in this version (0.2.0)
This is an update of the existing package (previous CRAN version: 0.1.2).

* cepiweek() no longer stops when a date is missing or cannot be converted:
  such values are returned as NA (NA_Date_ in the date column, NA_integer_ in
  the cepiweek column) and reported in a warning, which can be silenced with the
  new argument `quiet = TRUE`.
* The date column can now be of class Date, POSIXct or character; date-times
  are accepted and the time part is ignored.
* The `start` argument is now read with the same `format` as the date column,
  so it can be given as a character string such as "15-01-2024".
* Existing arguments and their default values are unchanged, so results on
  valid dates are identical to version 0.1.2.
* Documentation, README, DESCRIPTION and unit tests were updated accordingly
  (see NEWS.md).

## Reverse dependencies
There are no reverse dependencies.
