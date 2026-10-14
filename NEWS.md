# cepiweek 0.2.0

## Behaviour changes

* `cepiweek()` no longer stops when a date is missing or cannot be converted.
  Such values are returned as `NA` (`NA_Date_` in the date column and
  `NA_integer_` in `cepiweek`). A warning reports the number of values that could
  not be converted.
* If no valid date is found, the function returns `cepiweek = NA` with a warning
  instead of an error.

## New features

* New argument `quiet` to silence the warning about non-convertible dates.
* The date column can be of class `Date`, `POSIXct` or `character`.
* Date-times (for example `"2024-01-15 10:30:00"`) are accepted; the time part is
  ignored.
* Empty strings, `"NA"` and `"N/A"` are treated as missing dates.
* The `start` argument can be given as a character string, read with the same
  `format` as the date column; an invalid value gives an explicit error.
* Arguments `data` and `col_date` are now checked.

## Compatibility

* Arguments `data`, `col_date`, `start` and `format` and their default values are
  unchanged: results on valid dates are identical to version 0.1.2.

# cepiweek 0.1.2

* Previous version published on CRAN
