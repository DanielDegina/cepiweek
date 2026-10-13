#' Generate Continuous Epidemiological Week Index for a Date Column
#'
#' This function adds a continuous epidemiological week index to a data frame
#' by assigning each date to a sequential week number starting from a reference
#' date.
#'
#' The generated weeks are continuous and **do not reset at calendar year boundaries**.
#' They are **not ISO 8601 or MMWR weeks**. This function is intended for
#' epidemiological modeling, time-series analysis, and nowcasting applications.
#'
#' You can specify a `start` date for counting weeks; if none is provided, the minimum
#' date in the column is used. The function also allows specifying the `format` of the
#' dates to handle different conventions (day-month-year vs month-day-year).
#'
#' @param data A data frame containing the date column.
#' @param col_date Name of the date column (string). The column can be of class
#'   `Date`, `POSIXct` or `character`.
#' @param start Optional start date for counting weeks (Date or string, read with the
#'   same `format` as the column). Defaults to the minimum date in the column `col_date`.
#' @param format Optional date format flag. Use `"mdy"` if all dates are in
#'   month-day-year format (common in US). Defaults to `"dmy"`/`"ymd"` which handles
#'   day-month-year or year-month-day formats.
#' @param quiet Logical. If `FALSE` (default), a warning reports the number of values
#'   that could not be converted to a date. Set to `TRUE` to silence it.
#'
#' @return A data frame with an added `cepiweek` column containing continuous week
#'   indices. The date column is returned as class `Date`. Missing or non-convertible
#'   dates are returned as `NA` (`NA_Date_` in the date column and `NA_integer_`
#'   in `cepiweek`).
#'
#' @examples
#' # Standard dmy/ymd dates
#' k = data.frame(
#'   num = c(1, 2, 3),
#'   date = c("15-01-2024", "12/02/2025", "2026-08-01")
#' )
#' cepiweek(k, col_date = "date")
#'
#' # American format mm-dd-yyyy
#' k2 = data.frame(
#'   num = c(1, 2, 3),
#'   date = c("01/15/2024", "02-12-2025", "08/01/2026")
#' )
#' cepiweek(k2, col_date = "date", format = "mdy")
#'
#' # Missing and non-convertible dates give NA instead of an error
#' k3 = data.frame(
#'   num = 1:4,
#'   date = c("15-01-2024", NA, "not a date", "2024-02-05")
#' )
#' cepiweek(k3, col_date = "date", quiet = TRUE)
#'
#' @details
#' - The function automatically replaces `/` with `-` for consistency.
#' - Missing values, empty strings, `"NA"` and `"N/A"` are treated as missing dates.
#' - Dates that cannot be converted do not stop the function: they are set to `NA`
#'   and reported in a warning (unless `quiet = TRUE`).
#' - Date-times (for example `"2024-01-15 10:30:00"`) are accepted; the time is ignored.
#' - A warning is issued if the `start` date is after the earliest date in the column.
#' - Continuous weeks are aligned to Mondays.
#'
#' @seealso \href{https://github.com/DanielDegina/cepiweek}{GitHub page of the package}.
#'
#' @export
cepiweek = function(data, col_date, start = NULL, format = "dmy", quiet = FALSE) {
  # Verifications des arguments
  if (!is.data.frame(data)) {
    stop("'data' doit etre un data.frame")
  }
  if (!is.character(col_date) || length(col_date) != 1L || !col_date %in% names(data)) {
    stop("'col_date' doit etre le nom d'une colonne de 'data'")
  }

  # Conversion de la colonne en Date (NA si non convertible)
  dates = .cepiweek_to_date(data[[col_date]], format)

  # Dates non converties (hors valeurs deja manquantes)
  n_echec = .cepiweek_n_echec(data[[col_date]], dates)
  if (n_echec > 0 && !quiet) {
    warning(n_echec, " date(s) non convertible(s) remplacee(s) par NA")
  }

  data[[col_date]] = dates

  # Aucune date valide : on renvoie NA partout sans s'arreter
  if (all(is.na(dates))) {
    if (!quiet) warning("Aucune date valide : 'cepiweek' vaut NA")
    data$cepiweek = rep(NA_integer_, nrow(data))
    return(data)
  }

  # Date minimale observee et date de depart
  date_min_obs = min(dates, na.rm = TRUE)

  if (is.null(start)) {
    date_min = date_min_obs
  } else {
    date_min = .cepiweek_to_date(start, format)[1]
    if (is.na(date_min)) {
      stop("'start' n'est pas une date valide")
    }
    # Verification coherence du start
    if (date_min > date_min_obs) {
      warning("'start' est superieure a la date minimale")
    }
  }

  # Lundi de la semaine contenant la date de depart
  date_min_d1w = lubridate::floor_date(date_min, unit = "week", week_start = 1)

  # Indice de semaine continu (NA si la date est NA)
  data$cepiweek = as.integer(
    floor(as.numeric(
      difftime(dates, date_min_d1w, units = "days")) / 7) + 1
  )

  return(data)
}

# Conversion d'un vecteur (Date, POSIXct, character, factor) en Date.
# Les valeurs manquantes ou non convertibles donnent NA.
.cepiweek_to_date = function(x, format = "dmy") {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }

  # Nettoyage des separateurs et des valeurs vides
  txt = trimws(gsub("/", "-", as.character(x)))
  txt[toupper(txt) %in% c("", "NA", "N/A")] = NA_character_

  # Suppression de l'heure eventuelle (espace ou T suivi de hh:mm...) : seule
  # la date compte, et l'heure rend ambigus les formats dmy et ymd
  txt = trimws(sub("[ T]+[0-9]{1,2}:[0-9]{2}.*$", "", txt))

  orders = if (identical(format, "mdy")) {
    "mdy"
  } else {
    # Par defaut: dmy ou ymd
    c("dmy", "ymd")
  }

  as.Date(lubridate::parse_date_time(txt, orders = orders, quiet = TRUE))
}

# Nombre de valeurs presentes a l'origine mais non converties en Date.
.cepiweek_n_echec = function(x, dates) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    return(0L)
  }
  txt = trimws(as.character(x))
  present = !is.na(txt) & !(toupper(txt) %in% c("", "NA", "N/A"))
  sum(present & is.na(dates))
}

