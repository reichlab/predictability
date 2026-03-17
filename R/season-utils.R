#' Assign season labels and within-season week indices
#'
#' Adds `season` and `season_week` columns to a data.frame that contains
#' `epiweek` and `epiyear` columns. Seasons are defined by a configurable
#' start week (default 40, for typical respiratory disease seasons).
#'
#' @param data data.frame with columns `epiweek` (integer 1-53) and `epiyear`
#'   (integer)
#' @param start_week integer, the epiweek that begins each season (default 40)
#'
#' @returns The input data.frame with two new columns:
#'   \describe{
#'     \item{season}{character label, e.g. `"2010/2011"` when `start_week > 1`,
#'       or `"2010"` when `start_week == 1`}
#'     \item{season_week}{integer, sequential week index within season
#'       starting at 1}
#'   }
#'
#' @export
assign_seasons <- function(data, start_week = 40L) {
  ## --- input validation ---
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame")
  }
  if (!("epiweek" %in% names(data))) {
    stop("`data` must contain an `epiweek` column")
  }
  if (!("epiyear" %in% names(data))) {
    stop("`data` must contain an `epiyear` column")
  }
  start_week <- as.integer(start_week)
  if (length(start_week) != 1L || is.na(start_week) ||
      start_week < 1L || start_week > 53L) {
    stop("`start_week` must be an integer between 1 and 53")
  }

  ## --- assign season labels ---
  if (start_week == 1L) {
    data$season <- as.character(data$epiyear)
    data$season_week <- data$epiweek
    return(data)
  }

  ## For start_week > 1: weeks >= start_week belong to season starting in

  ## epiyear; weeks < start_week belong to season starting in epiyear - 1
  season_start_year <- ifelse(data$epiweek >= start_week,
                              data$epiyear,
                              data$epiyear - 1L)
  data$season <- paste0(season_start_year, "/", season_start_year + 1L)

  ## --- assign season_week as sequential index within each season ---
  ## Order by (epiyear, epiweek), then number sequentially within season
  ord <- order(data$epiyear, data$epiweek)
  data$season_week <- NA_integer_
  data$season_week[ord] <- unlist(
    tapply(seq_along(ord), data$season[ord], function(idx) seq_along(idx)),
    use.names = FALSE
  )

  data
}
