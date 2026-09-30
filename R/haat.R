#' Calculate Heat Area Above Threshold (HAAT)
#'
#' Computes the daily cumulative excess exposure above a threshold from
#' an hourly time series.
#'
#' @param datetime POSIXct vector, corresponding to the exposure measurement timestamps.
#' @param exposure Exposure measurements.
#' @param threshold Numeric threshold to be considered, in the same unit as the exposure.
#'  Could be either one of the following:
#'  - A constant threshold for the whole period.
#'  - An hourly threshold, for each `datetime`.
#'  - A daily threshold.
#' @param steps interpolation data points for each day, default to 100
#'
#' @return Cumulative daily exposure above the referred threshold (numeric).
#'
#' @examples
#' data(example_hourly)
#'
#' haat(
#'   datetime = example_hourly$datetime,
#'   exposure = example_hourly$temp,
#'   threshold = 32
#' )
#'
#' @export
haat <- function(datetime, exposure, threshold, steps=100, minimal_measurements = 6, type="linear") {

  # Checks

  if (!inherits(datetime, "POSIXct")) {
    stop("`datetime` must be a POSIXct/datetime vector.", call. = FALSE)
  }

  if (!is.numeric(exposure)) {
    stop("`exposure` must be numeric.", call. = FALSE)
  }

  if (!is.numeric(threshold)) {
    stop("HAAT `threshold` must be a numeric value.", call. = FALSE)
  }

  if (length(datetime) != length(exposure)) {
    stop("`datetime` and `exposure` must have the same length.", call. = FALSE)
  }

  if (is.unsorted(datetime)) {
    stop("`datetime` must be sorted in increasing order.", call. = FALSE)
  }

  dates <- as.Date(format(datetime, "%Y-%m-%d"))

  if (length(threshold) != 1 & length(threshold) != length(dates) & length(threshold) != length(datetime)) {
    stop("`threshold` should be of length 1 or the same length as the following: `datetime` or `dates`)", call. = FALSE)
  }

  if (minimal_measurements < 3) {
    minimal_measurements <- 3
    warning("`minimal_measurements` set to 3. dates with fewer than 3 measurements cannot be considered.")
  }

  # for the whole period
  dates <- factor(
    dates,
    levels = seq(min(dates), max(dates), by=1)
  )

  unique_dates <- levels(dates)
  first_date <- as.Date(unique_dates[1])
  last_date <- as.Date(unique_dates[length(unique_dates)])

  dates_count_non_na <- table(dates[!is.na(exposure)])

  xout <- as.numeric(seq(
    lubridate::ymd_h(paste0(first_date, "-0"), tz=lubridate::tz(datetime)),
    lubridate::ymd_h(paste0(last_date + 1, " -0"), tz=lubridate::tz(datetime)) - lubridate::hours(1),
    length.out = steps*as.numeric(last_date - first_date + 1)
  ))

  x <- as.numeric(datetime)

  interp <- approx(x, exposure, xout=xout)

  datetime_out <- as.POSIXct(interp$x, origin="1970-01-01", tz=lubridate::tz(datetime))
  date_out <- as.Date(format(datetime_out, "%Y-%m-%d"))
  x_min <- as.numeric(lubridate::floor_date(lubridate::ymd_h(paste0(date_out, " 12"), tz=lubridate::tz(datetime)), unit = "day"))
  hora_out <- (interp$x - x_min )/3600

  if (length(threshold) == length(datetime)) {
    threshold <- threshold[findInterval(xout, x)]
  } else if (length(threshold) == length(dates)) {
    threshold <- threshold[findInterval(xout,
                                        as.numeric(lubridate::floor_date(lubridate::ymd_h(paste0(dates, " 12"), tz=lubridate::tz(datetime)), unit = "day"))
                                        )]
  }

  y_above <- pmax(0, interp$y - threshold)

  r <- rle(as.numeric(date_out))

  starts <- cumsum(c(1L, head(r$lengths, -1L)))
  ends   <- cumsum(r$lengths)

  auc_results <- mapply(
    function(s, e) MESS::auc(hora_out[s:e], y_above[s:e], type = type),
    starts,
    ends
  )

  if (sum(dates_count_non_na > minimal_measurements) > 0) {
    warning(
        sprintf("%d dates with less valid measurements than minimal (%d) being set to NA.\nYou can switch the `minimal_measurements` argument.",
                sum(dates_count_non_na > minimal_measurements),
                minimal_measurements
        )
    )
  }

  auc_results <- unname(ifelse(dates_count_non_na > minimal_measurements, auc_results, NA))

  data.frame(
    date=unique(date_out),
    auc=auc_results
  )
}
