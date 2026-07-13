#' Calculate Heat Area Above Threshold (HAAT)
#'
#' Computes the daily cumulative excess exposure above a threshold from
#' an hourly time series.
#'
#' @param datetime POSIXct vector, corresponding to the exposure measurement timestamps.
#' @param exposure Exposure measurements.
#' @param threshold Numeric threshold to be considered, in the same unit as the exposure.
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
haat <- function(datetime, exposure, threshold, steps=100) {

  # Checks

  if (!inherits(datetime, "POSIXct")) {
    stop("`datetime` must be a POSIXct/datetime vector.", call. = FALSE)
  }

  if (!is.numeric(exposure)) {
    stop("`exposure` must be numeric.", call. = FALSE)
  }

  if (!is.numeric(threshold) || length(threshold) != 1L) {
    stop("HAAT `threshold` must be a single numeric value.", call. = FALSE)
  }

  if (length(datetime) != length(exposure)) {
    stop("`datetime` and `exposure` must have the same length.", call. = FALSE)
  }

  if (is.unsorted(datetime)) {
    stop("`datetime` must be sorted in increasing order.", call. = FALSE)
  }

  dates <- as.Date(format(datetime, "%Y-%m-%d"))

  if(any(as.numeric(dates - lag(dates)) >= 2, na.rm=T)) {
    stop("`datetime` is an interrupted time series (one or more days are missing)", call. = FALSE)
  }

  dates_count <- table(dates)

  n_3 <- sum(dates_count < 3)
  n_6 <- sum(dates_count < 6)
  n_12 <- sum(dates_count < 12)

  if (n_3 > 0) {
    warning(paste0(n_3, " dates in the time series with less than three measurements in a day."))
  }

  if (n_6 > 0) {
    warning(paste0(n_6, " dates in the time series with less than three measurements in a day."))
  }

  if (n_12 > 0) {
    warning(paste0(n_12, " dates in the time series with less than three measurements in a day."))
  }

  xout <- as.numeric(seq(
    lubridate::ymd_h(paste0(min(dates), "-0"), tz=lubridate::tz(datetime)),
    lubridate::ymd_h(paste0(max(dates) + 1, " -0"), tz=lubridate::tz(datetime)) - lubridate::hours(1),
    length.out = steps*as.numeric(max(dates) - min(dates) + 1)
  ))

  x <- as.numeric(datetime)

  interp <- approx(x, exposure, xout=xout)

  datetime_out <- as.POSIXct(interp$x, origin="1970-01-01", tz=lubridate::tz(datetime))
  date_out <- as.Date(format(datetime_out, "%Y-%m-%d"))
  x_min <- as.numeric(lubridate::ymd_h(paste0(date_out, " 0"), tz=lubridate::tz(datetime)))
  hora_out <- (interp$x - x_min )/3600
  y_above <- pmax(0, interp$y - threshold)

  r <- rle(as.numeric(date_out))

  starts <- cumsum(c(1L, head(r$lengths, -1L)))
  ends   <- cumsum(r$lengths)

  auc_results <- mapply(
    function(s, e) flux::auc(hora_out[s:e], y_above[s:e]),
    starts,
    ends
  )

  data.frame(
    date=unique(date_out),
    auc=auc_results
  )
}
