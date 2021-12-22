#' Price Inflation Data
#'
#' This data set contains monthly observations of annualized price inflation from January 1949 until November 2021.
#' Price inflation is calculated by taking the log difference between the CPI for Urban Consumers in one period and
#' its value exactly one year earlier.
#'
#' @format Data frame with 875 rows and 7 variables:
#' \describe{
#'   \item{date}{date in "YYYY-MM-DD" format}
#'   \item{pi}{price inflation in decimal format}
#'   \item{pi.1}{price inflation last month}
#'   \item{pi.2}{price inflation two months ago}
#'   \item{pi.3}{price inflation three months ago}
#'   \item{pi.6}{price inflation six months ago}
#'   \item{pi.12}{price inflation one year ago}
#' }
#'
#' @source \url{https://fred.stlouisfed.org/series/CPIAUCSL}
"cpi_data"

