#' Volt & Temperature Data
#'
#' An example dataset containing voltage and temperature data recorded
#' by 3 telemetry receivers, 2018/2019 season
#'
#' @format A data frame with 43,656 rows and 11 variables:
#' \describe{
#'   \item{file_name}{the full file path}
#'   \item{file}{the file name}
#'   \item{date}{date in "%d%/%m%/%y%" format}
#'   \item{time}{time in "%H:%M" format, PST}
#'   \item{volt_avg}{average volts during time interval}
#'   \item{volt_min}{minimum volts during time interval}
#'   \item{volt_max}{maximum volts during time interval}
#'   \item{temp_avg}{average temperature during time interval, degrees Fahrenheit}
#'   \item{temp_min}{minimum temperature during time interval, degrees Fahrenheit}
#'   \item{temp_max}{maximum temperature during time interval, degrees Fahrenheit}
#'   \item{receiver}{receiver code}
#' }
#' @source Biomark NAS - data/telemetry/lemhi/fixed_site_downloads/2018_2019/
"volt_temp_df"
