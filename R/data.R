#' Raw .txt Format Telemetry Data
#'
#' An example dataset containing observations for 5 receivers
#' and odd numbered frequencies, 2018/2019 season
#'
#' @format A data frame with 2694507 rows and 9 variables:
#' \describe{
#'   \item{file_name}{the full file path}
#'   \item{file}{the file name}
#'   \item{date}{date in "\%d\%/\%m\%/\%y\%" format}
#'   \item{time}{time in "\%H:\%M" format, PST}
#'   \item{receiver}{receiver code}
#'   \item{valid}{is observation valid, 1 = T, 0 = F}
#'   \item{frequency}{channel of record or observation}
#'   \item{tag_code}{receiver code}
#'   \item{signal_strength}{receiver code}
#' }
#' @source Biomark NAS - data/telemetry/lemhi/fixed_site_downloads/2018_2019/
"raw"

#' Compressed Telemetry Data
#'
#' An example dataset containing compressed observations
#' for 5 receivers and odd numbered frequencies, 2018/2019 season
#'
#' @format A data frame with 260437 rows and 7 variables:
#' \describe{
#'   \item{receiver}{receiver code}
#'   \item{valid}{is observation valid, 1 = T, 0 = F}
#'   \item{tag_id}{tag ID including the frequency and code}
#'   \item{start}{the time of first observation in "\%Y-\%m-\%d \%H:\%M:\%S" format}
#'   \item{end}{the time of last observation in "\%Y-\%m-\%d \%H:\%M:\%S" format}
#'   \item{n}{number of observations}
#'   \item{week}{the week in the study season}
#' }
#' @source Biomark NAS - data/telemetry/lemhi/fixed_site_downloads/2018_2019/
"compressed"

#' Volt & Temperature Data
#'
#' An example dataset containing voltage and temperature data recorded
#' by 3 telemetry receivers, 2018/2019 season
#'
#' @format A data frame with 43656 rows and 11 variables:
#' \describe{
#'   \item{file_name}{the full file path}
#'   \item{file}{the file name}
#'   \item{date}{date in "\%d\%/\%m\%/\%y\%" format}
#'   \item{time}{time in "\%H:\%M" format, PST}
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
