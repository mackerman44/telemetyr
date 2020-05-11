#' Raw .txt Format Telemetry Data
#'
#' An example dataset containing observations for 5 receivers
#' and 1 frequency, 2018/2019 season
#'
#' @format A data frame with 696930 rows and 9 variables:
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
#' for 5 receivers and 1 frequency, 2018/2019 season
#'
#' @format A data frame with 66468 rows and 7 variables:
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

#' Site Metadata
#'
#' Metadata for tag release and observation sites for the Lemhi River
#' juvenile Chinook salmon winter telemetry project
#'
#' @format A data frame with 45 rows and 12 variables:
#' \describe{
#'   \item{site_name}{name of release or observation site}
#'   \item{site_code}{site code, PTAGIS code in the case of \code{site_type == rst} | \code{site_type == pit_array}}
#'   \item{site_type}{rotary screw trap (rst), fixed radio telemetry, or PIT tag array}
#'   \item{receivers}{a list of receivers that have been used at the site}
#'   \item{rt_site_type}{for \code{site_type == rt_fixed}, pod or trailer}
#'   \item{point_y}{latitude, WGS84, decimal degrees}
#'   \item{point_x}{longitude, WGS84, decimal degrees}
#'   \item{use17_18}{Was the site used during the 2017/2018 pilot season?}
#'   \item{use18_19}{Was the site used during the 2018/2019 season?}
#'   \item{use19_20}{Was the site used during the 2019/2020 season?}
#'   \item{ptagis_rkm}{river rkm from PTAGIS}
#'   \item{rt_rkm}{river rkm for Lemhi River telemetry project, \code{site_code == TB} was designated as "000"}
#' }
#' @source Biomark NAS - data/telemetry/lemhi/fixed_site_downloads/2018_2019/
"site_metadata"

#' Tag Release Information
#'
#' Data for tags deployed in the Lemhi River juvenile Chinook
#' salmon winter telemetry project
#'
#' @format A data frame with 776 rows and 12 variables:
#' \describe{
#'   \item{season}{the year/season the tag was deployed}
#'   \item{tag_id}{tag ID including frequency and code}
#'   \item{pit_tag_id}{the PIT tag number also injected into the fish}
#'   \item{srr}{species, run, rear, uses PTAGIS nomenclature}
#'   \item{length}{length, millimeters}
#'   \item{weight}{weight, grams}
#'   \item{tag_purpose}{Was tag deployed in a fish, used as test tag, or other?}
#'   \item{release_site}{location that a fish was released at}
#'   \item{duty_cycle}{the batch or duty cycle of the fish}
#'   \item{activation_time}{the activation time of the tag, Pacific Standard}
#'   \item{release_time}{the release time for the fish, Pacific Standard}
#'   \item{notes}{relevant notes}
#' }
#' @source Biomark NAS - data/telemetry/lemhi/fixed_site_downloads/2018_2019/
"tag_releases"

#' Capture Histories - Long Format
#'
#' Capture histories, in long format, for 5 receivers and tags
#' in \code{tag_df} from Lemhi River juvenile Chinook salmon winter
#' telemetry project
#'
#' @format A data frame with 52 rows and 6 variables:
#' \describe{
#'   \item{tag_id}{tag ID including frequency and code}
#'   \item{loc}{observation location, site or receiver}
#'   \item{first_obs}{the date and time of first observation at loc}
#'   \item{last_obs}{the date and time of last observation at loc}
#'   \item{n}{the number of records that occurred between \code{first_obs} and \code{last_obs}}
#'   \item{week}{the week number the first observation is from}
#' }
#' @source Biomark NAS - data/telemetry/lemhi/fixed_site_downloads/2018_2019/
"ch_long"

#' Capture Histories - Wide Format
#'
#' Capture histories, in wide format, for 5 receivers and tags
#' in \code{tag_df} from Lemhi River juvenile Chinook salmon winter
#' telemetry project
#'
#' @format A data frame with 16 rows and 7 variables:
#' \describe{
#'   \item{tag_id}{tag ID including frequency and code}
#'   \item{cap_hist}{the concatenated capture history}
#'   \item{LH}{the site's code; Lemhi Hole}
#'   \item{CA}{the site's code; Carmen Creek}
#'   \item{TR}{the site's code; Tower Rock}
#'   \item{RR}{the site's code; Red Rock}
#'   \item{NF}{the site's code; North Fork}
#' }
#' @source Biomark NAS - data/telemetry/lemhi/fixed_site_downloads/2018_2019/
"ch_wide"

#' Tag Releases - For Capture Histories
#'
#' A subset of \code{tag_releases} for those tags included
#' in \code{ch_long} and \code{ch_wide}
#'
#' @format A data frame with 101 rows and 13 variables:
#' \describe{
#'   \item{season}{the year/season the tag was deployed}
#'   \item{radio_tag_id}{the radio tag ID including frequency, code and letter}
#'   \item{pit_tag_id}{the PIT tag number also injected into the fish}
#'   \item{tag_id}{tag ID including frequency and code}
#'   \item{srr}{species, run, rear, uses PTAGIS nomenclature}
#'   \item{length}{length, millimeters}
#'   \item{weight}{weight, grams}
#'   \item{tag_purpose}{Was tag deployed in a fish, used as test tag, or other?}
#'   \item{release_site}{location that a fish was released at}
#'   \item{duty_cycle}{the batch or duty cycle of the fish}
#'   \item{activation_time}{the activation time of the tag, Pacific Standard}
#'   \item{release_time}{the release time for the fish, Pacific Standard}
#'   \item{notes}{relevant notes}
#' }
#' @source Biomark NAS - data/telemetry/lemhi/fixed_site_downloads/2018_2019/
"tag_df"

#' Volt & Temperature Data
#'
#' An example dataset containing voltage and temperature data recorded
#' by 6 telemetry receivers, 2018/2019 season
#'
#' @format A data frame with 77826 rows and 11 variables:
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
"volt_temp"
