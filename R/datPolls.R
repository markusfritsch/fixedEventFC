#' Election poll data for German state elections from 2000-2023
#'
#' Election poll dataset for all German state elections from
#' January 2000 until December 2023. The dataset contains the
#' variables state, institute, poll type, poll participants,
#' date when poll is published, time horizon during which poll
#' was conducted, year of poll, week of poll, poll index in
#' state since January 2000, election cycle since January 2000,
#' poll index in election cycle since January 2000, starting
#' date of poll, end date of poll, number of days during which
#' poll was conducted, days from end of poll until poll was
#' published, days ahead of election, shares of six main
#' parties and party category other.
#'
#' The dataset was collected by \insertCite{Wahlrecht.de}{fixedEventFC}
#' and employed in \insertCite{Fritsch2024}{fixedEventFC} which also
#' provides more details on the different variables.
#'
#' @name datPolls
#'
#' @docType data
#'
#' @usage data(datPolls)
#'
#' @format A dataset with 2299 rows and 23 variables containing:
#' \describe{
#' \item{BL}{state identifier}
#' \item{Institut}{institute}
#' \item{PollType}{type of election poll}
#' \item{PollParticipants}{number of participants in the poll}
#' \item{PollPublished}{date on which poll was published}
#' \item{PollDate}{time horizon during which poll was conducted}
#' \item{PollYear}{year in which poll was publised}
#' \item{PollWeek}{week in which poll was publised}
#' \item{PollIndex}{poll index in state since January 2000}
#' \item{PollElCycle}{election cycle since January 2000}
#' \item{PollIndElCycle}{poll index in election cycle since January 2000}
#' \item{PollStart}{starting date of poll}
#' \item{PollEnd}{end date of poll}
#' \item{PollDays}{number of days during which poll was conducted}
#' \item{DaysTillPollPublished}{days from end of poll until poll was published}
#' \item{DaysToEl}{days ahead of election}
#' \item{CDU_CSU}{share of Christian Democratic/Social Union}
#' \item{SPD}{share of Social Democratic Party}
#' \item{Gruene}{share of Federation 90/The Greens}
#' \item{FDP}{share of Free Democratic Party}
#' \item{DIE_LINKE}{share of The Left Party}
#' \item{AfD}{share of Alternative for Germany}
#' \item{Sonstige}{share of other parties}
#' }
#'
#' @keywords datasets
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' \dontrun{
#'   data(datPolls, package = "fixedEventFC")
#'   n <- datPolls$PollParticipants
#'   mean(n)
#' }
#'
NULL

