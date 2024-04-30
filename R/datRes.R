#' Election results for German state elections from 2000-2023
#'
#' Election result dataset for all German state elections from
#' January 2000 until December 2023. The dataset contains the
#' variables state, date, year, week, share of 'Zweitstimmen'
#' obtained by six main parties and party category other,
#' seats in state parliament obtained by the six main parties,
#' total number of seats in state parliament, voter turnout,
#' state election cycle since January 2000.
#'
#' The dataset was obtained from \insertCite{Wahlrecht.de}{fixedEventFC}
#' and \url{https://www.bundeswahlleiterin.de}
#' and employed in \insertCite{Fritsch2024}{fixedEventFC} which also
#' provides more details on the different variables.
#'
#' @name datRes
#'
#' @docType data
#'
#' @usage data(datRes)
#'
#' @format A dataset with 81 rows and 20 variables containing:
#' \describe{
#' \item{BL}{state identifier}
#' \item{date}{date of state election}
#' \item{year}{year of state election}
#' \item{week}{week of state election}
#' \item{CDU_CSU.Z}{share of 'Zweitstimmen' obtained by Christian Democratic/Social Union}
#' \item{SPD.Z}{share of 'Zweitstimmen' obtained by Social Democratic Party}
#' \item{Gruene.Z}{share of 'Zweitstimmen' obtained by Federation 90/The Greens}
#' \item{FDP.Z}{share of 'Zweitstimmen' obtained by Free Democratic Party}
#' \item{DIE_LINKE.Z}{share of 'Zweitstimmen' obtained by The Left Party}
#' \item{AfD.Z}{share of 'Zweitstimmen' obtained by Alternative for Germany}
#' \item{CDU_CSU}{seats in state parliament obtained by Christian Democratic/Social Union}
#' \item{SPD}{seats in state parliament obtained by Social Democratic Party}
#' \item{Gruene}{seats in state parliament obtained by Federation 90/The Greens}
#' \item{FDP}{seats in state parliament obtained by Free Democratic Party}
#' \item{DIE_LINKE}{seats in state parliament obtained by The Left Party}
#' \item{AfD}{seats in state parliament obtained by Alternative for Germany}
#' \item{Sitze_gesamt}{total number of seats in state parliament}
#' \item{WB}{voter turnout}
#' \item{Other.Z}{share of 'Zweitstimmen' obtained by other parties}
#' \item{elCycle}{state election cycle since January 2000}
#' }
#'
#' @keywords datasets
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' \dontrun{
#'   data(datRes, package = "fixedEventFC")
#'   nrow(datRes)
#' }
#'
NULL
