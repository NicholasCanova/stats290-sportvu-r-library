##
## Global variable for package sportVU
##


shiny.dataframes <- list(sportvu = data.frame(), 
                         shots = data.frame(),
                         rosters = data.frame())


#' Reset the global package variable \code{shiny.dataframes}
#'
#' @importFrom utils assignInMyNamespace
#' @return the default value value of sportVU package global \code{shiny.dataframes}
#' @export
#'
resetOptions <- function() {
  assignInMyNamespace("shiny.dataframes", list(sportvu = data.frame(), 
                                               shots = data.frame(),
                                               rosters = data.frame()))
}

#' Set the package's sportvu dataframe
#'
#' @param df the new, parsed dataframe to assign
#' @return the changed value of the package global \code{shiny.dataframes}
#' @export
#' @importFrom utils assignInMyNamespace
#'
setSportVU <- function(df) {
  shiny.dataframes <- getShinyDataframes()
  shiny.dataframes$sportvu <- df
  assignInMyNamespace("shiny.dataframes", shiny.dataframes)
}

#' Set the package's shots dataframe
#'
#' @param df the new, parsed dataframe to assign
#' @return the changed value of the package global \code{shiny.dataframes}
#' @importFrom utils assignInMyNamespace
#' @export
#'
setShots <- function(df) {
  shiny.dataframes <- getShinyDataframes()
  shiny.dataframes$shots <- df
  assignInMyNamespace("shiny.dataframes", shiny.dataframes)
}

#' Set the package's rosters dataframe
#'
#' @param df the new, parsed dataframe to assign
#' @return the changed value of the package global \code{shiny.dataframes}
#' @importFrom utils assignInMyNamespace
#' @export
#'
setRosters <- function(df) {
  shiny.dataframes <- getShinyDataframes()
  shiny.dataframes$rosters <- df
  assignInMyNamespace("shiny.dataframes", shiny.dataframes)
}

#' Return the value of the global package variable \code{shiny.dataframes}
#'
#' @return the current value of sportVU package global \code{shiny.dataframes}
#' @export
#'
getShinyDataframes <- function() {
  shiny.dataframes
}


