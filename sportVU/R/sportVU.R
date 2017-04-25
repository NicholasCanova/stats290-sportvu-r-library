#' A package with built-in ShinyApp for visualizing NBA sports data.
#'
#' This package is useful to users who want to visualize and analyse NBA sports data.
#' It provides a list of parser functions that the user can call using \code{link{masterParse}} 
#' function to parse a variety of XML files. After having parsed the shots data and sportvu data,
#' the user can launch the ShinyApp by calling \code{link{runShotChart}} function. The ShinyApp
#' helps visuzlize such data through a motion chart and a shot chart.
#' @seealso \code{link{masterParse}}
#' @seealso \code{link{runShotChart}}
#' @importFrom utils globalVariables
#' @docType package
#' @references Hadley Wickham's book at \url{http://r-pkgs.had.co.nz/}
#' @name sportVU
NULL


utils::globalVariables(names = c("x","y","group_ID","period","result","tfgm","tfga",
                                 "result2","shot.distance","x_loc","y_loc","game_clock",
                                 "quarter.id","time_elapsed","vel","dist"))