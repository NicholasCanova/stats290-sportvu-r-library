#' Generate selectInput dropdown options for ShinyApp Shot Charts
#' 
#' @description Generate selectInput dropdown options for ShinyApp
#' @param df the shots dataframe from which dropdown options are pulled from
#' @return list of 4 vectors
#' @importFrom stats setNames
#' @export
#' 
selectionCreatorShots <- function(df) {

  # toggle all players
  shooter = unique(df$player)
  shooter.selection = sort(setNames(shooter, shooter))
  shooter.selection = c("All Players", shooter.selection)
  names(shooter.selection)[1] = "All Players"
  
  # toggle all teams
  team = unique(df$team.name)
  team.selection = sort(setNames(team, team))
  team.selection = c("All Teams", team.selection)
  names(team.selection)[1] = "All Teams"
  
  # toggle which quarter
  period = unique(df$period)
  period.selection = sort(setNames(period, period))
  period.selection = c("All Periods", period.selection)
  names(period.selection)[1] = c("All Periods")
  
  # toggle off the dribble / pull up
  dribble = c("All Shots", "Off The Dribble", "Pull Up")
  dribble.selection = sort(setNames(dribble, dribble))
  
  # put all 4 selection vectors into a list, return list
  all.selections = list(shooter.selection = shooter.selection, 
                       team.selection = team.selection,
                       period.selection = period.selection,
                       dribble.selection = dribble.selection)
  all.selections
}


#' Generate selectInput dropdown options for ShinyApp Movement Chart
#' 
#' @description Generate selectInput dropdown options for ShinyApp
#' @param df the shots dataframe from which dropdown options are pulled from
#' @return list of 2 vectors
#' @export
#' 
selectionCreatorMovements <- function(df) {
  
  # toggle all players
  shooter = unique(df$player)
  shooter.selection = sort(setNames(shooter, shooter))
  shooter.selection = c("No Player Selected", shooter.selection)
  names(shooter.selection)[1] = "No Player Selected"
  
  # toggle game ID
  game.id = unique(df$game.id)
  game.id.selection = sort(setNames(game.id, game.id))
  game.id.selection = c("No Game Selected", game.id.selection)
  names(game.id.selection)[1] = "No Game Selected"
  
  # put all 2 selection vectors into a list, return list
  all.selections = list(shooter.selection = shooter.selection, 
                        game.id.selection = game.id.selection)
  
  all.selections
}