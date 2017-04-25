#' Calculates shot metrics for an individual player-game
#' 
#' @description Takes shot data and returns shooting metrics
#' @param shot_data a dataframe of sportVU data with shooting data
#' @return list of dataframes of data
#' @export 
#' @import dplyr
#' 
#' 
createShotTable <- function(shot_data) {
  shot_data$result2 <- shot_data$result
  shot_data$result2[shot_data$points.type == 3 & shot_data$result == 1] = 1.5
  
  shot_data$tfgm <- 0
  shot_data$tfgm[shot_data$result2 == 1.5] = 1
  
  shot_data$tfga <- 0
  shot_data$tfga[shot_data$points.type == 3] = 1
  
  by_qtr <- group_by(shot_data, period)
  shooting_qtr <- summarise(by_qtr,
                            fgm = sum(result),
                            fga = n(),
                            tfgm = sum(tfgm),
                            tfga = sum(tfga),
                            effective_fg_pct = mean(result2),
                            avg_shot_distance = mean(shot.distance)
  )

  shooting_qtr$fg_pct <- shooting_qtr$fgm / shooting_qtr$fga
  shooting_qtr$fg_pct <- shooting_qtr$fg_pct * 100
  shooting_qtr$fg_pct <- round(shooting_qtr$fg_pct, 1)
  shooting_qtr$fg_pct <- paste0(shooting_qtr$fg_pct, '%')

  shooting_qtr$effective_fg_pct <- shooting_qtr$effective_fg_pct * 100
  shooting_qtr$effective_fg_pct <- round(shooting_qtr$effective_fg_pct, 1)
  shooting_qtr$effective_fg_pct <- paste0(shooting_qtr$effective_fg_pct, '%')

  shooting_qtr$avg_shot_distance <- round(shooting_qtr$avg_shot_distance, 1)

  shooting_qtr$tfgm_pct <- shooting_qtr$tfgm / shooting_qtr$tfga
  shooting_qtr$tfgm_pct <- shooting_qtr$tfgm_pct * 100
  shooting_qtr$tfgm_pct <- round(shooting_qtr$tfgm_pct, 1)
  shooting_qtr$tfgm_pct <- paste0(shooting_qtr$tfgm_pct, '%')

  shooting_qtr$fgm <- round(shooting_qtr$fgm, 0)
  shooting_qtr$fga <- round(shooting_qtr$fga, 0)
  shooting_qtr$tfgm <- round(shooting_qtr$tfgm, 0)
  shooting_qtr$tfga <- round(shooting_qtr$tfga, 0)
  


  shooting_qtr <- shooting_qtr[c('period','fgm','fga','fg_pct','tfgm','tfga','tfgm_pct','effective_fg_pct','avg_shot_distance')]
  colnames(shooting_qtr) <- c('Qtr', 'FGM', 'FGA', 'FG%', '3PM', '3PA', '3P%', 'Eff. FG%', 'Avg. Shot Dist. (Ft)')
  return(shooting_qtr)
}

