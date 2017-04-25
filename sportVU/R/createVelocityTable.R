#' Calculate velocities for 1/25 second intervals
#' 
#' @description Takes coordinate (x,y) vectors as parameters, and returns   
#' @description the movement metrics from moment to moment  
#' @param xloc a vector of x coordinate data for one player
#' @param yloc a vector of y coordinate data for one player
#' @return vector of velocities
#' @export
#' @import dplyr
#' 
velocity <- function(xloc, yloc) {
  
  # calculate change in coordinates from coord to coord
  diffx <- as.vector((diff(xloc)))
  diffy <- as.vector((diff(yloc)))

  # calculate pythagorean change in distance
  diffx_sq <- diffx ^ 2
  diffy_sq <- diffy ^ 2
  a <- sqrt(diffx_sq + diffy_sq)
  
  # convert to miles per hour
  b <- a * 25 * 0.681818
  b <- c(0, b)
  b
}


#' Calculate distances traveled for 1/25 second intervals
#' 
#' @description Takes coordinate (x,y) vectors as parameters, and returns   
#' @description the distances traveled from moment to moment  
#' @param xloc a vector of x coordinate data for one player
#' @param yloc a vector of y coordinate data for one player
#' @return vector of distances traveled
#' @export
#' 
distance <- function(xloc, yloc){
  
  # calculate change in coordinates from coord to coord
  diffx <- as.vector((diff(xloc)))
  diffy <- as.vector((diff(yloc)))
  
  # calculate pythagorean change in distance
  diffx_sq <- diffx ^ 2
  diffy_sq <- diffy ^ 2
  a <- sqrt(diffx_sq + diffy_sq)

  # convert from feet to miles  
  c <- a / 5280  
  c <- c(0, c)
  c
}


#' Calculate time a player played in game
#' 
#' @description Takes vector of game clock times an individual player was on     
#' @description court, and returns time elapsed by a player on court  
#' @param clock a vector of game-clock times
#' @return a vector of times elapsed
#' @export
#' 
time_played <- function(clock) {
  time_elapsed <- as.vector(abs((diff(clock))))
  time_elapsed <- c(0, time_elapsed)
}


#' Create dataframe with movement metrics for individual player-game
#' 
#' @description Takes sportvu dataframe with x_loc, y_loc, game_clock columns, 
#' @description and calculates / returns a number of movement metrics. 
#' @param data a sportvu dataframe with x_loc, y_loc and game_clock columns
#' @return a dataframe of movement metrics
#' @export
#' @import dplyr
#' @importFrom stats quantile
#' 
createVelocityTable <- function(data) {

  # calculate velocity, distance, and time played vectors
  # if time elapsed > 1 second, player subbed out and in, set time elapsed to 0
  data$vel <- velocity(data$x_loc, data$y_loc)
  data$dist <- distance(data$x_loc, data$y_loc)
  data$time_elapsed <- time_played(data$game_clock)
  data$time_elapsed[data$time_elapsed > 1] = 0
  

  
  # filter out extreme values (top 1%) that may be outliers due to substutions / time outs / game stoppages
  data <- data[data$vel <= quantile(data$vel, 0.99) & data$dist <= quantile(data$dist, 0.99), ]
  
  # group by quarter then summarize each column by summarizing functions below
  by_qtr <- group_by(data, quarter.id)
  mvmt_qtr <- summarise(by_qtr,
                        time_played = sum(time_elapsed),
                        avg_speed_mph = mean(vel),
                        top_speed_mph = max(vel),
                        miles_run = sum(dist))
  
  # format data appropriately
  mvmt_qtr$avg_speed_mph <- round(mvmt_qtr$avg_speed_mph, 1)
  mvmt_qtr$top_speed_mph <- round(mvmt_qtr$top_speed_mph, 1)
  mvmt_qtr$miles_run <- round(mvmt_qtr$miles_run, 1)
  mvmt_qtr$time_played <- mvmt_qtr$time_played / 60  # per minute
  mvmt_qtr$time_played <- round(mvmt_qtr$time_played, 1)  
  mvmt_qtr$miles_per36_min <- mvmt_qtr$miles_run / mvmt_qtr$time_played * 36
  mvmt_qtr$miles_per36_min <- round(mvmt_qtr$miles_per36_min, 1)

  
  colnames(mvmt_qtr) <- c('Qtr', 'Time Played (min)', 'Avg. Speed (mph)', 'Top Speed (mph)', 'Miles Run', 'Miles Run per 36 Min.')
  
  return(mvmt_qtr)
}

