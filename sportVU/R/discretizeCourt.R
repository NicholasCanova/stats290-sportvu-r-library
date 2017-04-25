#' Discretize the NBA court for plotting hexagons
#' 
#' @description Discretizes the NBA court into 26x47 points
#' @param x_loc the x-coordinates
#' @param y_loc the y-coordinates
#' @return dataframe of counts for each coordinate
#' @export
#' @importFrom plyr round_any
#' @keywords discretize
#'
discretizeCourt <- function(x_loc, y_loc) {
  y <- seq(0, 50, by = 2)
  x1 <- seq(1, 93, by = 2)
  x2 <- seq(2, 94, by = 2)
  x <- c(x1, x2)
  
  coordinates <- data.frame(
    x = rep(x, 13),
    y = rep(y, each = length(x1)),
    count = 0
  )
  
  for (i in seq(1,length(x_loc),by=10)) {
    this_x = x_loc[i]
    this_y = y_loc[i]
    
    # round y to nearest even number
    test_min_y <- round_any(this_y, 2)
    
    if((test_min_y + 2) %% 4 == 0) {
      test_min_x <- round_any(this_x, 2)
    } else {
      test_min_x <- round_any(this_x + 1, 2) - 1
    }
    
    coordinates$count[coordinates$x == test_min_x & coordinates$y == test_min_y] <-
      coordinates$count[coordinates$x == test_min_x & coordinates$y == test_min_y] + 1
  }  
  
  return(coordinates)
}
