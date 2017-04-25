# A. COURT LINE COORDINATES
# =========================

#' Calculate coordinates of NBA basketball court
#' 
#' @description Using the official specifications of NBA court lines, creates a set  
#' @description of coordinates that can be passed to ggplot2's geom_path() function 
#' @description to draw an NBA court.
#' @return dataframe of coordinates for NBA court lines
#' @export
#' @import dplyr
#' @keywords NBA ggplot2
#' 
drawCourt_ggplot = function(){
  
  # Setup Court Diagram Points
  # ==================================
  
  # A.1. Corner 3PT Line
  # ==================================
  left.corner_x = c(3, 3)
  left.corner_y = c(0, 14)
  
  right.corner_x = c(47, 47)
  right.corner_y = c(0, 14)
  
  # A.2. Three-Point Arc
  # ==================================
  # define arc angle.
  theta_wing = seq(atan(-22.000/9.25), atan(-9.27318148/22.02381), length.out = 200)
  theta_straight = seq(atan(-9.27318148/22.02381), atan(9.27318148/22.02381), length.out = 200)
  
  # arc_one is far wing
  arc_one = mutate(data.frame(x = -23.90*sin(theta_wing) + 25, 
                              y = 23.90*cos(theta_wing) + 4.75), 
                   distance = sqrt((x - 25)^2 + (y - 4.75)^2))
  
  # arc_two is straight-away
  arc_two = mutate(data.frame(x = -23.90*sin(theta_straight) + 25, 
                              y = 23.90*cos(theta_straight) + 4.75), 
                   distance = sqrt((x - 25)^2 + (y - 4.75)^2))
  
  # arc_three is near wing
  arc_three = mutate(data.frame(x = 23.90*sin(rev(theta_wing)) + 25, 
                                y = 23.90*cos(rev(theta_wing)) + 4.75), 
                     distance = sqrt((x - 25)^2 + (y - 4.75)^2))
  
  # adjust the end-points to precisely match corner three lines. 
  arc_one$x[1] = 47.000
  arc_one$y[1] = 14.000
  
  arc_three$x[nrow(arc_three)] = 3.000
  arc_three$y[nrow(arc_three)] = 14.000
  
  # A.3. Input three-point arc coordinates
  # ==================================
  arc_x = c(rev(arc_three$x), rev(arc_two$x), rev(arc_one$x))
  arc_y = c(rev(arc_three$y), rev(arc_two$y), rev(arc_one$y))
  
  # A.4 Combine 3PT Coordinates
  # ==================================
  three_point_x = c(left.corner_x, arc_x, right.corner_x)
  three_point_y = c(left.corner_y, arc_y, right.corner_y)
  three_point_ID = rep('three_point', length(three_point_x))
  
  
  # B.1. Some Free Throw Line Starters
  # ==================================
  left.outside_x = c(17, 17)
  left.outside_y = c(0, 19)
  
  right.outside_x = c(33, 33)
  right.outside_y = c(0, 19.00)
  
  free.throw.line_x = c(17, 33)
  free.throw.line_y = c(19, 19)
  
  FT.perimeter_x = c(left.outside_x, free.throw.line_x, right.outside_x)
  FT.perimeter_y = c(left.outside_y, free.throw.line_y, right.outside_y)
  FT.perimeter_ID = rep('FT_perimeter', length(FT.perimeter_x))
  
  left.inside_x = c(19, 19)
  left.inside_y = c(0, 19)
  left.inside_ID = rep('FT_inside_left', length(left.inside_x))
  
  right.inside_x = c(31, 31)
  right.inside_y = c(0, 19)
  right.inside_ID = rep('FT_inside_right', length(right.inside_x))
  
  # B.2. Center Court Circles
  # ==================================
  num.pts = 200
  full.circle = seq(0, 2*pi, length.out = num.pts)
  
  FT_circle_x = 6*cos(full.circle) + 25
  FT_circle_y = 6*sin(full.circle) + 19
  FT_circle_ID = rep('FT_circle_detail', num.pts)
  
  # B.3. Combine FT Coordinates
  # ==================================
  FT_x = c(FT.perimeter_x, left.inside_x, right.inside_x, FT_circle_x)
  FT_Y = c(FT.perimeter_y, left.inside_y, right.inside_y, FT_circle_y)
  FT_ID = c(FT.perimeter_ID, left.inside_ID, right.inside_ID, FT_circle_ID)
  
  
  # C.1. Court Perimeter 
  # ==================================
  court.perimeter_x = c(0, 50, 50, 50, 50, 0, 0, 0)
  court.perimeter_y = c(0, 0, 0, 47, 47, 47, 47, 0)
  court.perimeter_ID = rep(c('court_perimeter_B',
                             'court_perimeter_R',
                             'court_perimeter_T',
                             'court_perimeter_L'), each = 2) 
  # C.2. Court Detail
  # ==================================
  court.detail_x = c(0, 3, 47, 50)
  court.detail_y = c(28, 28, 28, 28)
  court.detail_ID = rep(c('detail_left', 'detail_right'), each = 2)
  
  baseline.detail_x = c(14, 14, 36, 36)
  baseline.detail_y = c(0, 1, 0, 1)
  baseline.detail_ID = rep(c('baseline_left', 'baseline_right'), each = 2)
  
  # C.3. Center Court Circles
  # ==================================
  num.pts = 200
  half.circle = seq(pi, 2*pi, length.out = num.pts)
  
  circle_x = c(6*cos(half.circle) + 25, 2*cos(half.circle) + 25)
  circle_y = c(6*sin(half.circle) + 47, 2*sin(half.circle) + 47)
  circle_ID = rep(c('inner_circle_detail', 'outer_circle_detail'), each = num.pts)
  
  # C.4. Combine Detail Coordinates
  # ==================================
  detail_x = c(court.perimeter_x, court.detail_x, circle_x)
  detail_y = c(court.perimeter_y, court.detail_y, circle_y)
  detail_ID  =c(court.perimeter_ID, court.detail_ID, circle_ID)
  
  
  # D. Final Combnation of everything
  # ==================================
  x_coordinates = c(three_point_x, FT_x, detail_x, baseline.detail_x)
  y_coordinates = c(three_point_y, FT_Y, detail_y, baseline.detail_y)
  ID = c(three_point_ID, FT_ID, detail_ID, baseline.detail_ID)
  
  court_coordinates = data.frame(x = x_coordinates,
                                 y = y_coordinates, 
                                 group_ID = ID)
  return(court_coordinates)
}
# =====


# B. BACKBOARD LINE COORDINATES
# =============================

#' Calculate coordinates of NBA basketball court
#' 
#' @description Using the official specifications of NBA backboard and hoop dimensions,   
#' @description creates a set of coordinates that can be passed to ggplot2's geom_path()
#' @description function to draw an NBA court.
#' @return dataframe of coordinates for NBA backboard and hoop
#' @export
#' @keywords NBA ggplot2
#' 
drawBackboard_Basket_ggplot = function(){
  
  # A.1. Draw the Backboard
  # ==================================
  backboard_x = c(22, 28)
  backboard_y = c(4, 4)
  backboard_ID = rep('backboard', length(backboard_x))
  
  # A.2. Draw the Basket
  # ==================================
  basket.theta = seq(0, 2*pi, length.out = 100)
  
  basket_x = 0.75*cos(basket.theta) + 25
  basket_y = 0.75*sin(basket.theta) + 4.75
  basket_ID = rep('basket', length(basket.theta))
  
  
  # B.1. Draw the Basket
  # ==================================  
  x_coordinates = c(backboard_x, basket_x)
  y_coordinates = c(backboard_y, basket_y)
  ID = c(backboard_ID, basket_ID)
  
  court_coordinates = data.frame(x = x_coordinates,
                                 y = y_coordinates, 
                                 group_ID = ID)
  return(court_coordinates)
}
# =====


# C. SHOT METRIC SUMMARY
# ======================

#' Convert NBA shot coordinates to display on half the court
#' 
#' @description Transforms each (x,y) coordinate in the shot dataframe 
#' @description to the new (x,y) coordinate it corresponds to with respect to
#' @description the half court drawn by createBlank_ShotChart() function. 
#' @param shot_data a formatted shots dataframe of shot information (coords)  
#' @return shot dataframe with the new x and y locations
#' @importFrom plyr round_any
#' @export
#' @keywords shot_data
#' 
halfcourtData <- function(shot_data) {
  
  # convert made/missed shot, and # of dribbles, into binary 0 1 variables
  shot_data$result <- ifelse(shot_data$result == "missed", 0, 1)
  shot_data$dribbles <- ifelse(shot_data$dribbles > 0, "Off The Dribble", "Pull Up")
  
  # transform x and y coordinates so that they relate to half the court
  x_loc <- shot_data$x_loc
  y_loc <- shot_data$y_loc
  shot_data$x_loc <- ifelse(x_loc > 47, 50-y_loc, y_loc)
  shot_data$y_loc <- ifelse(x_loc > 47, -(94-x_loc), -x_loc)

  # round the x and y coordinates
  shot_data$x_loc <- round_any(shot_data$x_loc, 3)
  shot_data$y_loc <- round_any(shot_data$y_loc, 3)

  return(shot_data)
}
# =====


# D. SHOT PERCENTAGES
# ===================
# 
#' Convert shot data into player shot percentages by location 
#' 
#' @description The function takes as an argument the shot data filtered by a specific 
#' @description player and other criteria, and computes the number of attempts and 
#' @description the percent of those attempts that were successful, at each unique (x,y)   
#' @description location in the input data
#' @param player.data a dataframe of shot info filtered by player, etc. 
#' @return dataframe with information on the number of attempts and successes at each (x,y) location of the player
#' @export
#' @import dplyr
#' @keywords NBA chart_data
#' 
generateChart_Data = function(player.data){
  
  # Extract Player Data
  # ===================
  player.attempts = player.data %>% group_by(x_loc, y_loc) %>% tally()
  colnames(player.attempts) = c('x', 'y', 'player.attempts')
  
  player.makes = player.data %>% group_by(x_loc, y_loc) %>% tally(result)
  colnames(player.makes) = c('x', 'y', 'player.makes')
  
  player = mutate(merge(player.makes, 
                        player.attempts, 
                        by.x = c('x', 'y'), 
                        by.y = c('x', 'y')), 
                  player.percent = player.makes/player.attempts)
  return(player)
}
# =====


# E. GGPLOT EMPTY SHOT CHART (HALF COURT)
# ==========================

#' Draw vertical-oriented half-court of NBA basketball court lines, backboard and hoop
#' 
#' @description Using the coordinates output from both the drawCourt_ggplot and 
#' @description drawBackboard_Basket_ggplot functions, create a ggplot outline of  
#' @description an NBA court.
#' @param plot.title The title of the plot
#' @return ggplot object of an NBA half-court
#' @export
#' @import ggplot2
#' @keywords NBA ggplot2
#' 
createBlank_ShotChart = function(plot.title){
 
  # Court parameters
  chartTextColor = 'black'
  chartBgColor = 'white'
  
  courtLine_Color = 'black'
  courtLine_Size = 1.15
  basketColor = 'black'
  basketSize = 1.25
  
  # A. Initialize Plot
  # ==================
  ggplot() + 
    
  # B. Draw Court 
  # =============
  geom_path(data = drawCourt_ggplot(), 
            mapping = aes(x = x, 
                          y = -y, 
                          group = group_ID),
            color = courtLine_Color, size = courtLine_Size) +
    
  # C. Draw basket and backboard
  # ============================
  geom_path(data = drawBackboard_Basket_ggplot(), 
            mapping = aes(x = x, 
                          y = -y, 
                          group = group_ID),
            color = basketColor, size = basketSize) +
    
    # D. Plot title and restrict plot to one half of court 
    # ====================================================
  xlim(c(0, 50)) +
    ylim(c(-47, 0)) +
    
    ggtitle(plot.title) + 
    
    # E. Define background colors, plot labels, etc.
    # ==============================================
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        legend.key = element_rect(colour = NA, fill = chartBgColor, size = 0.5),
        legend.title = element_text(size = 16, color = chartTextColor),
        legend.text = element_text(size = 16, face = 'bold', color = chartTextColor),
        legend.background = element_rect(fill = chartBgColor),
        plot.title = element_text(size = 24, hjust = 0.5, vjust = 0.5, color = chartTextColor, margin = margin(10, 0, 10, 0)),
        plot.background = element_rect(fill = chartBgColor, color = chartBgColor),
        panel.background = element_rect(fill = chartBgColor),
        panel.border = element_rect(color = courtLine_Color, fill = NA, size = 3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
}
# =====


# F. GGPLOT EMPTY MOTION CHART (FULL COURT)
# =========================================

#' Draw horizontal-oriented full-court of NBA basketball court lines, backboard and hoop
#' 
#' @description Using the coordinates output from both the drawCourt_ggplot and 
#' @description drawBackboard_Basket_ggplot functions, create a ggplot outline of  
#' @description an NBA court.
#' @param plot.title The title of the plot
#' @return ggplot object of an NBA full-court
#' @export
#' @import ggplot2
#' @keywords NBA ggplot2
#' 
createBlank_motionChart = function(plot.title) {
  
  # A. Court parameters
  # ================
  chartTextColor = 'black'
  chartBgColor = 'white'
  
  courtLine_Color = 'black'
  courtLine_Size = 1.15
  basketColor = 'black'
  basketSize = 1.25
  
  # B. Double the court and basket data 
  # ===================================
  court_data_left = drawCourt_ggplot()
  court_data_right = drawCourt_ggplot()
  court_data_right$y = 94 - court_data_right$y
  
  basket_data_left = drawBackboard_Basket_ggplot()
  basket_data_right = drawBackboard_Basket_ggplot()
  basket_data_right$y = 94 - basket_data_right$y
  
  # C. Initialize Plot
  # ==================
  ggplot() + 
    
  # D. Draw Court 
  # =============
  geom_path(data = court_data_left, 
            mapping = aes(x = y, 
                          y = x, 
                          group = group_ID),
            color = courtLine_Color, size = courtLine_Size) +
    
  geom_path(data = court_data_right, 
            mapping = aes(x = y, 
                          y = x, 
                          group = group_ID),
            color = courtLine_Color, size = courtLine_Size) +
  
  # E. Draw basket and backboard
  # ============================
  geom_path(data = basket_data_left, 
            mapping = aes(x = y, 
                          y = x, 
                          group = group_ID),
            color = basketColor, size = basketSize) +
    
  geom_path(data = basket_data_right, 
            mapping = aes(x = y, 
                          y = x, 
                          group = group_ID),
            color = basketColor, size = basketSize) +
    
  # F. Plot title and restrict plot to one half of court 
  # ====================================================
  xlim(c(0, 94)) +
  ylim(c(0, 50)) +
    
  ggtitle(plot.title) +
    
  # G. Define background colors, plot labels, etc.
  # ==============================================
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        legend.key = element_rect(colour = NA, fill = chartBgColor, size = 0.5),
        legend.title = element_text(size = 16, color = chartTextColor),
        legend.text = element_text(size = 16, face = 'bold', color = chartTextColor),
        legend.background = element_rect(fill = chartBgColor),
        plot.title = element_text(size = 24, hjust = 0.5, vjust = 0.5, color = chartTextColor, margin = margin(10, 0, 10, 0)),
        plot.background = element_rect(fill = chartBgColor, color = chartBgColor),
        panel.background = element_rect(fill = chartBgColor),
        panel.border = element_rect(color = courtLine_Color, fill = NA, size = 3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
}
# =====


# G. GGPLOT FILLED-IN SHOT CHART (HALF COURT)
# =======================================

#' Draw vertical-oriented full-court of NBA basketball shot chart
#' 
#' @description Using the coordinates output from both the drawCourt_ggplot and 
#' @description drawBackboard_Basket_ggplot functions, first create a ggplot outline  
#' @description of an NBA court. Then add an additional layer of geom_point() that   
#' @description displays a player's color-coded shot percentages from various (x,y)
#' @description locations on the court.
#' @param x_loc vector with x coordinate of the shooter's location on court
#' @param y_loc vector with y coordinate of the shooter's location on court
#' @param makes vector with the number of successful attempts at each (x_loc,y_loc)
#' @param attempts vector with the number of attempts made at each (x_loc,y_loc)
#' @param plotTitle the title of the plot
#' @return ggplot object of an NBA player's shot chart
#' @export
#' @import ggplot2
#' @keywords NBA ggplot2
#' @examples 
#' shots <- merge(test_shots, 
#' test_rosters[, names(test_rosters) %in% c("global.player.id", "player", "team.name")],
#' by = "global.player.id", all.x = TRUE)
#' shots <- halfcourtData(shots)
#' player.data = shots[shots$player == "Kobe Bryant", ]
#' chart.data = generateChart_Data(player.data)
#' plot.title = 'Kobe Bryant Shot Chart'
#' createShot_Chart(chart.data$x, chart.data$y, chart.data$player.makes, 
#' chart.data$player.attempts, plot.title)
createShot_Chart = function(x_loc, y_loc, makes, attempts, plotTitle){
  
  # Court parameters
  chartTextColor = 'black'
  chartBgColor = 'white'
  courtLine_Color = 'black'
  courtLine_Size = 1.15
  basketColor = 'black'
  basketSize = 1.25
  
  # Color - based on shots in a surrounding area
  the_color = c()
  for (i in 1:length(makes)) {
    x_i = x_loc[i]
    y_i = y_loc[i]
    
    indices = which(x_loc <= x_i+3 & x_loc >= x_i-3 & y_loc <= y_i+3 & y_loc >= y_i-3)
    pct = sum(makes[indices]) / sum(attempts[indices])
    pct = ifelse(is.na(pct), 0, pct)
    
    the_color = c(the_color, pct)
  }
  
  # Size, based on a scaled version of the number of attempts
  log_attempts = log(attempts + 0.33)
  scale = max(log_attempts)
  the_size = 16 * (log_attempts / scale)
  
  # A. INITIALIZE PLOT
  # ==================
  ggplot() + 
    
  # B. Draw Court 
  # =============
  geom_path(data = drawCourt_ggplot(), 
            mapping = aes(x = x, 
                          y = -y, 
                          group = group_ID),
            color = courtLine_Color, size = courtLine_Size) +
    
  # C. Plot shooter's shot chart and add colors
  # ============================
  geom_point(aes(x = x_loc, y = y_loc, color = the_color),
                 alpha=0.9,
                 size = the_size) +

  scale_color_gradient2(low="deepskyblue", mid = "dodgerblue", high="red", 
                        midpoint = mean(the_color), guide = guide_colourbar(title ="Percent of shots made")) +
                        # guide = guide_colourbar(title ="Percent of shots made")) +
    
  # D. Draw basket and backboard
  # ============================
  geom_path(data = drawBackboard_Basket_ggplot(), 
            mapping = aes(x = x, 
                          y = -y, 
                          group = group_ID),
            color = basketColor, size = basketSize) +
    
  # E. Plot title and restrict plot to one half of court 
  # ====================================================
  ggtitle(plotTitle) +
    xlim(c(0, 50)) +
    ylim(c(-47, 0)) +
    
  guides(alpha = FALSE,
         size = FALSE) +
    
    
  # F. Define background colors, plot labels, etc.
  # ==============================================
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        legend.key = element_rect(colour = NA, fill = chartBgColor, size = 0.5),
        legend.background = element_rect(fill = chartBgColor),
        plot.title = element_text(size = 24, hjust = 0.5, vjust = 0.5, color = chartTextColor, margin = margin(10, 0, 10, 0)),
        plot.background = element_rect(fill = chartBgColor, color = chartBgColor),
        panel.background = element_rect(fill = chartBgColor),
        panel.border = element_rect(color = courtLine_Color, fill = NA, size = 3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
}

# =====


# H. GGPLOT FILLED-IN MOTION CHART (FULL COURT)
# =============================================

#' Draw horizontal-oriented full-court of NBA basketball movement-tracking chart
#' 
#' @description Using the coordinates output from both the drawCourt_ggplot and 
#' @description drawBackboard_Basket_ggplot functions, first create a ggplot outline  
#' @description of an NBA court. Then add an additional layer of player coordinates, 
#' @description plotting the (x,y) location of a player 25x per second, revealing a 
#' @description unique movement-tracking chart of the player's motions on the court.
#' @param plot.title the title of the plot
#' @param x_loc vector with x coordinates of a shooter's position
#' @param y_loc vector with  y coordinates of a shooter's position
#' @return ggplot object of an NBA player running around the court
#' @export
#' @import ggplot2
#' @keywords NBA ggplot2
#' 
createMotion_Chart = function(plot.title, x_loc, y_loc){
  
  # A. Court parameters
  # ===================
  chartTextColor = 'black'
  chartBgColor = 'white'
  
  courtLine_Color = 'black'
  courtLine_Size = 1.15
  basketColor = 'black'
  basketSize = 1.25
  
  # B. Pre-processing Before Plotting 
  # =================================
  # mirror the court / basketball data
  court_data_left = drawCourt_ggplot()
  court_data_right = drawCourt_ggplot()
  court_data_right$y = 94 - court_data_right$y
  
  basket_data_left = drawBackboard_Basket_ggplot()
  basket_data_right = drawBackboard_Basket_ggplot()
  basket_data_right$y = 94 - basket_data_right$y
  
  # distances vector 
  end = length(x_loc)
  distances = sqrt(((x_loc[2:end] - x_loc[1:(end-1)]) ^ 2) + ((y_loc[2:end] - y_loc[1:(end-1)]) ^ 2))
  
  # colors (continuous 0-1), size (binary (0,1) to remove large distances), and group (all 1)
  my_colors = seq(1,end,by=1)/(2*end)
  my_split = c(ifelse(distances > 1, '1', '0'), '0')
  my_group = rep(1,end)
  
  # hexagons for highlighting where players run frequently
  coordinate_counts = discretizeCourt(x_loc, y_loc)
  coordinate_counts = coordinate_counts[coordinate_counts$count > 7, ]
  
  # coord sizes and colors
  coord_colors = coordinate_counts$count
  coord_colors = 0.5 + ((coord_colors-min(coord_colors)) / (2*(max(coord_colors)-min(coord_colors))))
  coord_sizes = (coord_colors - 0.5) * 30
  
  # create line for 
  
  # C. Initialize Plot
  # ==================
  ggplot() + 
    
  # D. Draw running around coordinates
  # ==================================
  geom_path(aes(x = x_loc, y = y_loc, col = my_colors, size = my_split, group = my_group), 
            alpha = 0.3) + 
  # scale_color_manual(values = c("black", "white")) +
  scale_color_gradient(limits = c(0,1), low = 'blue', high = 'darkred') +
  scale_size_manual(values = c(0.6, 0)) +
    
  # C.2 Draw hexagons highlighting where players run frequently
  geom_point(aes(x = coordinate_counts$x, y = coordinate_counts$y, col = coord_colors),
             size = coord_sizes,
             shape = 16,
             # color = coord_sizes+1,
             alpha = 0.5) + 
      
  # E. Draw Court 
  # =============
  geom_path(data = court_data_left, 
            mapping = aes(x = y, 
                          y = x,
                          group = group_ID),
            color = courtLine_Color, size = courtLine_Size) +
    
  geom_path(data = court_data_right, 
            mapping = aes(x = y, 
                          y = x, 
                          group = group_ID),
            color = courtLine_Color, size = courtLine_Size) +
  
  # F. Draw basket and backboard
  # ============================
  geom_path(data = basket_data_left, 
            mapping = aes(x = y, 
                          y = x, 
                          group = group_ID),
            color = basketColor, size = basketSize) +
    
  geom_path(data = basket_data_right, 
            mapping = aes(x = y, 
                          y = x, 
                          group = group_ID),
            color = basketColor, size = basketSize) +
  
  # G. Plot title and restrict plot to one half of court 
  # ====================================================
  xlim(c(0, 94)) +
  ylim(c(0, 50)) +
    
  ggtitle(plot.title) + 
  # H. Define background colors, plot labels, etc.
  # ==============================================
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        legend.position = 'null',
        plot.title = element_text(size = 24, hjust = 0.5, vjust = 0.5, color = chartTextColor, margin = margin(10, 0, 10, 0)),
        plot.background = element_rect(fill = chartBgColor, color = chartBgColor),
        panel.background = element_rect(fill = chartBgColor),
        panel.border = element_rect(color = courtLine_Color, fill = NA, size = 3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
}
# ===== 
