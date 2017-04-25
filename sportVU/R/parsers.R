# A. ROSTER PARSE
# ===============

#' Load and parse NBA roster information
#' 
#' @description Parse sportVU XML file with NBA roster data into dataframe
#' @param file the file name of the XML document to be parsed
#' @return dataframe of NBA roster information
#' @export
#' @import XML
#' @keywords rosters, XML
#'
rosterParse <- function(file) {
  
  # parse the xml file
  rosters <- xmlParse(file)
  
  # count number of players on each team  
  roster1 = rosters["//nba-roster"]
  n = sum(names(getChildrenStrings(roster1[[1]])) != "nba-player")
  rosters_count = sapply(roster1, 
                         FUN = function(x) length(xmlChildren(x))) - n
  
  # create these now in advance of creating the whole data frame
  team_names = sapply(rosters["//nba-roster/team-name/@name"], as.character)
  team_id = sapply(rosters["//nba-roster/team-code/@id"], as.integer)
  
  # there are other variables that could be grabbed from this XML 
  # (rookie year, personal info, position details, weight/height, birth city, draft info)
  rosters_df <- data.frame(
    firstname = as.character(rosters["//nba-roster/nba-player/name/@first-name"]),
    lastname = as.character(rosters["//nba-roster/nba-player/name/@last-name"]),
    player = as.character(rosters["//nba-roster/nba-player/name/@display-name"]),
    jersey = as.integer(rosters["//nba-roster/nba-player/player-number/@number"]),
    position = as.character(rosters["//nba-roster/nba-player/player-position/@abbrev"]),
    primaryposition.id = as.integer(rosters["//nba-roster/nba-player/primary-position/@id"]),
    global.player.id = as.integer(rosters["//nba-roster/nba-player/player-code/@global-id"]),
    player.id = as.integer(rosters["//nba-roster/nba-player/player-code/@id"]),
    team.id = rep(team_id, rosters_count),
    team.name = as.character(rep(team_names, rosters_count)),
    stringsAsFactors = FALSE
  )
  
  rosters_df
}
# =====


# B. ONCOURT PARSE
# ===============

#' Load and parse NBA oncourt information
#' 
#' @description Parse sportVU XML file with oncourt data, where each unique combination
#' @description of 10 players appearing on the court in a single corresponds with an 
#' @description oncourt.id. Use oncourt.id and game.id to identify a unique combination 
#' @description of 10 players in a specific game. 
#' @param files a vector with the file names of the XML files to be parsed
#' @return dataframe of oncourt.id information
#' @export
#' @import XML
#' @keywords oncourt XML
#' 
oncourtParse <- function(files) {
  
  # initialize df for all games, loop over each game file passed
  oncourt_final = c()
  for(k in 1:length(files)) {
    
    # load and parse xml file 
    xml_url = files[k]
    oncourt = xmlParse(xml_url)
    
    # create dataframe of what we need but in wrong format
    id_player_map <- data.frame(
      oncourt.id = unlist(lapply(as.integer(oncourt["//nba-oncourt-players/oncourt/@id"]), rep, 10)),
      player.id = sapply(oncourt["//player/@id"], as.integer),
      stringsAsFactors = FALSE
    )
    
    # grab unique oncourt.ids, initialize dataframe that we do need
    unique_ids = unique(id_player_map$oncourt.id)
    oncourt_df = as.data.frame(matrix(nrow = length(unique_ids), ncol = 11))
    colnames(oncourt_df) = c("oncourt.id", "v1.id", "v2.id", "v3.id", "v4.id", "v5.id", "h1.id", "h2.id", "h3.id", "h4.id", "h5.id")
    oncourt_df$oncourt.id = unique_ids
    
    # nested for-loop the oncourt ids and players for each id, fill oncourt dataframe
    idx = 0
    for(i in 1:nrow(oncourt_df)) {
      for(j in 2:11) {
        idx = idx + 1
        oncourt_df[i,j] = id_player_map$player.id[idx]
      }
    }
    
    oncourt_df$game.id = as.integer(oncourt["//nba-oncourt-players/gamecode/@code"])
    oncourt_final = rbind(oncourt_final, oncourt_df)
  }
  
  oncourt_final
}
# =====


# C. PLAYBYPLAY PARSE
# ===================

#' Load and parse NBA playbyplay information
#' 
#' @description Parse sportVU XML file with playbyplay data, which includes data 
#' @description of all NBA plays (shots, rebounds, steals, time out, end of quarter,  
#' @description etc.) for each NBA game parsed. 
#' @param files a vector with the file names of the XML files to be parsed
#' @return dataframe of playbyplay information
#' @export
#' @import XML
#' @importFrom data.table rbindlist
#' @keywords playbyplay XML
#' 
playbyplayParse <- function(files) {
  
  # initialize df for all games, loop over each game file passed
  playbyplay_final = c()
  for(k in 1:length(files)) {
    
    # load and parse the xml file
    xml_url = files[k]
    playbyplay = xmlParse(xml_url)
    
    # playbyplay_df = sapply(playbyplay["//nba-playbyplay/play"], as.character) # NEED THIS TO PASS CRAN CHECK, BUT THEN FUNCTION BREAKS
    playbyplay_df = sapply(playbyplay["//nba-playbyplay/play"], as, 'character')
    playbyplay_df = lapply(playbyplay_df, function(x) {
      df <- as.data.frame.list(xmlToList(x), stringsAsFactors = FALSE)
      is.na(df) <- df == ""
      df
    })
    playbyplay_df = as.data.frame(rbindlist(playbyplay_df, fill = TRUE))
    
    # columns to convert to integers / numeric / logical types
    convert_ints = c("quarter", "oncourt.id", "time.minutes", "id", "global.player.id.1", 
                     "player.id.1", "display.id.1", "global.team.code.1", "team.code.1", 
                     "global.player.id.2", "player.id.2", "display.id.2", "global.team.code.2", 
                     "team.code.2", "global.player.id.3", "player.id.3", "display.id.3", 
                     "global.team.code.3", "team.code.3", "points.type", "event.id", 
                     "detail.id", "distance", "player.score", "player.fouls", "visitor.score", 
                     "home.score", "visitor.fouls", "home.fouls", "position.id")
    convert_nums = c("time.seconds", "x.shot.coord", "y.shot.coord")
    convert_logs = c("blocked", "fastbreak", "in.paint", "second.chance", "off.turnover")
    
    # convert said columns using two apply calls
    playbyplay_df[, names(playbyplay_df) %in% convert_ints] = apply(playbyplay_df[, names(playbyplay_df) %in% convert_ints], 
                                                                    2, function(x) as.integer(x))
    playbyplay_df[, names(playbyplay_df) %in% convert_nums] = apply(playbyplay_df[, names(playbyplay_df) %in% convert_nums], 
                                                                    2, function(x) as.numeric(x))
    playbyplay_df[, names(playbyplay_df) %in% convert_logs] = apply(playbyplay_df[, names(playbyplay_df) %in% convert_logs], 
                                                                    2, function(x) as.logical(x))
    
    playbyplay_df$game.id = as.integer(playbyplay["//nba-playbyplay/gamecode/@code"])
    playbyplay_final = rbind(playbyplay_final, playbyplay_df)
  }
  
  playbyplay_final
}
# =====


# D. MOMENTS PARSE
# ================

#' Load and parse NBA moments information
#' 
#' @description Parse sportVU XML file with moments data, which includes data 
#' @description of all NBA moments (passes, dribbles, shots, steals, etc.). Moments   
#' @description is a more granular version of the playbyplay parser, mainly due to the 
#' @description inclusion of passing and dribbling moments.
#' @param files a vector with the file names of the XML files to be parsed
#' @return dataframe of moments information
#' @export
#' @import XML
#' @keywords moments XML
#' 
momentsParse <- function(files) {
  
  # initialize df for all games, loop over each game file passed
  moments_final <- c()
  for(k in 1:length(files)) {
    
    # load and parse xml file 
    xml_url <- files[k]
    moments <- xmlParse(xml_url)
    
    # create the main moments DF with easy-to-grab column
    moments_df <- data.frame(
      event.id = as.integer(moments["//sequence-pbp/moment/@event-id"]),
      game.clock = as.numeric(moments["//sequence-pbp/moment/@game-clock"]),
      time = as.numeric(moments["//sequence-pbp/moment/@time"]),
      player.id = as.integer(moments["//sequence-pbp/moment/@player-id"]),
      global.player.id = as.integer(moments["//sequence-pbp/moment/@global-player-id"]),
      pbp.seq.number = as.integer(moments["//sequence-pbp/moment/@pbp-seq-number"]),
      shot.clock = as.numeric(moments["//sequence-pbp/moment/@shot-clock"]),
      stringsAsFactors = FALSE
    )
    
    # grab the quarter IDs 
    quarter.id = as.integer(moments["//sequence-pbp/@period"])
    counts = unlist(lapply(moments["//sequence-pbp"], FUN = function(x) length(XML::xmlChildren(x))))
    moments_df$quarter.id = rep(quarter.id, counts)
    
    # add game.id and rbind with final
    moments_df$game.id = as.integer(moments["//nba-boxscore/gamecode/@code"])
    moments_final = rbind(moments_final, moments_df)
  }
  
  moments_final
}
# =====


# E. BOXSCORES PARSE
# ==================

#' Load and parse NBA boxscore information
#' 
#' @description Parse sportVU XML file with player boxscores data, which includes traditional  
#' @description boxscore information (points, rebounds, assists, blocks, etc.) as well   
#' @description as other unique metrics captured by the sportVU data (seconds possessing 
#' @description ball, feet run, average speed, touches, passes made, etc.).
#' @param files a vector with the file names of the XML files to be parsed
#' @return dataframe of boxscore information
#' @export
#' @import XML
#' @keywords boxscores XML
#' 
boxscoresParse <- function(files) {
  
  # initialize df for all games, loop over each game file passed
  boxscores_final = c()
  for(k in 1:length(files)) {
    
    # load and parse the xml file
    xml_url = files[k]
    boxscores = xmlParse(xml_url)
    
    # create dataframe of what we need for this game
    boxscores_df <- data.frame(
      player.id = sapply(boxscores["//players/player/player-code/@id"], as.integer),
      seconds.played = sapply(boxscores["//players/player/total-seconds-tracked/@seconds"], as.integer),
      seconds.possessed = sapply(boxscores["//players/player/time-of-possession/@seconds"], as.integer),
      feet.run = sapply(boxscores["//players/player/distance-run/@feet"], as.integer),
      average.speed.mph = sapply(boxscores["//players/player/average-speed/@miles-per-hour"], as.numeric),
      touches = sapply(boxscores["//players/player/number-of-touches/@touches"], as.integer),
      passes.made = sapply(boxscores["//players/player/passes/@passes-made"],as.integer),
      passes.received = sapply(boxscores["//players/player/passes/@passes-received"],as.integer),
      PTS = sapply(boxscores["//players/player/points/@points"], as.integer),
      FGM = sapply(boxscores["//players/player/field-goals/@made"], as.integer),
      FGA = sapply(boxscores["//players/player/field-goals/@attempted"], as.integer),
      FTM = sapply(boxscores["//players/player/free-throws/@made"], as.integer),
      FTA = sapply(boxscores["//players/player/free-throws/@attempted"], as.integer),
      AST = sapply(boxscores["//players/player/assists/@assists"], as.integer),
      ORB = sapply(boxscores["//players/player/rebounds/@offensive"], as.integer),
      DRB = sapply(boxscores["//players/player/rebounds/@defensive"], as.integer),
      TRB = sapply(boxscores["//players/player/rebounds/@total"], as.integer),
      STL = sapply(boxscores["//players/player/steals/@steals"], as.integer),
      BLK = sapply(boxscores["//players/player/blocked-shots/@blocked-shots"], as.integer),
      TO = sapply(boxscores["//players/player/turnovers/@turnovers"], as.integer),
      PF = sapply(boxscores["//players/player/personal-fouls/@fouls"], as.integer),
      PM = sapply(boxscores["//players/player/plus-minus/@number"], as.integer),
      stringsAsFactors = FALSE
    )
    
    # add game id, add whole thing to a larger dataframe
    boxscores_df$game.id = as.integer(boxscores["//nba-boxscore/gamecode/@code"])
    boxscores_final = rbind(boxscores_final, boxscores_df)
  }
  
  boxscores_final  
}
# =====


# F. POSSESSIONS PARSE
# ====================

#' Load and parse NBA possessions information
#' 
#' @description Parse sportVU XML file with possession data, which includes data 
#' @description for all NBA possessions (points scored, length of possession, touches, 
#' @description passes, etc.). Less granular than both the playbyplay and moments
#' @description parsers.
#' @param files a vector with the file names of the XML files to be parsed
#' @return dataframe of possessions information
#' @export
#' @import dplyr
#' @import XML
#' @keywords possessions XML
#' 
possessionsParse <- function(files) {
  
  # initialize df for all games, loop over each game file passed
  possessions_final = c()
  for(k in 1:length(files)) {
    
    # load and parse the xml file
    xml_url <- files[k]
    possessions <- xmlParse(xml_url)
    
    # create the dataframe of what we need for this game
    possessions_df <- data.frame(
      team.alias = sapply(possessions["//possessions/quarter/possession/@team-alias"], as.character),
      team.global.id = sapply(possessions["//possessions/quarter/possession/@team-global-id"], as.integer),
      team.id = sapply(possessions["//possessions/quarter/possession/@team-id"], as.integer),
      time.start = sapply(possessions["//possessions/quarter/possession/@time-start"], as.character),
      time.end = sapply(possessions["//possessions/quarter/possession/@time-end"], as.character),
      start.timestamp = sapply(possessions["//possessions/quarter/possession/@start-timestamp"], as.numeric),
      end.timestamp = sapply(possessions["//possessions/quarter/possession/@end-timestamp"], as.numeric),
      possession.length = sapply(possessions["//possessions/quarter/possession/@possession-length"], as.character),
      touches = sapply(possessions["//possessions/quarter/possession/@touches"], as.integer),
      passes = sapply(possessions["//possessions/quarter/possession/@passes"], as.integer),
      driblles = sapply(possessions["//possessions/quarter/possession/@dribbles"], as.integer),
      # result = sapply(possessions["//possessions/quarter/possession/@result"], as.integer),
      result = sapply(possessions["//possessions/quarter/possession/@result"], as.character),
      points = sapply(possessions["//possessions/quarter/possession/@points"], as.integer),
      stringsAsFactors = FALSE
    )
    
    # grab quarter IDs, grab counts for each ID, add to the df
    quarter.id = as.integer(possessions["//possessions/quarter/@number"])
    counts = unlist(lapply(possessions["//possessions/quarter"], FUN = function(x) length(XML::xmlChildren(x))))
    possessions_df$quarter.id = rep(quarter.id, counts)
    
    # remove time stamps of -1
    possessions_df <- possessions_df[possessions_df$start.timestamp != -1 & possessions_df$end.timestamp != -1, ]
    
    # add game.id and rbind with final
    possessions_df$game.id = as.integer(possessions["//nba-boxscore/gamecode/@code"])
    possessions_final = rbind(possessions_final, possessions_df)
  }
  
  possessions_final
}
# =====


# G. SPORTVU PARSE
# ================

#' Load and parse NBA sportVU information
#' 
#' @description Parse sportVU XML file with moments data, which includes motion tracking 
#' @description coordinates of all 10 players as well as the ball, each 25x per second.   
#' @description This is the most granular parser due to the high frequency of coordinate 
#' @description data.
#' @param files a vector with the file names of the XML files to be parsed
#' @return dataframe of sportVU information
#' @export
#' @import XML
#' @import dplyr
#' @importFrom utils tail
#' @keywords moments XML
#' 
sportvuParse <- function(files) {
  
  # initialize df for all games, loop over each game file passed
  sportvu_final = c()
  for(k in 1:length(files)) {
    
    # load and parse the xml file
    xml_url = files[k]
    sportvu = xmlParse(xml_url)
    
    # quick load the data into a dataframe - 10 secs for test data to load
    sportvu_df <- data.frame(
      game_clock = as.numeric(sportvu["//sequences/moment/@game-clock"]),
      time = as.character(sportvu["//sequences/moment/@time"]),
      game_event_id = as.integer(sportvu["//sequences/moment/@game-event-id"]),
      shot_clock = as.numeric(sportvu["//sequences/moment/@shot-clock"]), 
      locations = as.character(sportvu["//sequences/moment/@locations"])
    )
    
    # convert locations into what we want, many more columns 
    locations = as.character(sportvu_df$locations)
    locations = strsplit(gsub(";", ",", locations), ",")
    reps = sapply(locations, length) / 5
    locations = as.data.frame(matrix(unlist(locations), ncol = 5, byrow = TRUE), stringsAsFactors = FALSE)
    colnames(locations) = c("team_id", "global.player.id", "x_loc", "y_loc", "radius")
    
    locations$global.player.id = as.integer(locations$global.player.id); locations$team_id = as.integer(locations$team_id); locations$x_loc = as.numeric(locations$x_loc); locations$y_loc = as.numeric(locations$y_loc); locations$radius = as.numeric(locations$radius)
    
    # connect locations back with sportvu_df
    sportvu_df = sportvu_df[!(names(sportvu_df) %in% "locations")]
    sportvu_df = sportvu_df[rep(row.names(sportvu_df), reps), ]
    sportvu_df = cbind(sportvu_df, locations)
    sportvu_df$order = seq(1,nrow(sportvu_df),by=1)
    
    # remove entries from sportvu when the ball is dead
    sportvu_df = as.data.frame(
      sportvu_df %>%
        group_by(game_clock) %>%
        slice(tail(1:n(), 11))
    )
    
    # set shot clock NA's to 0, resort the first time
    sportvu_df = sportvu_df[order(sportvu_df$order), ]
    sportvu_df$shot_clock[is.na(sportvu_df$shot_clock)] = 0 
    
    # add the game.id, quarter.id and rbind with final
    sportvu_df$quarter.id = as.integer(sportvu["//sequences/@period"])
    sportvu_df$game.id = as.integer(sportvu["//nba-boxscore/gamecode/@code"])
    sportvu_final = rbind(sportvu_final, sportvu_df)
  }
  
  sportvu_final$order = seq(1,nrow(sportvu_final),by=1)
  sportvu_final  
}
# =====


# H. SCHEDULE PARSE
# =================

#' Load and parse NBA schedule information
#' 
#' @description Parse sportVU XML file with schedule data, which includes data 
#' @description on an NBA season's schedule and game outcomes (teams playing, score, date, winner)  
#' @param file the file name of the XML document to be parsed
#' @return dataframe of schedule information
#' @export
#' @import XML
#' @keywords schedule XML
#' 
scheduleParse <- function(file) {
  
  # load and parse the xml file
  schedule <- xmlParse(file)
  
  # define xpath2 function that fills in NA when required
  xpath2 <-function(x, ...){
    y <- xpathSApply(x, ...)
    ifelse(length(y) == 0, NA,  paste(y, collapse=", "))
  }
  nd <- getNodeSet(schedule, "//game-schedule")   
  
  # create the dataframe that we need
  schedule_df <- data.frame(
    visitor.team = sapply(nd, xpath2, ".//visiting-team/team-name", xmlGetAttr, "name"),
    visitor.alias = sapply(nd, xpath2, ".//visiting-team/team-name", xmlGetAttr, "alias"),
    visitor.points = sapply(nd, xpath2, ".//visiting-team-score", xmlGetAttr, "score"),
    visitor.outcome = sapply(nd, xpath2, ".//outcome-visit", xmlGetAttr, "outcome"),
    home.team = sapply(nd, xpath2, ".//home-team/team-name", xmlGetAttr, "name"),
    home.alias = sapply(nd, xpath2, ".//home-team/team-name", xmlGetAttr, "alias"),
    home.points = sapply(nd, xpath2, ".//home-team-score", xmlGetAttr, "score"),
    home.outcome = sapply(nd, xpath2, ".//outcome-home", xmlGetAttr, "outcome"),
    quarters = sapply(nd, xpath2, ".//total-quarters", xmlGetAttr, "total"),
    year = sapply(nd, xpath2, ".//date", xmlGetAttr, "year"), 
    month = sapply(nd, xpath2, ".//date", xmlGetAttr, "month"),
    day = sapply(nd, xpath2, ".//date", xmlGetAttr, "date")
  )
  
  schedule_df
}
# ===== 


# I. SHOTS PARSE
# ==============

#' Load and parse NBA shot information
#' 
#' @description Parse sportVU XML file with NBA shot data, which includes data 
#' @description of all NBA shots (coordinates, make/miss, dribbles, etc.). To   
#' @description be used for shot-chart application.
#' @param files a vector with the file names of the XML files to be parsed
#' @return dataframe of shot information
#' @export
#' @import XML
#' @keywords shots XML
#' 
shotsParse <- function(files) {
  
  # initialize df for all games, loop over each game file passed
  shots_final = c()
  for(k in 1:length(files)) {
    
    # load and parse the xml file
    xml_url = files[k]
    shots = xmlParse(xml_url)
    
    # xpath2 used to deal with missing child nodes
    xpath2 <-function(x, ...){
      y <- xpathSApply(x, ...)
      ifelse(length(y) == 0, NA,  paste(y, collapse=", "))
    }
    
    # grab the player.ids and global.player.ids for players in game, along with shot.logs (NAs for no shots in shot.log)
    player_nodes = getNodeSet(shots, "//player")
    player.id = sapply(player_nodes, xpath2, ".//player-code", xmlGetAttr, "id")
    global.player.id = sapply(player_nodes, xpath2, ".//player-code", xmlGetAttr, "global-id")
    shot.log = sapply(player_nodes, xpath2, ".//shot-log")
    
    # use NAs in shot.log to index out those players that didn't shoot
    player.id = as.integer(player.id[!is.na(shot.log)])
    global.player.id = as.integer(global.player.id[!is.na(shot.log)])
    
    # grab information for all shots taken in the game
    shot_attrs = getNodeSet(shots, "//players/player/shot-log/shot")  
    shot_attrs = lapply(shot_attrs, FUN = function(x) xmlAttrs(x))
    shot_attrs_df = data.frame(matrix(unlist(shot_attrs), nrow=length(shot_attrs), byrow=T),
                               stringsAsFactors = FALSE)
    names(shot_attrs_df) = names(shot_attrs[[1]])
    
    # grab shots per player, by applying xmlChildren over all shot-log nodes
    shot_log_nodes = getNodeSet(shots, "//players/player/shot-log") 
    nr_shots_per_player = sapply(shot_log_nodes, 
                                 FUN = function(x) length(xmlChildren(x))) 
    
    # combine everything into final dataframe
    shots_df = data.frame(
      player.id = rep(player.id, nr_shots_per_player),
      global.player.id = rep(global.player.id, nr_shots_per_player),
      shot_attrs_df
    )
    
    # convert ints to ints, numerics to numerics, etc., change col names to x_loc, y_loc 
    convert_ints = c("period", "points.type", "dribbles")
    convert_nums = c("touch.time", "shot.distance", "x.coordinate", "y.coordinate")
    
    shots_df[, names(shots_df) %in% convert_ints] = apply(shots_df[, names(shots_df) %in% convert_ints], 
                                                          2, function(x) as.integer(x))
    shots_df[, names(shots_df) %in% convert_nums] = apply(shots_df[, names(shots_df) %in% convert_nums], 
                                                          2, function(x) as.numeric(x))
    colnames(shots_df)[colnames(shots_df) == "x.coordinate"] = "x_loc"
    colnames(shots_df)[colnames(shots_df) == "y.coordinate"] = "y_loc"
    
    # add game id, add whole thing to a larger dataframe
    shots_df$game.id = as.integer(shots["//nba-boxscore/gamecode/@code"])
    shots_final = rbind(shots_final, shots_df)
  }
  
  shots_final  
}
# =====


# J. MASTER PARSE
# ===============

#' Load and parse all NBA information
#' 
#' @description Parse all sportVU XML files in directory passed 
#' @param directory a directory path with XML files
#' @return list of dataframes of data
#' @export
#' @import XML
#' @keywords sportVU XML
#' 
masterParse <- function(directory) {
  
  # A. grab all file names, initialize list of file types
  # =====================================================
  # grab filenames, organize into list
  files <- list.files(directory)
  files_list <- list(NBA_ALL_ROSTER = c(), 
                     NBA_FINAL_ONCOURT = c(), 
                     NBA_FINAL_SEQUENCE_OPTICAL = c(),
                     NBA_FINAL_SEQUENCE_PBP_OPTICAL = c(), 
                     NBA_FINALBOX_OPTICAL = c(), 
                     NBA_FINALPBP_EXP = c(),
                     NBA_SCHEDULE = c())
  
  # allocate file types the their respective list
  files_list$NBA_ALL_ROSTER <- c(files[intersect(grep("NBA_ALL_ROSTER", files), 
                                                 grep("XML", files))])
  
  files_list$NBA_FINAL_ONCOURT <- c(files[intersect(grep("NBA_FINAL_ONCOURT", files), 
                                                    grep("XML", files))])
  
  files_list$NBA_FINAL_SEQUENCE_OPTICAL <- c(files[intersect(grep("NBA_FINAL_SEQUENCE_OPTICAL", files), 
                                                             grep("XML", files))])
  
  files_list$NBA_FINAL_SEQUENCE_PBP_OPTICAL <- c(files[intersect(grep("NBA_FINAL_SEQUENCE_PBP_OPTICAL", files), 
                                                                 grep("XML", files))])
  
  files_list$NBA_FINALBOX_OPTICAL <- c(files[intersect(grep("NBA_FINALBOX_OPTICAL", files), 
                                                       grep("XML", files))])
  
  files_list$NBA_FINALPBP_EXP <- c(files[intersect(grep("NBA_FINALPBP_EXP", files), 
                                                   grep("XML", files))])
  
  files_list$NBA_SCHEDULE <- c(files[intersect(grep("NBA_SCHEDULE", files), 
                                               grep("XML", files))])
  
  
  # rename OT to TO for sorting (OT after Q4), then sort, then convert back after sorting
  files_list$NBA_FINAL_SEQUENCE_OPTICAL <- gsub("OT", "TO", files_list$NBA_FINAL_SEQUENCE_OPTICAL)
  files_list <- lapply(files_list, FUN = sort)
  files_list$NBA_FINAL_SEQUENCE_OPTICAL <- gsub("TO", "OT", files_list$NBA_FINAL_SEQUENCE_OPTICAL)
  
  
  # B. grab all file names, initialize list of file types
  # =====================================================
  # initialize list that will store parsed dataframes
  parsed_list <- list(rosters = c(), 
                      oncourt = c(), 
                      playbyplay = c(),
                      moments = c(), 
                      boxscores = c(), 
                      possessions = c(),
                      sportvu = c(),
                      schedule = c(),
                      shots = c())
  
  # and start parsing
  roster_files <- paste(directory, files_list$NBA_ALL_ROSTER, sep = '')
  parsed_list$rosters <- rosterParse(roster_files)  
  
  oncourt_files <- paste(rep(directory, length(files_list$NBA_FINAL_ONCOURT)), files_list$NBA_FINAL_ONCOURT, sep = '')
  parsed_list$oncourt <- oncourtParse(oncourt_files)
  
  # here
  playbyplay_files <- paste(rep(directory, length(files_list$NBA_FINALPBP_EXP)), files_list$NBA_FINALPBP_EXP, sep = '')
  parsed_list$playbyplay <- playbyplayParse(playbyplay_files)
  
  moments_files <- paste(rep(directory, length(files_list$NBA_FINAL_SEQUENCE_PBP_OPTICAL)), files_list$NBA_FINAL_SEQUENCE_PBP_OPTICAL, sep = '')
  parsed_list$moments <- momentsParse(moments_files)
  
  boxscores_files <- paste(rep(directory, length(files_list$NBA_FINALBOX_OPTICAL)), files_list$NBA_FINALBOX_OPTICAL, sep = '')
  parsed_list$boxscores <- boxscoresParse(boxscores_files)
  
  # here
  possessions_files <- paste(rep(directory, length(files_list$NBA_FINALBOX_OPTICAL)), files_list$NBA_FINALBOX_OPTICAL, sep = '')
  parsed_list$possessions <- possessionsParse(possessions_files)
  
  sportvu_files <- paste(rep(directory, length(files_list$NBA_FINAL_SEQUENCE_OPTICAL)), files_list$NBA_FINAL_SEQUENCE_OPTICAL, sep = '')
  parsed_list$sportvu <- sportvuParse(sportvu_files)
  
  schedule_files <- paste(rep(directory, length(files_list$NBA_SCHEDULE)), files_list$NBA_SCHEDULE, sep = '')
  parsed_list$schedule <- scheduleParse(schedule_files)
  
  shots_files <- paste(rep(directory, length(files_list$NBA_FINALBOX_OPTICAL)), files_list$NBA_FINALBOX_OPTICAL, sep = '')
  parsed_list$shots <- shotsParse(shots_files)
  
  return(parsed_list)
}
# =====

