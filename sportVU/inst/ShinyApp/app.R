library(shiny);
library(shinythemes);
library(dplyr)  


# 1. Pre-Processing
# =================

# 1.A grab the sportvu and shots dataframes from the global variable list
shiny.dataframes <- getShinyDataframes()
sportvu <- shiny.dataframes$sportvu
shots <- shiny.dataframes$shots
rosters <- shiny.dataframes$rosters


# 1.B merge rosters onto sportvu, shots dataframes (for team and player names)
sportvu <- merge(sportvu, rosters[, names(rosters) %in% c("global.player.id", "player", "team.name")],
                 by = "global.player.id")
sportvu <- sportvu[order(sportvu$order), ]

shots <- merge(shots, rosters[, names(rosters) %in% c("global.player.id", "player", "team.name")],
               by = "global.player.id", all.x = TRUE)


# 1.C convert coordinates to half-court for shotchart
shots <- halfcourtData(shots)


# 1.D load the selection obects for the selectInputs
selects1 <- selectionCreatorShots(shots)
selects2 <- selectionCreatorMovements(sportvu)



# 2. UI layout
# ============
ui <- fluidPage(theme = shinytheme('simplex'),
                
                # 2.A Create Shiny App Title 
                # ==========================
                fluidRow(
                  column(width = 12, align = 'center',
                         h2('NBA Shot Chart and Movement Tracking Application'))
                ),
                
                fluidRow(
                  
                  
                  # 2.B Create All Widgets For App
                  # ==============================
                  column(width = 4, align = 'center', 
                         # header, and selectInput debugger
                         h3('Shot Chart Parameters'), hr(),
                         tags$head(tags$style(HTML(".shiny-split-layout > div {
                                                   overflow: visible; } "))),
                         
                         # select inputs for shot chart row 1
                         splitLayout(
                           selectInput(inputId = 'shooter.input', label = 'Select Shooter:', multiple = FALSE, 
                                       choices = selects1$shooter.selection, selected = 'All Players'),
                           selectInput(inputId = 'period.input', label = 'Select Quarter:', multiple = FALSE, 
                                       choices = selects1$period.selection, selected = 'All Periods')
                         ),
                         
                         # select inputs for shot chart row 2
                         splitLayout(
                           selectInput(inputId = 'team.input', label = 'Select Team:', multiple = FALSE, 
                                       choices = selects1$team.selection, selected = 'All Teams'),
                           selectInput(inputId = 'dribble.input', label = 'Select Shot Type:', multiple = FALSE, 
                                       choices = selects1$dribble.selection, selected = 'All Shots')
                         ),
                         
                         # select inputs for movement chart
                         h3('Movement Chart Parameters'), hr(),
                         splitLayout(
                           selectInput(inputId = 'runner.input', label = 'Select Shooter:', multiple = FALSE, 
                                       choices = selects2$shooter.selection, selected = 'No Player Selected'),
                           selectInput(inputId = 'game.input', label = 'Select GameID:', multiple = FALSE, 
                                       choices = selects2$game.id.selection, selected = 'No Game Selected')
                         ),
                         
                         # action buttons to toggle between the two charts 
                         hr(),
                         splitLayout(actionButton(inputId = 'toggleshotchart', label = 'Launch Shots'), 
                                     actionButton(inputId = 'togglemovement', label = 'Launch Movements')),
                         
                         # title for player summary table, and the table itself
                         br(), h3('Player Summary Tables'),
                         hr(), tableOutput('shot.table')
                  ), 
                  
                  
                  # 2.C Launch the Chart (note wrapped in uiOutput)
                  # ===============================================
                  column(width = 8, align = 'center',
                         br(),
                         uiOutput("plot_ui")
                  )
                )
)



# 3. Server Layout
# ================
server <- shinyServer(function(input, output) {
  
  # 3.A create reactive value plotData$value to toggle between charts
  # =================================================================
  plotData <- reactiveValues(value = NULL)
  
  observeEvent(input$toggleshotchart, {
    plotData$value <- 0
  })
  
  observeEvent(input$togglemovement, {
    plotData$value <- 1
  })
  
  
  # 3.B Update input parameters each time an action button is clicked
  # =================================================================
  period <- eventReactive(input$toggleshotchart, {input$period.input})
  player <- eventReactive(input$toggleshotchart, {input$shooter.input})
  team <- eventReactive(input$toggleshotchart, {input$team.input})
  dribbles <- eventReactive(input$toggleshotchart, {input$dribble.input})
  
  mov_player <- eventReactive(input$togglemovement, {input$runner.input})
  mov_gameid <- eventReactive(input$togglemovement, {input$game.input})
  
  
  # 3.C the renderPlot - contains code for both charts
  # ==================================================
  output$plot <- renderPlot({
    
    # initialize launching of app with a blank shot chart
    if (is.null(plotData$value)) {
      plot_title = "Let's Get Started - Select Inputs and Choose your Chart Type"
      createBlank_motionChart(plot_title)
    }
    
    # display shot chart if shot chart button is clicked
    else if(plotData$value == 0) {
      shot.data <- shots
      
      # Filter the shot data according to the user inputs
      # 1. Player  # 2. Team  # 3. Quarter  # 4. Period 
      if(player() != "All Players"){
        player.data = filter(shot.data, player == player())
        plot.title = paste(player(), 'Shot Chart\n', sep = ' ')
      }
      if(player() == "All Players" & team() == "All Teams"){
        player.data = shot.data
        plot.title = paste('All Players and All Teams', 'Shot Chart\n', sep = ' ')
      }
      if(player() == "All Players" & team() != "All Teams"){
        player.data = filter(shot.data, team.name == team())
        plot.title = paste('Team',team(), 'Shot Chart\n',sep = ' ')
      }
      if(period() != "All Periods"){
        player.data = filter(player.data, period == period())
      } 
      if(dribbles() != "All Shots"){
        player.data = filter(player.data, dribbles == dribbles())
      }
      
      
      # 3.D the shot chart code
      # =======================
      # If player.data filtered to 0 rows, display empty court
      if (nrow(player.data) == 0) {
        plot_title = "No shot data for this selection - please try generalizing your selection inputs"
        createBlank_ShotChart(plot_title)
      } 
      
      # otherwise display the shotchart
      else {
        chart.data = generateChart_Data(player.data)
        createShot_Chart(chart.data$x, chart.data$y, chart.data$player.makes, chart.data$player.attempts, plot.title)
      }
    }
    
    
    # 3.E the movement chart code
    # ===========================
    # display shot chart if shot chart button is clicked
    else {
      this_player = mov_player()
      this_gameID = mov_gameid()
      
      # display blank court if player and gameID not selected
      if(this_player == "No Player Selected" | this_gameID == "No Game Selected") {
        plot_title = 'Please Select A Player and GameID from the Selection Inputs'
        createBlank_motionChart(plot_title)
      } 
      
      # for player / gameID combo 
      else {
        plot_data = sportvu[sportvu$player == this_player & sportvu$game.id == this_gameID, ]
        
        # if data does not exist for player / gameID combo, display blank court
        if(nrow(plot_data) == 0) {
          plot_title = paste("No data available for this player / gameID combination")
          createBlank_motionChart(plot_title)
        }
        
        # otherwise, display movement chart
        else {
          plot_title = paste(this_player, 'Motion Chart for', this_gameID, sep = ' ')
          createMotion_Chart(plot_title, plot_data$x_loc, plot_data$y_loc)
        }
      }
    }
  })
  
  
  # 3.F the plot_ui toggles between charts
  # ======================================
  output$plot_ui <- renderUI({
    if(is.null(plotData$value)) {
      plot.width = 1128
      plot.height = 650
    } else if(plotData$value == 0) {
      plot.width = 900
      plot.height = 846
    } else {
      plot.width = 1128
      plot.height = 650
    }
    plotOutput('plot', width = plot.width, height = plot.height)
  })
  
  
  # 3.G for each chart, a supporting table
  # ====================================== 
  # For each chart, a supporting table
  output$shot.table <- renderTable({
    
    if (is.null(plotData$value)) {
      empty_df <- data.frame(matrix(ncol = 6, nrow = 4))
      colnames(empty_df) <- c('Qtr', 'Time Played (min)', 'Avg. Speed (mph)', 'Top Speed (mph)', 'Miles Run', 'Miles Run per 36 Min.')
      empty_df$Qtr <- c(1,2,3,4)
      return(empty_df)
    }
    else if (plotData$value == 1) {
      if (mov_player() != 'No Player Selected') {
        sportvu <- sportvu[sportvu$player == mov_player(), ]
      } else {
        empty_df <- data.frame(matrix(ncol = 6, nrow = 4))
        colnames(empty_df) <- c('Qtr', 'Time Played (min)', 'Avg. Speed (mph)', 'Top Speed (mph)', 'Miles Run', 'Miles Run per 36 Min.')
        empty_df$Qtr <- c(1,2,3,4)
        return(empty_df)
      }
      
      if (mov_gameid() != 'No Game Selected') {
        sportvu <- sportvu[sportvu$game.id == mov_gameid(), ]
      }
      
      # createVelocityTable(sportvu, input$runner.input, input$game.input)
      createVelocityTable(sportvu)
    }
    
    else {
      if (player() != 'All Players') {
        shots <- shots[shots$player == player(), ]
      } else if (team() != 'All Teams') {
        shots <- shots[shots$team.name == team(), ]
      }
      
      if (dribbles() != 'All Shots') {
        shots <- shots[shots$dribbles == dribbles(), ]
      }
      
      createShotTable(shots)
    }
  }, digits = 0, align = 'c', width = 2)
})

shinyApp(ui = ui, server = server)
