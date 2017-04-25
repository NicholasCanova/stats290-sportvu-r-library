
# reset, set WD, load libaries
# ============================
rm(list = ls())
setwd("/Users/Home/Dropbox/My Documents/Stanford University/2016-17 Winter Quarter/STATS 290 Paradigms for Computing/Stats290 Project/")
library(devtools); library(readr)
#library(spoRtvu)
#library(sportvuR)

# create pkgName, document, install and load
# ==========================================
pkgName  <- "sportVU"
devtools::document(pkg = pkgName)
devtools::install(pkg = pkgName)
library(sportVU)


# single parser of data needed for app
# ====================================
rosters = sportVU::rosterParse(file = 'Datas/NBA_SPORTSVU_FILES/NBA_ALL_ROSTER.XML')
head(rosters, n = 10)

shot_files = c("Datas/NBA_SPORTSVU_FILES/NBA_FINALBOX_OPTICAL$2016012523.XML", 
               "Datas/NBA_SPORTSVU_FILES/NBA_FINALBOX_OPTICAL$2016013113.XML", 
               "Datas/NBA_SPORTSVU_FILES/NBA_FINALBOX_OPTICAL$2016020323.XML", 
               "Datas/NBA_SPORTSVU_FILES/NBA_FINALBOX_OPTICAL$2016020605.XML", 
               "Datas/NBA_SPORTSVU_FILES/NBA_FINALBOX_OPTICAL$2016021022.XML") 
shots = sportVU::shotsParse(files = shot_files)
head(shots, n = 10)


movement_files = c("Datas/NBA_SPORTSVU_FILES/NBA_FINAL_SEQUENCE_OPTICAL$2016020605_Q1.XML", 
                   "Datas/NBA_SPORTSVU_FILES/NBA_FINAL_SEQUENCE_OPTICAL$2016020605_Q2.XML", 
                   "Datas/NBA_SPORTSVU_FILES/NBA_FINAL_SEQUENCE_OPTICAL$2016020605_Q3.XML", 
                   "Datas/NBA_SPORTSVU_FILES/NBA_FINAL_SEQUENCE_OPTICAL$2016020605_Q4.XML")
movements = sportVU::sportvuParse(files = movement_files)
head(movements, n = 10)

allData = masterParse(directory = 'Datas/NBA_SPORTSVU_SAMPLE_FILE/')


# launch shot and movement charts
# ===============================
sportVU::setShots(shots)
sportVU::setSportVU(movements)
sportVU::setRosters(rosters)


# launch the app
sportVU::runShotChart()

 