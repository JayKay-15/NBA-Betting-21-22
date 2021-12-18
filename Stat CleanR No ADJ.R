######## Stat CleanR - No ADJ ########

### Creates adjusted year to date stats for score and win predictions

if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, readxl, lubridate, openxlsx, nbastatR, rvest)

rm(list=ls())
setwd("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/")

# cur_date <- "3_15_2021"

### Pull game logs & arrange by date

game_logs(seasons = 2022, result_types = c("team","players"))

dataGameLogsTeam <- dataGameLogsTeam %>% arrange(dateGame,idGame)
dataGameLogsTeam$dateGame <- as_date(dataGameLogsTeam$dateGame)
dataGameLogsPlayer <- dataGameLogsPlayer %>% arrange(dateGame,idGame)

### Attach game logs to itself to get all stats for each game in one row

gl <- left_join(dataGameLogsTeam, dataGameLogsTeam, by = c("idGame" = "idGame", "slugTeam" = "slugOpponent"))

gl <- gl %>%
    select(13,8,17,62,7,45,90,34,79,
           24,25,27,28,35,36,37,38,39,40,43,41,42,44,
           69,70,72,73,80,81,82,83,84,85,88,86,87,89)

colnames(gl) <- c("Date", "teamLoc", "teamName", "opptName", "teamRslt", 
                  "teamPTS", "opptPTS", "teamMin", "opptMin", 
                  "teamFGM", "teamFGA", "team3PM", "team3PA", "teamFTM",
                  "teamFTA", "teamORB", "teamDRB", "teamTRB", "teamAST",
                  "teamTOV", "teamSTL", "teamBLK", "teamPF", 
                  "opptFGM", "opptFGA", "oppt3PM", "oppt3PA", "opptFTM", 
                  "opptFTA", "opptORB", "opptDRB", "opptTRB", "opptAST", 
                  "opptTOV", "opptSTL", "opptBLK", "opptPF")

# Filter for home/away 

home <- gl %>%
    filter(teamLoc == "H")

away <- gl %>%
    filter(teamLoc == "A")

##### Games count - Season #####

gl <- gl %>%
    add_count(teamName, name = "teamGameCount") %>%
    add_count(opptName, name = "opptGameCount")

##### Games count - Away #####

away <- away %>%
    add_count(teamName, name = "teamGameCount") %>%
    add_count(opptName, name = "opptGameCount")

##### Games count - Home #####

home <- home %>%
    add_count(teamName, name = "teamGameCount") %>%
    add_count(opptName, name = "opptGameCount")

##### SEASON TOTALS #####

season_grouped <- gl %>%
    select(3,6:38) %>%
    group_by(teamName,teamGameCount) %>%
    summarise(across(c(teamPTS:opptPF), sum))

##### SEASON ADVANCED STATS #####

season_adv <- season_grouped

season_adv$Poss <- with(season_adv, teamFGA - teamORB + teamTOV + (.44 * teamFTA))
season_adv$oPoss <- with(season_adv, opptFGA - opptORB + opptTOV + (.44 * opptFTA))
season_adv$Pace <- with(season_adv, 48 * (Poss + oPoss) / (2 * (teamMin/5)))
season_adv$oPace <- with(season_adv, 48 * (Poss + oPoss) / (2 * (opptMin/5)))
season_adv$ORtg <- with(season_adv, (teamPTS / Poss) * 100)
season_adv$DRtg <- with(season_adv, (opptPTS / oPoss) * 100)

season_adv$FG <- with(season_adv, teamFGM / teamFGA)
season_adv$SR2 <- with(season_adv, (teamFGA - team3PA) / teamFGA)
season_adv$FG3 <- with(season_adv, team3PM / team3PA)
season_adv$SR3 <- with(season_adv, team3PA / teamFGA)
season_adv$FT <- with(season_adv, teamFTM / teamFTA)
season_adv$FTR <- with(season_adv, teamFTM / teamFGA)
season_adv$ORB <- with(season_adv, teamORB / (teamORB + opptDRB))
season_adv$DRB <- with(season_adv, teamDRB / (teamDRB + opptORB))
season_adv$TRB <- with(season_adv, teamTRB / (teamTRB + opptTRB))
season_adv$AST <- with(season_adv, teamAST / teamFGM)
season_adv$TOV <- with(season_adv, teamTOV / Poss)
season_adv$STL <- with(season_adv, teamSTL / oPoss)
season_adv$BLK <- with(season_adv, teamBLK / (opptFGA - oppt3PA))
season_adv$PF <- with(season_adv, teamPF / oPoss)
season_adv$eFG <- with(season_adv, (teamFGM + .5 * team3PM) / teamFGA)
season_adv$TS <- with(season_adv, teamPTS / (2 * teamFGA + .44 * teamFTA))

season_adv$oFG <- with(season_adv, opptFGM / opptFGA)
season_adv$oSR2 <- with(season_adv, (opptFGA - oppt3PA) / opptFGA)
season_adv$oFG3 <- with(season_adv, oppt3PM / oppt3PA)
season_adv$oSR3 <- with(season_adv, oppt3PA / opptFGA)
season_adv$oFT <- with(season_adv, opptFTM / opptFTA)
season_adv$oFTR <- with(season_adv, opptFTM / opptFGA)
season_adv$oORB <- with(season_adv, opptORB / (opptORB + teamDRB))
season_adv$oDRB <- with(season_adv, opptDRB / (opptDRB + teamORB))
season_adv$oTRB <- with(season_adv, opptTRB / (teamTRB + opptTRB))
season_adv$oAST <- with(season_adv, opptAST / opptFGM)
season_adv$oTOV <- with(season_adv, opptTOV / oPoss)
season_adv$oSTL <- with(season_adv, opptSTL / Poss)
season_adv$oBLK <- with(season_adv, opptBLK / (teamFGA - team3PA))
season_adv$oPF <- with(season_adv, opptPF / Poss)
season_adv$oeFG <- with(season_adv, (opptFGM + .5 * oppt3PM) / opptFGA)
season_adv$oTS <- with(season_adv, opptPTS / (2 * opptFGA + .44 * opptFTA))

season_final <- season_adv %>%
    select(1,41:72,39,40,37)

colnames(season_final) <- c("Team","FG","SR2","FG3","SR3","FT","FTR","ORB","DRB","TRB",
                          "AST","TOV","STL","BLK","PF","eFG","TS",
                          "oFG","oSR2","oFG3","oSR3","oFT","oFTR","oORB","oDRB","oTRB",
                          "oAST","oTOV","oSTL","oBLK","oPF","oeFG","oTS",
                          "ORtg","DRtg","Pace")

### GROUPING HOME GAMES 

home_grouped <- home %>%
    select(3,6:38) %>%
    group_by(teamName,teamGameCount) %>%
    summarise(across(c(teamPTS:opptPF), sum))

###### HOME ADVANCED STATS ######

home_adv <- home_grouped

home_adv$Poss <- with(home_adv, teamFGA - teamORB + teamTOV + (.44 * teamFTA))
home_adv$oPoss <- with(home_adv, opptFGA - opptORB + opptTOV + (.44 * opptFTA))
home_adv$Pace <- with(home_adv, 48 * (Poss + oPoss) / (2 * (teamMin/5)))
home_adv$oPace <- with(home_adv, 48 * (Poss + oPoss) / (2 * (opptMin/5)))
home_adv$ORtg <- with(home_adv, (teamPTS / Poss) * 100)
home_adv$DRtg <- with(home_adv, (opptPTS / oPoss) * 100)

home_adv$FG <- with(home_adv, teamFGM / teamFGA)
home_adv$SR2 <- with(home_adv, (teamFGA - team3PA) / teamFGA)
home_adv$FG3 <- with(home_adv, team3PM / team3PA)
home_adv$SR3 <- with(home_adv, team3PA / teamFGA)
home_adv$FT <- with(home_adv, teamFTM / teamFTA)
home_adv$FTR <- with(home_adv, teamFTM / teamFGA)
home_adv$ORB <- with(home_adv, teamORB / (teamORB + opptDRB))
home_adv$DRB <- with(home_adv, teamDRB / (teamDRB + opptORB))
home_adv$TRB <- with(home_adv, teamTRB / (teamTRB + opptTRB))
home_adv$AST <- with(home_adv, teamAST / teamFGM)
home_adv$TOV <- with(home_adv, teamTOV / Poss)
home_adv$STL <- with(home_adv, teamSTL / oPoss)
home_adv$BLK <- with(home_adv, teamBLK / (opptFGA - oppt3PA))
home_adv$PF <- with(home_adv, teamPF / oPoss)
home_adv$eFG <- with(home_adv, (teamFGM + .5 * team3PM) / teamFGA)
home_adv$TS <- with(home_adv, teamPTS / (2 * teamFGA + .44 * teamFTA))

home_adv$oFG <- with(home_adv, opptFGM / opptFGA)
home_adv$oSR2 <- with(home_adv, (opptFGA - oppt3PA) / opptFGA)
home_adv$oFG3 <- with(home_adv, oppt3PM / oppt3PA)
home_adv$oSR3 <- with(home_adv, oppt3PA / opptFGA)
home_adv$oFT <- with(home_adv, opptFTM / opptFTA)
home_adv$oFTR <- with(home_adv, opptFTM / opptFGA)
home_adv$oORB <- with(home_adv, opptORB / (opptORB + teamDRB))
home_adv$oDRB <- with(home_adv, opptDRB / (opptDRB + teamORB))
home_adv$oTRB <- with(home_adv, opptTRB / (teamTRB + opptTRB))
home_adv$oAST <- with(home_adv, opptAST / opptFGM)
home_adv$oTOV <- with(home_adv, opptTOV / oPoss)
home_adv$oSTL <- with(home_adv, opptSTL / Poss)
home_adv$oBLK <- with(home_adv, opptBLK / (teamFGA - team3PA))
home_adv$oPF <- with(home_adv, opptPF / Poss)
home_adv$oeFG <- with(home_adv, (opptFGM + .5 * oppt3PM) / opptFGA)
home_adv$oTS <- with(home_adv, opptPTS / (2 * opptFGA + .44 * opptFTA))

home_final <- home_adv %>%
    select(1,41:72,39,40,37)

colnames(home_final) <- c("Team","FG","SR2","FG3","SR3","FT","FTR","ORB","DRB","TRB",
                          "AST","TOV","STL","BLK","PF","eFG","TS",
                          "oFG","oSR2","oFG3","oSR3","oFT","oFTR","oORB","oDRB","oTRB",
                          "oAST","oTOV","oSTL","oBLK","oPF","oeFG","oTS",
                          "ORtg","DRtg","Pace")

### GROUPING AWAY GAMES

away_grouped <- away %>%
    select(3,6:38) %>%
    group_by(teamName,teamGameCount) %>%
    summarise(across(c(teamPTS:opptPF), sum))

###### AWAY ADVANCED STATS #####

away_adv <- away_grouped

away_adv$Poss <- with(away_adv, teamFGA - teamORB + teamTOV + (.44 * teamFTA))
away_adv$oPoss <- with(away_adv, opptFGA - opptORB + opptTOV + (.44 * opptFTA))
away_adv$Pace <- with(away_adv, 48 * (Poss + oPoss) / (2 * (teamMin/5)))
away_adv$oPace <- with(away_adv, 48 * (Poss + oPoss) / (2 * (opptMin/5)))
away_adv$ORtg <- with(away_adv, (teamPTS / Poss) * 100)
away_adv$DRtg <- with(away_adv, (opptPTS / oPoss) * 100)

away_adv$FG <- with(away_adv, teamFGM / teamFGA)
away_adv$SR2 <- with(away_adv, (teamFGA - team3PA) / teamFGA)
away_adv$FG3 <- with(away_adv, team3PM / team3PA)
away_adv$SR3 <- with(away_adv, team3PA / teamFGA)
away_adv$FT <- with(away_adv, teamFTM / teamFTA)
away_adv$FTR <- with(away_adv, teamFTM / teamFGA)
away_adv$ORB <- with(away_adv, teamORB / (teamORB + opptDRB))
away_adv$DRB <- with(away_adv, teamDRB / (teamDRB + opptORB))
away_adv$TRB <- with(away_adv, teamTRB / (teamTRB + opptTRB))
away_adv$AST <- with(away_adv, teamAST / teamFGM)
away_adv$TOV <- with(away_adv, teamTOV / Poss)
away_adv$STL <- with(away_adv, teamSTL / oPoss)
away_adv$BLK <- with(away_adv, teamBLK / (opptFGA - oppt3PA))
away_adv$PF <- with(away_adv, teamPF / oPoss)
away_adv$eFG <- with(away_adv, (teamFGM + .5 * team3PM) / teamFGA)
away_adv$TS <- with(away_adv, teamPTS / (2 * teamFGA + .44 * teamFTA))

away_adv$oFG <- with(away_adv, opptFGM / opptFGA)
away_adv$oSR2 <- with(away_adv, (opptFGA - oppt3PA) / opptFGA)
away_adv$oFG3 <- with(away_adv, oppt3PM / oppt3PA)
away_adv$oSR3 <- with(away_adv, oppt3PA / opptFGA)
away_adv$oFT <- with(away_adv, opptFTM / opptFTA)
away_adv$oFTR <- with(away_adv, opptFTM / opptFGA)
away_adv$oORB <- with(away_adv, opptORB / (opptORB + teamDRB))
away_adv$oDRB <- with(away_adv, opptDRB / (opptDRB + teamORB))
away_adv$oTRB <- with(away_adv, opptTRB / (teamTRB + opptTRB))
away_adv$oAST <- with(away_adv, opptAST / opptFGM)
away_adv$oTOV <- with(away_adv, opptTOV / oPoss)
away_adv$oSTL <- with(away_adv, opptSTL / Poss)
away_adv$oBLK <- with(away_adv, opptBLK / (teamFGA - team3PA))
away_adv$oPF <- with(away_adv, opptPF / Poss)
away_adv$oeFG <- with(away_adv, (opptFGM + .5 * oppt3PM) / opptFGA)
away_adv$oTS <- with(away_adv, opptPTS / (2 * opptFGA + .44 * opptFTA))

away_final <- away_adv %>%
    select(1,41:72,39,40,37)

colnames(away_final) <- c("Team","FG","SR2","FG3","SR3","FT","FTR","ORB","DRB","TRB",
                               "AST","TOV","STL","BLK","PF","eFG","TS",
                               "oFG","oSR2","oFG3","oSR3","oFT","oFTR","oORB","oDRB","oTRB",
                               "oAST","oTOV","oSTL","oBLK","oPF","oeFG","oTS",
                               "ORtg","DRtg","Pace")

### HOME LEAGUE AVG STATS

home_lg_avg <- home_final %>%
    group_by() %>%
    summarise(across(where(is.numeric), mean))

home_lg_avg$PPG <- mean(home$teamPTS)

home_lg_avg$Lg_Avg <- "Home"
home_lg_avg <- home_lg_avg %>%
    select(37,1:36)

### AWAY LEAGUE AVG STATS

away_lg_avg <- away_final %>%
    group_by() %>%
    summarise(across(where(is.numeric), mean))

away_lg_avg$PPG <- mean(away$teamPTS)

away_lg_avg$Lg_Avg <- "Away"
away_lg_avg <- away_lg_avg %>%
    select(37,1:36)

### SEASON LEAGUE AVG STATS

season_lg_avg <- season_final %>%
    group_by() %>%
    summarise(across(where(is.numeric), mean))

season_lg_avg$PPG <- (away_lg_avg$PPG + home_lg_avg$PPG)/2

season_lg_avg$Lg_Avg <- "Season"
season_lg_avg <- season_lg_avg %>%
    select(37,1:36)

# COMBINE LEAGUE AVERAGE TABLES

league_avg <- bind_rows(season_lg_avg, home_lg_avg, away_lg_avg)

#### Game Details ####

standings_raw <- standings(seasons = 2022, season_types = c("Regular Season"))

standings <- standings_raw %>%
    select(4,7,8,22,17,25,13,24,90,27,12,23,87,26,11,50)

colnames(standings) <- c("Team","Conference","Division","Record","Win%","L10","Streak",
                         "Away Record","Away Win%","Away L10","Away Streak",
                         "Home Record", "Home Win%","Home L10","Home Streak",
                         "Conference Rank")

#### Change dataframe names for consistency when running models ####

### Early
away_final_wt <- as.data.frame(season_final)
home_final_wt <- as.data.frame(season_final)

### Later
# away_final_wt <- away_final
# home_final_wt <- home_final


players <- dataGameLogsPlayer %>%
    select(16,42,35,51,30) %>%
    group_by(nameTeam,namePlayer) %>%
    summarise(across(where(is.numeric), ~round(mean(.),1)))

pl_pts <- players %>%
    group_by(nameTeam) %>%
    filter(pts == max(pts)) %>%
    distinct(nameTeam, .keep_all = TRUE)

pl_reb <- players %>%
    group_by(nameTeam) %>%
    filter(treb == max(treb)) %>%
    distinct(nameTeam, .keep_all = TRUE)

pl_ast <- players %>%
    group_by(nameTeam) %>%
    filter(ast == max(ast)) %>%
    distinct(nameTeam, .keep_all = TRUE)
    







