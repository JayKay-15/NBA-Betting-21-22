######## Stat CleanR ########

### Creates adjusted year to date stats for Kendall & Tyra

if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, readxl, na.tools, caTools, Amelia,
               ggthemes, ggrepel, ggimage, XML, RCurl, openxlsx,
               rvest, nflfastR, nbastatR, nbaTools)

rm(list=ls())
setwd("/Users/Jesse/Documents/MyStuff/NBA Database/2020-2021")

# cur_date <- "3_15_2021"
# u <- paste0("/Users/Jesse/Documents/MyStuff/NBA Database/2021-2022/",cur_date,".xlsx")

### Pull game logs & arrange by date

game_logs(seasons = 2021, result_types = c("team","players"))

dataGameLogsTeam <- dataGameLogsTeam %>% arrange(dateGame,idGame)
dataGameLogsPlayer <- dataGameLogsPlayer %>% arrange(dateGame,idGame)

### Fix the LA Clippers team name

dataGameLogsTeam$nameTeam <- replace(dataGameLogsTeam$nameTeam, 
                                     dataGameLogsTeam$nameTeam == "LA Clippers","Los Angeles Clippers")

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
                  "teamTO", "teamSTL", "teamBLK", "teamPF", 
                  "opptFGM", "opptFGA", "oppt3PM", "oppt3PA", "opptFTM", 
                  "opptFTA", "opptORB", "opptDRB", "opptTRB", "opptAST", 
                  "opptTO", "opptSTL", "opptBLK", "opptPF")

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
    summarise_at(vars(teamPTS:opptPF), sum)

##### SEASON ADVANCED STATS #####

season_adv <- season_grouped

season_adv$Poss <- (season_adv$teamFGA - season_adv$teamORB + season_adv$teamTO + (.44 * season_adv$teamFTA))
season_adv$oPoss <- (season_adv$opptFGA - season_adv$opptORB + season_adv$opptTO + (.44 * season_adv$opptFTA))
season_adv$Pace <- (48 * (season_adv$Poss + season_adv$oPoss) / (2 * (season_adv$teamMin/5)))
season_adv$oPace <- (48 * (season_adv$Poss + season_adv$oPoss) / (2 * (season_adv$opptMin/5)))
season_adv$ORtg <- (season_adv$teamPTS / season_adv$Poss) * 100
season_adv$DRtg <- (season_adv$opptPTS / season_adv$oPoss) * 100

season_adv$FG <- season_adv$teamFGM / season_adv$teamFGA
season_adv$SR2 <- (season_adv$teamFGA - season_adv$team3PA) / season_adv$teamFGA
season_adv$FG3 <- season_adv$team3PM / season_adv$team3PA
season_adv$SR3 <- season_adv$team3PA / season_adv$teamFGA
season_adv$FT <- season_adv$teamFTM / season_adv$teamFTA
season_adv$FTR <- season_adv$teamFTM / season_adv$teamFGA
season_adv$ORB <- season_adv$teamORB / (season_adv$teamORB + season_adv$opptDRB)
season_adv$DRB <- season_adv$teamDRB / (season_adv$teamDRB + season_adv$opptORB)
season_adv$TRB <- season_adv$teamTRB / (season_adv$teamTRB + season_adv$opptTRB)
season_adv$AST <- season_adv$teamAST / season_adv$teamFGM
season_adv$TO <- season_adv$teamTO / season_adv$Poss
season_adv$STL <- season_adv$teamSTL / season_adv$oPoss
season_adv$BLK <- season_adv$teamBLK / (season_adv$opptFGA - season_adv$oppt3PA)
season_adv$PF <- season_adv$teamPF / season_adv$oPoss
season_adv$eFG <- (season_adv$teamFGM + .5 * season_adv$team3PM) / season_adv$teamFGA
season_adv$TS <- season_adv$teamPTS / (2 * season_adv$teamFGA + .44 * season_adv$teamFTA)

season_adv$oFG <- season_adv$opptFGM / season_adv$opptFGA
season_adv$oSR2 <- (season_adv$opptFGA - season_adv$oppt3PA) / season_adv$opptFGA
season_adv$oFG3 <- season_adv$oppt3PM / season_adv$oppt3PA
season_adv$oSR3 <- season_adv$oppt3PA / season_adv$opptFGA
season_adv$oFT <- season_adv$opptFTM / season_adv$opptFTA
season_adv$oFTR <- season_adv$opptFTM / season_adv$opptFGA
season_adv$oORB <- season_adv$opptORB / (season_adv$opptORB + season_adv$teamDRB)
season_adv$oDRB <- season_adv$opptDRB / (season_adv$opptDRB + season_adv$teamORB)
season_adv$oTRB <- season_adv$opptTRB / (season_adv$teamTRB + season_adv$opptTRB)
season_adv$oAST <- season_adv$opptAST / season_adv$opptFGM
season_adv$oTO <- season_adv$opptTO / season_adv$oPoss
season_adv$oSTL <- season_adv$opptSTL / season_adv$Poss
season_adv$oBLK <- season_adv$opptBLK / (season_adv$teamFGA - season_adv$team3PA)
season_adv$oPF <- season_adv$opptPF / season_adv$Poss
season_adv$oeFG <- (season_adv$opptFGM + .5 * season_adv$oppt3PM) / season_adv$opptFGA
season_adv$oTS <- season_adv$opptPTS / (2 * season_adv$opptFGA + .44 * season_adv$opptFTA)

season_final <- season_adv %>%
    select(1,41:72,39,40,37)

### GROUPING HOME GAMES 

home_grouped <- home %>%
    select(3,6:38) %>%
    group_by(teamName,teamGameCount) %>%
    summarise_at(vars(teamPTS:opptPF), sum)

###### HOME ADVANCED STATS ######

home_adv <- home_grouped

home_adv$Poss <- (home_adv$teamFGA - home_adv$teamORB + home_adv$teamTO + (.44 * home_adv$teamFTA))
home_adv$oPoss <- (home_adv$opptFGA - home_adv$opptORB + home_adv$opptTO + (.44 * home_adv$opptFTA))
home_adv$Pace <- (48 * (home_adv$Poss + home_adv$oPoss) / (2 * (home_adv$teamMin/5)))
home_adv$oPace <- (48 * (home_adv$Poss + home_adv$oPoss) / (2 * (home_adv$opptMin/5)))
home_adv$ORtg <- (home_adv$teamPTS / home_adv$Poss) * 100
home_adv$DRtg <- (home_adv$opptPTS / home_adv$oPoss) * 100

home_adv$FG <- home_adv$teamFGM / home_adv$teamFGA
home_adv$SR2 <- (home_adv$teamFGA - home_adv$team3PA) / home_adv$teamFGA
home_adv$FG3 <- home_adv$team3PM / home_adv$team3PA
home_adv$SR3 <- home_adv$team3PA / home_adv$teamFGA
home_adv$FT <- home_adv$teamFTM / home_adv$teamFTA
home_adv$FTR <- home_adv$teamFTM / home_adv$teamFGA
home_adv$ORB <- home_adv$teamORB / (home_adv$teamORB + home_adv$opptDRB)
home_adv$DRB <- home_adv$teamDRB / (home_adv$teamDRB + home_adv$opptORB)
home_adv$TRB <- home_adv$teamTRB / (home_adv$teamTRB + home_adv$opptTRB)
home_adv$AST <- home_adv$teamAST / home_adv$teamFGM
home_adv$TO <- home_adv$teamTO / home_adv$Poss
home_adv$STL <- home_adv$teamSTL / home_adv$oPoss
home_adv$BLK <- home_adv$teamBLK / (home_adv$opptFGA - home_adv$oppt3PA)
home_adv$PF <- home_adv$teamPF / home_adv$oPoss
home_adv$eFG <- (home_adv$teamFGM + .5 * home_adv$team3PM) / home_adv$teamFGA
home_adv$TS <- home_adv$teamPTS / (2 * home_adv$teamFGA + .44 * home_adv$teamFTA)

home_adv$oFG <- home_adv$opptFGM / home_adv$opptFGA
home_adv$oSR2 <- (home_adv$opptFGA - home_adv$oppt3PA) / home_adv$opptFGA
home_adv$oFG3 <- home_adv$oppt3PM / home_adv$oppt3PA
home_adv$oSR3 <- home_adv$oppt3PA / home_adv$opptFGA
home_adv$oFT <- home_adv$opptFTM / home_adv$opptFTA
home_adv$oFTR <- home_adv$opptFTM / home_adv$opptFGA
home_adv$oORB <- home_adv$opptORB / (home_adv$opptORB + home_adv$teamDRB)
home_adv$oDRB <- home_adv$opptDRB / (home_adv$opptDRB + home_adv$teamORB)
home_adv$oTRB <- home_adv$opptTRB / (home_adv$teamTRB + home_adv$opptTRB)
home_adv$oAST <- home_adv$opptAST / home_adv$opptFGM
home_adv$oTO <- home_adv$opptTO / home_adv$oPoss
home_adv$oSTL <- home_adv$opptSTL / home_adv$Poss
home_adv$oBLK <- home_adv$opptBLK / (home_adv$teamFGA - home_adv$team3PA)
home_adv$oPF <- home_adv$opptPF / home_adv$Poss
home_adv$oeFG <- (home_adv$opptFGM + .5 * home_adv$oppt3PM) / home_adv$opptFGA
home_adv$oTS <- home_adv$opptPTS / (2 * home_adv$opptFGA + .44 * home_adv$opptFTA)

home_final <- home_adv %>%
    select(1,41:72,39,40,37)

### GROUPING AWAY GAMES

away_grouped <- away %>%
    select(3,6:38) %>%
    group_by(teamName,teamGameCount) %>%
    summarise_at(vars(teamPTS:opptPF), sum)

###### AWAY ADVANCED STATS #####

away_adv <- away_grouped

away_adv$Poss <- (away_adv$teamFGA - away_adv$teamORB + away_adv$teamTO + (.44 * away_adv$teamFTA))
away_adv$oPoss <- (away_adv$opptFGA - away_adv$opptORB + away_adv$opptTO + (.44 * away_adv$opptFTA))
away_adv$Pace <- (48 * (away_adv$Poss + away_adv$oPoss) / (2 * (away_adv$teamMin/5)))
away_adv$oPace <- (48 * (away_adv$Poss + away_adv$oPoss) / (2 * (away_adv$opptMin/5)))
away_adv$ORtg <- (away_adv$teamPTS / away_adv$Poss) * 100
away_adv$DRtg <- (away_adv$opptPTS / away_adv$oPoss) * 100

away_adv$FG <- away_adv$teamFGM / away_adv$teamFGA
away_adv$SR2 <- (away_adv$teamFGA - away_adv$team3PA) / away_adv$teamFGA
away_adv$FG3 <- away_adv$team3PM / away_adv$team3PA
away_adv$SR3 <- away_adv$team3PA / away_adv$teamFGA
away_adv$FT <- away_adv$teamFTM / away_adv$teamFTA
away_adv$FTR <- away_adv$teamFTM / away_adv$teamFGA
away_adv$ORB <- away_adv$teamORB / (away_adv$teamORB + away_adv$opptDRB)
away_adv$DRB <- away_adv$teamDRB / (away_adv$teamDRB + away_adv$opptORB)
away_adv$TRB <- away_adv$teamTRB / (away_adv$teamTRB + away_adv$opptTRB)
away_adv$AST <- away_adv$teamAST / away_adv$teamFGM
away_adv$TO <- away_adv$teamTO / away_adv$Poss
away_adv$STL <- away_adv$teamSTL / away_adv$oPoss
away_adv$BLK <- away_adv$teamBLK / (away_adv$opptFGA - away_adv$oppt3PA)
away_adv$PF <- away_adv$teamPF / away_adv$oPoss
away_adv$eFG <- (away_adv$teamFGM + .5 * away_adv$team3PM) / away_adv$teamFGA
away_adv$TS <- away_adv$teamPTS / (2 * away_adv$teamFGA + .44 * away_adv$teamFTA)

away_adv$oFG <- away_adv$opptFGM / away_adv$opptFGA
away_adv$oSR2 <- (away_adv$opptFGA - away_adv$oppt3PA) / away_adv$opptFGA
away_adv$oFG3 <- away_adv$oppt3PM / away_adv$oppt3PA
away_adv$oSR3 <- away_adv$oppt3PA / away_adv$opptFGA
away_adv$oFT <- away_adv$opptFTM / away_adv$opptFTA
away_adv$oFTR <- away_adv$opptFTM / away_adv$opptFGA
away_adv$oORB <- away_adv$opptORB / (away_adv$opptORB + away_adv$teamDRB)
away_adv$oDRB <- away_adv$opptDRB / (away_adv$opptDRB + away_adv$teamORB)
away_adv$oTRB <- away_adv$opptTRB / (away_adv$teamTRB + away_adv$opptTRB)
away_adv$oAST <- away_adv$opptAST / away_adv$opptFGM
away_adv$oTO <- away_adv$opptTO / away_adv$oPoss
away_adv$oSTL <- away_adv$opptSTL / away_adv$Poss
away_adv$oBLK <- away_adv$opptBLK / (away_adv$teamFGA - away_adv$team3PA)
away_adv$oPF <- away_adv$opptPF / away_adv$Poss
away_adv$oeFG <- (away_adv$opptFGM + .5 * away_adv$oppt3PM) / away_adv$opptFGA
away_adv$oTS <- away_adv$opptPTS / (2 * away_adv$opptFGA + .44 * away_adv$opptFTA)

away_final <- away_adv %>%
    select(1,41:72,39,40,37)

### HOME LEAGUE AVG STATS

home_lg_avg <- home_final %>%
    group_by() %>%
    summarise_if(is.numeric, mean)

home_lg_avg$Lg_Avg <- "Home"
home_lg_avg <- home_lg_avg %>%
    select(36,1:35)

### AWAY LEAGUE AVG STATS

away_lg_avg <- away_final %>%
    group_by() %>%
    summarise_if(is.numeric, mean)

away_lg_avg$Lg_Avg <- "Away"
away_lg_avg <- away_lg_avg %>%
    select(36,1:35)

### SEASON LEAGUE AVG STATS

season_lg_avg <- season_final %>%
    group_by() %>%
    summarise_if(is.numeric, mean)

season_lg_avg$Lg_Avg <- "Season"
season_lg_avg <- season_lg_avg %>%
    select(36,1:35)

# COMBINE LEAGUE AVERAGE TABLES

league_avg <- bind_rows(season_lg_avg, home_lg_avg, away_lg_avg)

##### RAW SCHEDULE AND RESULTS ######

raw_adv <- gl

raw_adv$Poss <- (raw_adv$teamFGA - raw_adv$teamORB + raw_adv$teamTO + (.44 * raw_adv$teamFTA))
raw_adv$oPoss <- (raw_adv$opptFGA - raw_adv$opptORB + raw_adv$opptTO + (.44 * raw_adv$opptFTA))
raw_adv$Pace <- (48 * (raw_adv$Poss + raw_adv$oPoss) / (2 * (raw_adv$teamMin/5)))
raw_adv$oPace <- (48 * (raw_adv$Poss + raw_adv$oPoss) / (2 * (raw_adv$opptMin/5)))
raw_adv$ORtg <- (raw_adv$teamPTS / raw_adv$Poss) * 100
raw_adv$DRtg <- (raw_adv$opptPTS / raw_adv$oPoss) * 100

raw_adv$FG <- raw_adv$teamFGM / raw_adv$teamFGA
raw_adv$SR2 <- (raw_adv$teamFGA - raw_adv$team3PA) / raw_adv$teamFGA
raw_adv$FG3 <- raw_adv$team3PM / raw_adv$team3PA
raw_adv$SR3 <- raw_adv$team3PA / raw_adv$teamFGA
raw_adv$FT <- raw_adv$teamFTM / raw_adv$teamFTA
raw_adv$FTR <- raw_adv$teamFTM / raw_adv$teamFGA
raw_adv$ORB <- raw_adv$teamORB / (raw_adv$teamORB + raw_adv$opptDRB)
raw_adv$DRB <- raw_adv$teamDRB / (raw_adv$teamDRB + raw_adv$opptORB)
raw_adv$TRB <- raw_adv$teamTRB / (raw_adv$teamTRB + raw_adv$opptTRB)
raw_adv$AST <- raw_adv$teamAST / raw_adv$teamFGM
raw_adv$TO <- raw_adv$teamTO / raw_adv$Poss
raw_adv$STL <- raw_adv$teamSTL / raw_adv$oPoss
raw_adv$BLK <- raw_adv$teamBLK / (raw_adv$opptFGA - raw_adv$oppt3PA)
raw_adv$PF <- raw_adv$teamPF / raw_adv$oPoss
raw_adv$eFG <- (raw_adv$teamFGM + .5 * raw_adv$team3PM) / raw_adv$teamFGA
raw_adv$TS <- raw_adv$teamPTS / (2 * raw_adv$teamFGA + .44 * raw_adv$teamFTA)

raw_adv$oFG <- raw_adv$opptFGM / raw_adv$opptFGA
raw_adv$oSR2 <- (raw_adv$opptFGA - raw_adv$oppt3PA) / raw_adv$opptFGA
raw_adv$oFG3 <- raw_adv$oppt3PM / raw_adv$oppt3PA
raw_adv$oSR3 <- raw_adv$oppt3PA / raw_adv$opptFGA
raw_adv$oFT <- raw_adv$opptFTM / raw_adv$opptFTA
raw_adv$oFTR <- raw_adv$opptFTM / raw_adv$opptFGA
raw_adv$oORB <- raw_adv$opptORB / (raw_adv$opptORB + raw_adv$teamDRB)
raw_adv$oDRB <- raw_adv$opptDRB / (raw_adv$opptDRB + raw_adv$teamORB)
raw_adv$oTRB <- raw_adv$opptTRB / (raw_adv$teamTRB + raw_adv$opptTRB)
raw_adv$oAST <- raw_adv$opptAST / raw_adv$opptFGM
raw_adv$oTO <- raw_adv$opptTO / raw_adv$oPoss
raw_adv$oSTL <- raw_adv$opptSTL / raw_adv$Poss
raw_adv$oBLK <- raw_adv$opptBLK / (raw_adv$teamFGA - raw_adv$team3PA)
raw_adv$oPF <- raw_adv$opptPF / raw_adv$Poss
raw_adv$oeFG <- (raw_adv$opptFGM + .5 * raw_adv$oppt3PM) / raw_adv$opptFGA
raw_adv$oTS <- raw_adv$opptPTS / (2 * raw_adv$opptFGA + .44 * raw_adv$opptFTA)

raw_final <- raw_adv %>%
    select(2:4,46:77,44,45,42)

###### HOME/AWAY DIFF ######

#HOME DIFF

home_diff <- league_avg[2,-1] - league_avg[1,-1]
home_diff_percent <- home_diff[ , ]/league_avg[ 1, -1]

#AWAY DIFF

away_diff <- league_avg[3,-1] - league_avg[1,-1]
away_diff_percent <- away_diff[ , ]/league_avg[ 1, -1]

#ADVANTAGE

advantage_season <- gl %>%
    select(6) %>%
    summarise_if(is.numeric, mean)

advantage_season$ppg <- "season_ppg"

advantage_home <- home %>%
    select(2,6) %>%
    summarise_if(is.numeric, mean)

advantage_home$ppg <- "home_ppg"

advantage_away <- away %>%
    select(2,6) %>%
    summarise_if(is.numeric, mean)

advantage_away$ppg <- "away_ppg"

advantage <- bind_rows(advantage_season,advantage_home,advantage_away)

advantage_diff_home <- advantage[2,1] - advantage[1,1]
# advantage_diff_home <- advantage_diff_home[,]/advantage [1,1]
colnames(advantage_diff_home) <- "Home Advantage Points"

advantage_diff_away <- advantage[3,1] - advantage[1,1]
# advantage_diff_away <- advantage_diff_away[,]/advantage[1,1]
colnames(advantage_diff_away) <- "Away Advantage Points"

advantage_mx <- matrix()
advantage_mx <- bind_cols(advantage_diff_home, advantage_diff_away)

######### ROUND 1 ADJUSTMENTS ########

## join each team's average stats on to raw_adj
## split by home/away then add averages
## bring file back together

raw_adj_home <- left_join(raw_final, away_final, by = c("opptName" = "teamName")) %>%
    left_join(., home_final, by = c("teamName" = "teamName")) %>%
    filter(teamLoc == "H")

raw_adj_away <- left_join(raw_final, home_final, by = c("opptName" = "teamName")) %>%
    left_join(., away_final, by = c("teamName" = "teamName")) %>%
    filter(teamLoc == "A")

raw_adj <- bind_rows(raw_adj_home,raw_adj_away)

# opptavg = .y, team avg. = no tail, team actual for that game = .x

raw_adj$FG_adj <- (raw_adj$FG.x - (raw_adj$oFG.y - season_lg_avg$FG))
raw_adj$SR2_adj <- (raw_adj$SR2.x - (raw_adj$oSR2.y - season_lg_avg$SR2))
raw_adj$FG3_adj <- (raw_adj$FG3.x - (raw_adj$oFG3.y - season_lg_avg$FG3 ))
raw_adj$SR3_adj <- (raw_adj$SR3.x - (raw_adj$oSR3.y - season_lg_avg$SR3))
raw_adj$FT_adj <- (raw_adj$FT.x - (raw_adj$oFT.y - season_lg_avg$FT))
raw_adj$FTR_adj <- (raw_adj$FTR.x - (raw_adj$oFTR.y - season_lg_avg$FTR))
raw_adj$ORB_adj <- (raw_adj$ORB.x + (raw_adj$oDRB.y - season_lg_avg$DRB))
raw_adj$DRB_adj <- (raw_adj$DRB.x - (raw_adj$oORB.y - season_lg_avg$ORB))
raw_adj$TRB_adj <- (raw_adj$TRB.x + (raw_adj$oTRB.y - season_lg_avg$TRB))
raw_adj$AST_adj <- (raw_adj$AST.x - (raw_adj$oAST.y - season_lg_avg$AST))
raw_adj$TO_adj <- (raw_adj$TO.x - (raw_adj$oTO.y - season_lg_avg$TO))
raw_adj$STL_adj <- (raw_adj$STL.x - (raw_adj$oSTL.y - season_lg_avg$STL))
raw_adj$BLK_adj <- (raw_adj$BLK.x - (raw_adj$oBLK.y - season_lg_avg$BLK))
raw_adj$PF_adj <- (raw_adj$PF.x - (raw_adj$oPF.y - season_lg_avg$PF)) 
raw_adj$eFG_adj <- (raw_adj$eFG.x - (raw_adj$oeFG.y - season_lg_avg$eFG))
raw_adj$TS_adj <- (raw_adj$TS.x - (raw_adj$oTS.y - season_lg_avg$TS))
raw_adj$ExpPace <- (season_lg_avg$Pace + (raw_adj$Pace - season_lg_avg$Pace) + 
                        (raw_adj$Pace.y - season_lg_avg$Pace))
raw_adj$PaceDiff <- (raw_adj$Pace.x - raw_adj$ExpPace)
raw_adj$PaceR <- (raw_adj$Pace / (raw_adj$Pace + raw_adj$Pace.y))
raw_adj$oPaceR <- (raw_adj$Pace.y / (raw_adj$Pace + raw_adj$Pace.y))
raw_adj$Pace_adj <- (raw_adj$Pace + (raw_adj$PaceDiff * raw_adj$PaceR))
raw_adj$ORtg_adj <- (raw_adj$ORtg.x - (raw_adj$DRtg.y - season_lg_avg$DRtg))
raw_adj$DRtg_adj <- (raw_adj$DRtg.x - (raw_adj$ORtg.y - season_lg_avg$ORtg))

raw_adj$oFG_adj <- (raw_adj$oFG.x - (raw_adj$FG - season_lg_avg$FG))
raw_adj$oSR2_adj <- (raw_adj$oSR2.x - (raw_adj$SR2 - season_lg_avg$SR2))
raw_adj$oFG3_adj <- (raw_adj$oFG3.x - (raw_adj$FG3 - season_lg_avg$FG3 ))
raw_adj$oSR3_adj <- (raw_adj$oSR3.x - (raw_adj$SR3 - season_lg_avg$SR3))
raw_adj$oFT_adj <- (raw_adj$oFT.x - (raw_adj$FT - season_lg_avg$FT))
raw_adj$oFTR_adj <- (raw_adj$oFTR.x - (raw_adj$FTR - season_lg_avg$FTR))
raw_adj$oORB_adj <- (raw_adj$oORB.x + (raw_adj$DRB - season_lg_avg$DRB))
raw_adj$oDRB_adj <- (raw_adj$oDRB.x - (raw_adj$ORB - season_lg_avg$ORB))
raw_adj$oTRB_adj <- (raw_adj$oTRB.x + (raw_adj$TRB - season_lg_avg$TRB))
raw_adj$oAST_adj <- (raw_adj$oAST.x - (raw_adj$AST - season_lg_avg$AST))
raw_adj$oTO_adj <- (raw_adj$oTO.x - (raw_adj$TO - season_lg_avg$TO))
raw_adj$oSTL_adj <- (raw_adj$oSTL.x - (raw_adj$STL - season_lg_avg$STL))
raw_adj$oBLK_adj <- (raw_adj$oBLK.x - (raw_adj$BLK - season_lg_avg$BLK))
raw_adj$oPF_adj <- (raw_adj$oPF.x - (raw_adj$PF - season_lg_avg$PF)) 
raw_adj$oeFG_adj <- (raw_adj$oeFG.x - (raw_adj$eFG - season_lg_avg$eFG))
raw_adj$oTS_adj <- (raw_adj$oTS.x - (raw_adj$TS - season_lg_avg$TS))

### GROUP ROUND 1 ADJUSTMENTS

season_adj_round_1 <- raw_adj %>%
    select(2,109:124,132:147,130,131,129) %>%
    group_by(teamName) %>%
    summarise_if(is.numeric, mean)

home_adj_round_1 <- raw_adj %>%
    select(1,2,109:124,132:147,130,131,129) %>%
    filter(teamLoc == "H") %>%
    group_by(teamName) %>%
    summarise_if(is.numeric, mean)

away_adj_round_1 <- raw_adj %>%
    select(1,2,109:124,132:147,130,131,129) %>%
    filter(teamLoc == "A") %>%
    group_by(teamName) %>%
    summarise_if(is.numeric, mean)

######### ROUND 2 ADJUSTMENTS ########

#Joining for oppt stats
raw_adj_home_2 <- left_join(raw_final, away_adj_round_1, by = c("opptName" = "teamName")) %>%
    left_join(., home_adj_round_1, by = c("teamName" = "teamName")) %>%
    filter(teamLoc == "H")
#Joining for team stats
raw_adj_away_2 <- left_join(raw_final, home_adj_round_1, by = c("opptName" = "teamName")) %>%
    left_join(., away_adj_round_1, by = c("teamName" = "teamName")) %>%
    filter(teamLoc == "A")

raw_adj_2 <- bind_rows(raw_adj_home_2,raw_adj_away_2)

#.x = oppt avg, .y = team avg, no tail = game actual

raw_adj_2$FG_adj <- (raw_adj_2$FG - (raw_adj_2$oFG_adj.x - season_lg_avg$FG))
raw_adj_2$SR2_adj <- (raw_adj_2$SR2 - (raw_adj_2$oSR2_adj.x - season_lg_avg$SR2))
raw_adj_2$FG3_adj <- (raw_adj_2$FG3 - (raw_adj_2$oFG3_adj.x - season_lg_avg$FG3 ))
raw_adj_2$SR3_adj <- (raw_adj_2$SR3 - (raw_adj_2$oSR3_adj.x - season_lg_avg$SR3))
raw_adj_2$FT_adj <- (raw_adj_2$FT - (raw_adj_2$oFT_adj.x - season_lg_avg$FT))
raw_adj_2$FTR_adj <- (raw_adj_2$FTR - (raw_adj_2$oFTR_adj.x - season_lg_avg$FTR))
raw_adj_2$ORB_adj <- (raw_adj_2$ORB + (raw_adj_2$oDRB_adj.x - season_lg_avg$DRB))
raw_adj_2$DRB_adj <- (raw_adj_2$DRB - (raw_adj_2$oORB_adj.x - season_lg_avg$ORB))
raw_adj_2$TRB_adj <- (raw_adj_2$TRB + (raw_adj_2$oTRB_adj.x - season_lg_avg$TRB))
raw_adj_2$AST_adj <- (raw_adj_2$AST - (raw_adj_2$oAST_adj.x - season_lg_avg$AST))
raw_adj_2$TO_adj <- (raw_adj_2$TO - (raw_adj_2$oTO_adj.x - season_lg_avg$TO))
raw_adj_2$STL_adj <- (raw_adj_2$STL - (raw_adj_2$oSTL_adj.x - season_lg_avg$STL))
raw_adj_2$BLK_adj <- (raw_adj_2$BLK - (raw_adj_2$oBLK_adj.x - season_lg_avg$BLK))
raw_adj_2$PF_adj <- (raw_adj_2$PF - (raw_adj_2$oPF_adj.x - season_lg_avg$PF)) 
raw_adj_2$eFG_adj <- (raw_adj_2$eFG - (raw_adj_2$oeFG_adj.x - season_lg_avg$eFG))
raw_adj_2$TS_adj <- (raw_adj_2$TS - (raw_adj_2$oTS_adj.x - season_lg_avg$TS))
raw_adj_2$ExpPace <- (season_lg_avg$Pace + (raw_adj_2$Pace - season_lg_avg$Pace) + 
                          (raw_adj_2$Pace_adj.x - season_lg_avg$Pace))
raw_adj_2$PaceDiff <- (raw_adj_2$Pace - raw_adj_2$ExpPace)
raw_adj_2$PaceR <- (raw_adj_2$Pace_adj.y / (raw_adj_2$Pace_adj.y + raw_adj_2$Pace_adj.x))
raw_adj_2$oPaceR <- (raw_adj_2$Pace_adj.x / (raw_adj_2$Pace_adj.y + raw_adj_2$Pace_adj.x))
raw_adj_2$Pace_adj <- (raw_adj_2$Pace_adj.y + (raw_adj_2$PaceDiff * raw_adj_2$PaceR))
raw_adj_2$ORtg_adj <- (raw_adj_2$ORtg - (raw_adj_2$DRtg_adj.x - season_lg_avg$DRtg))
raw_adj_2$DRtg_adj <- (raw_adj_2$DRtg - (raw_adj_2$ORtg_adj.x - season_lg_avg$ORtg))

raw_adj_2$oFG_adj <- (raw_adj_2$oFG - (raw_adj_2$FG_adj.y - season_lg_avg$FG))
raw_adj_2$oSR2_adj <- (raw_adj_2$oSR2 - (raw_adj_2$SR2_adj.y - season_lg_avg$SR2))
raw_adj_2$oFG3_adj <- (raw_adj_2$oFG3 - (raw_adj_2$FG3_adj.y - season_lg_avg$FG3 ))
raw_adj_2$oSR3_adj <- (raw_adj_2$oSR3 - (raw_adj_2$SR3_adj.y - season_lg_avg$SR3))
raw_adj_2$oFT_adj <- (raw_adj_2$oFT - (raw_adj_2$FT_adj.y - season_lg_avg$FT))
raw_adj_2$oFTR_adj <- (raw_adj_2$oFTR - (raw_adj_2$FTR_adj.y - season_lg_avg$FTR))
raw_adj_2$oORB_adj <- (raw_adj_2$oORB + (raw_adj_2$DRB_adj.y - season_lg_avg$DRB))
raw_adj_2$oDRB_adj <- (raw_adj_2$oDRB - (raw_adj_2$ORB_adj.y - season_lg_avg$ORB))
raw_adj_2$oTRB_adj <- (raw_adj_2$oTRB + (raw_adj_2$TRB_adj.y - season_lg_avg$TRB))
raw_adj_2$oAST_adj <- (raw_adj_2$oAST - (raw_adj_2$AST_adj.y - season_lg_avg$AST))
raw_adj_2$oTO_adj <- (raw_adj_2$oTO - (raw_adj_2$TO_adj.y - season_lg_avg$TO))
raw_adj_2$oSTL_adj <- (raw_adj_2$oSTL - (raw_adj_2$STL_adj.y - season_lg_avg$STL))
raw_adj_2$oBLK_adj <- (raw_adj_2$oBLK - (raw_adj_2$BLK_adj.y - season_lg_avg$BLK))
raw_adj_2$oPF_adj <- (raw_adj_2$oPF - (raw_adj_2$PF_adj.y - season_lg_avg$PF)) 
raw_adj_2$oeFG_adj <- (raw_adj_2$oeFG - (raw_adj_2$eFG_adj.y - season_lg_avg$eFG))
raw_adj_2$oTS_adj <- (raw_adj_2$oTS - (raw_adj_2$TS_adj.y - season_lg_avg$TS))

### GROUP ROUND 2 ADJUSTMENTS

season_adj_round_2 <- raw_adj_2 %>%
    select(2,109:124,132:147,130,131,129) %>%
    group_by(teamName) %>%
    summarise_if(is.numeric, mean)

home_adj_round_2 <- raw_adj_2 %>%
    select(1,2,109:124,132:147,130,131,129) %>%
    filter(teamLoc == "H") %>%
    group_by(teamName) %>%
    summarise_if(is.numeric, mean)

away_adj_round_2 <- raw_adj_2 %>%
    select(1,2,109:124,132:147,130,131,129) %>%
    filter(teamLoc == "A") %>%
    group_by(teamName) %>%
    summarise_if(is.numeric, mean)

######### ROUND 3 ADJUSTMENTS ########

#Joining for oppt stats
raw_adj_home_3 <- left_join(raw_final, away_adj_round_2, by = c("opptName" = "teamName")) %>%
    left_join(., home_adj_round_2, by = c("teamName" = "teamName")) %>%
    filter(teamLoc == "H")
#Joining for team stats
raw_adj_away_3 <- left_join(raw_final, home_adj_round_2, by = c("opptName" = "teamName")) %>%
    left_join(., away_adj_round_2, by = c("teamName" = "teamName")) %>%
    filter(teamLoc == "A")

raw_adj_3 <- bind_rows(raw_adj_home_3,raw_adj_away_3)

#.x = oppt avg, .y = team avg, no tail = game actual

raw_adj_3$FG_adj <- (raw_adj_3$FG - (raw_adj_3$oFG_adj.x - season_lg_avg$FG))
raw_adj_3$SR2_adj <- (raw_adj_3$SR2 - (raw_adj_3$oSR2_adj.x - season_lg_avg$SR2))
raw_adj_3$FG3_adj <- (raw_adj_3$FG3 - (raw_adj_3$oFG3_adj.x - season_lg_avg$FG3 ))
raw_adj_3$SR3_adj <- (raw_adj_3$SR3 - (raw_adj_3$oSR3_adj.x - season_lg_avg$SR3))
raw_adj_3$FT_adj <- (raw_adj_3$FT - (raw_adj_3$oFT_adj.x - season_lg_avg$FT))
raw_adj_3$FTR_adj <- (raw_adj_3$FTR - (raw_adj_3$oFTR_adj.x - season_lg_avg$FTR))
raw_adj_3$ORB_adj <- (raw_adj_3$ORB + (raw_adj_3$oDRB_adj.x - season_lg_avg$DRB))
raw_adj_3$DRB_adj <- (raw_adj_3$DRB - (raw_adj_3$oORB_adj.x - season_lg_avg$ORB))
raw_adj_3$TRB_adj <- (raw_adj_3$TRB + (raw_adj_3$oTRB_adj.x - season_lg_avg$TRB))
raw_adj_3$AST_adj <- (raw_adj_3$AST - (raw_adj_3$oAST_adj.x - season_lg_avg$AST))
raw_adj_3$TO_adj <- (raw_adj_3$TO - (raw_adj_3$oTO_adj.x - season_lg_avg$TO))
raw_adj_3$STL_adj <- (raw_adj_3$STL - (raw_adj_3$oSTL_adj.x - season_lg_avg$STL))
raw_adj_3$BLK_adj <- (raw_adj_3$BLK - (raw_adj_3$oBLK_adj.x - season_lg_avg$BLK))
raw_adj_3$PF_adj <- (raw_adj_3$PF - (raw_adj_3$oPF_adj.x - season_lg_avg$PF)) 
raw_adj_3$eFG_adj <- (raw_adj_3$eFG - (raw_adj_3$oeFG_adj.x - season_lg_avg$eFG))
raw_adj_3$TS_adj <- (raw_adj_3$TS - (raw_adj_3$oTS_adj.x - season_lg_avg$TS))
raw_adj_3$ExpPace <- (season_lg_avg$Pace + (raw_adj_3$Pace - season_lg_avg$Pace) + 
                          (raw_adj_3$Pace_adj.x - season_lg_avg$Pace))
raw_adj_3$PaceDiff <- (raw_adj_3$Pace - raw_adj_3$ExpPace)
raw_adj_3$PaceR <- (raw_adj_3$Pace_adj.y / (raw_adj_3$Pace_adj.y + raw_adj_3$Pace_adj.x))
raw_adj_3$oPaceR <- (raw_adj_3$Pace_adj.x / (raw_adj_3$Pace_adj.y + raw_adj_3$Pace_adj.x))
raw_adj_3$Pace_adj <- (raw_adj_3$Pace_adj.y + (raw_adj_3$PaceDiff * raw_adj_3$PaceR))
raw_adj_3$ORtg_adj <- (raw_adj_3$ORtg - (raw_adj_3$DRtg_adj.x - season_lg_avg$DRtg))
raw_adj_3$DRtg_adj <- (raw_adj_3$DRtg - (raw_adj_3$ORtg_adj.x - season_lg_avg$ORtg))

raw_adj_3$oFG_adj <- (raw_adj_3$oFG - (raw_adj_3$FG_adj.y - season_lg_avg$FG))
raw_adj_3$oSR2_adj <- (raw_adj_3$oSR2 - (raw_adj_3$SR2_adj.y - season_lg_avg$SR2))
raw_adj_3$oFG3_adj <- (raw_adj_3$oFG3 - (raw_adj_3$FG3_adj.y - season_lg_avg$FG3 ))
raw_adj_3$oSR3_adj <- (raw_adj_3$oSR3 - (raw_adj_3$SR3_adj.y - season_lg_avg$SR3))
raw_adj_3$oFT_adj <- (raw_adj_3$oFT - (raw_adj_3$FT_adj.y - season_lg_avg$FT))
raw_adj_3$oFTR_adj <- (raw_adj_3$oFTR - (raw_adj_3$FTR_adj.y - season_lg_avg$FTR))
raw_adj_3$oORB_adj <- (raw_adj_3$oORB + (raw_adj_3$DRB_adj.y - season_lg_avg$DRB))
raw_adj_3$oDRB_adj <- (raw_adj_3$oDRB - (raw_adj_3$ORB_adj.y - season_lg_avg$ORB))
raw_adj_3$oTRB_adj <- (raw_adj_3$oTRB + (raw_adj_3$TRB_adj.y - season_lg_avg$TRB))
raw_adj_3$oAST_adj <- (raw_adj_3$oAST - (raw_adj_3$AST_adj.y - season_lg_avg$AST))
raw_adj_3$oTO_adj <- (raw_adj_3$oTO - (raw_adj_3$TO_adj.y - season_lg_avg$TO))
raw_adj_3$oSTL_adj <- (raw_adj_3$oSTL - (raw_adj_3$STL_adj.y - season_lg_avg$STL))
raw_adj_3$oBLK_adj <- (raw_adj_3$oBLK - (raw_adj_3$BLK_adj.y - season_lg_avg$BLK))
raw_adj_3$oPF_adj <- (raw_adj_3$oPF - (raw_adj_3$PF_adj.y - season_lg_avg$PF)) 
raw_adj_3$oeFG_adj <- (raw_adj_3$oeFG - (raw_adj_3$eFG_adj.y - season_lg_avg$eFG))
raw_adj_3$oTS_adj <- (raw_adj_3$oTS - (raw_adj_3$TS_adj.y - season_lg_avg$TS))

### GROUP ROUND 3 ADJUSTMENTS

season_adj_round_3 <- raw_adj_3 %>%
    select(2,109:124,132:147,130,131,129) %>%
    group_by(teamName) %>%
    summarise_if(is.numeric, mean)

home_adj_round_3 <- raw_adj_3 %>%
    select(1,2,109:124,132:147,130,131,129) %>%
    filter(teamLoc == "H") %>%
    group_by(teamName) %>%
    summarise_if(is.numeric, mean)

away_adj_round_3 <- raw_adj_3 %>%
    select(1,2,109:124,132:147,130,131,129) %>%
    filter(teamLoc == "A") %>%
    group_by(teamName) %>%
    summarise_if(is.numeric, mean)

### Weighting Data frames ###

season_uw <- raw_adj_3 %>%
    select(2,109:124,132:147,130,131,129)

home_uw <- raw_adj_3 %>%
    select(1,2,109:124,132:147,130,131,129) %>%
    filter(teamLoc == "H")

away_uw <- raw_adj_3 %>%
    select(1,2,109:124,132:147,130,131,129) %>%
    filter(teamLoc == "A")

##### WEIGHTING - AWAY ####

wt_holder_away <- data.frame()

a <- 1
g <- nrow(away_final)

for (a in a:g) {
    
    act_id <- as.character(away_final[a,1])
    
    adj_gxg <- away_uw %>%
        filter(teamName == act_id)
    
    ngames <- nrow(adj_gxg)
    
    if (ngames > 20) { weightmax <- 4 } else { weightmax <- 1 + ((ngames - 1) * .157894737)  }
    weightmin <- 1
    weightdist <- (weightmax - weightmin) / (ngames - 1)
    if (ngames < 2) { weightdist <- 0 }
    
    weightcurve <- matrix(0, nrow = ngames, ncol = 1)
    c <- 1
    i <- nrow(weightcurve)
    
    for (c in c:i) {
        
        weightcurve[c] <- weightmin + ((c - 1) * weightdist)
        
    }
    
    weight_sums <- sum(weightcurve)
    weight_avg <- mean(weightcurve)
    
    FG_wt <- (adj_gxg$FG_adj * weightcurve) / weight_sums
    SR2_wt <- (adj_gxg$SR2_adj * weightcurve) / weight_sums
    FG3_wt <- (adj_gxg$FG3_adj * weightcurve) / weight_sums
    SR3_wt <- (adj_gxg$SR3_adj * weightcurve) / weight_sums
    FT_wt <- (adj_gxg$FT_adj * weightcurve) / weight_sums
    FTR_wt <- (adj_gxg$FTR_adj * weightcurve) / weight_sums
    ORB_wt <- (adj_gxg$ORB_adj * weightcurve) / weight_sums
    DRB_wt <- (adj_gxg$DRB_adj * weightcurve) / weight_sums
    TRB_wt <- (adj_gxg$TRB_adj * weightcurve) / weight_sums
    AST_wt <- (adj_gxg$AST_adj * weightcurve) / weight_sums
    TO_wt <- (adj_gxg$TO_adj * weightcurve) / weight_sums
    STL_wt <- (adj_gxg$STL_adj * weightcurve) / weight_sums
    BLK_wt <- (adj_gxg$BLK_adj * weightcurve) / weight_sums
    PF_wt <- (adj_gxg$PF_adj * weightcurve) / weight_sums 
    eFG_wt <- (adj_gxg$eFG_adj * weightcurve) / weight_sums
    TS_wt <- (adj_gxg$TS_adj * weightcurve) / weight_sums
    Pace_wt <- (adj_gxg$Pace_adj * weightcurve) / weight_sums
    ORtg_wt <- (adj_gxg$ORtg_adj * weightcurve) / weight_sums
    DRtg_wt <- (adj_gxg$DRtg_adj * weightcurve) / weight_sums
    
    oFG_wt <- (adj_gxg$oFG_adj * weightcurve) / weight_sums
    oSR2_wt <- (adj_gxg$oSR2_adj * weightcurve) / weight_sums
    oFG3_wt <- (adj_gxg$oFG3_adj * weightcurve) / weight_sums
    oSR3_wt <- (adj_gxg$oSR3_adj * weightcurve) / weight_sums
    oFT_wt <- (adj_gxg$oFT_adj * weightcurve) / weight_sums
    oFTR_wt <- (adj_gxg$oFTR_adj * weightcurve) / weight_sums
    oORB_wt <- (adj_gxg$oORB_adj * weightcurve) / weight_sums
    oDRB_wt <- (adj_gxg$oDRB_adj * weightcurve) / weight_sums
    oTRB_wt <- (adj_gxg$oTRB_adj * weightcurve) / weight_sums
    oAST_wt <- (adj_gxg$oAST_adj * weightcurve) / weight_sums
    oTO_wt <- (adj_gxg$oTO_adj * weightcurve) / weight_sums
    oSTL_wt <- (adj_gxg$oSTL_adj * weightcurve) / weight_sums
    oBLK_wt <- (adj_gxg$oBLK_adj * weightcurve) / weight_sums
    oPF_wt <- (adj_gxg$oPF_adj * weightcurve) / weight_sums
    oeFG_wt <- (adj_gxg$oeFG_adj * weightcurve) / weight_sums
    oTS_wt <- (adj_gxg$oTS_adj * weightcurve) / weight_sums
    
    FG_wt <- sum(FG_wt)
    SR2_wt <- sum(SR2_wt)
    FG3_wt <- sum(FG3_wt)
    SR3_wt <- sum(SR3_wt)
    FT_wt <- sum(FT_wt)
    FTR_wt <- sum(FTR_wt)
    ORB_wt <- sum(ORB_wt)
    DRB_wt <- sum(DRB_wt)
    TRB_wt <- sum(TRB_wt)
    AST_wt <- sum(AST_wt)
    TO_wt <- sum(TO_wt)
    STL_wt <- sum(STL_wt)
    BLK_wt <- sum(BLK_wt)
    PF_wt <- sum(PF_wt)
    eFG_wt <- sum(eFG_wt)
    TS_wt <- sum(TS_wt)
    Pace_wt <- sum(Pace_wt)
    ORtg_wt <- sum(ORtg_wt)
    DRtg_wt <- sum(DRtg_wt)
    
    oFG_wt <- sum(oFG_wt)
    oSR2_wt <- sum(oSR2_wt)
    oFG3_wt <- sum(oFG3_wt)
    oSR3_wt <- sum(oSR3_wt)
    oFT_wt <- sum(oFT_wt)
    oFTR_wt <- sum(oFTR_wt)
    oORB_wt <- sum(oORB_wt)
    oDRB_wt <- sum(oDRB_wt)
    oTRB_wt <- sum(oTRB_wt)
    oAST_wt <- sum(oAST_wt)
    oTO_wt <- sum(oTO_wt)
    oSTL_wt <- sum(oSTL_wt)
    oBLK_wt <- sum(oBLK_wt)
    oPF_wt <- sum(oPF_wt)
    oeFG_wt <- sum(oeFG_wt)
    oTS_wt <- sum(oTS_wt)
    
    wt_df <- data.frame(act_id,FG_wt,SR2_wt,FG3_wt,SR3_wt,FT_wt,
                        FTR_wt,ORB_wt,DRB_wt,TRB_wt,AST_wt,TO_wt,
                        STL_wt,BLK_wt,PF_wt,eFG_wt,TS_wt,oFG_wt,oSR2_wt,oFG3_wt,oSR3_wt,
                        oFT_wt,oFTR_wt,oORB_wt,oDRB_wt,oTRB_wt,oAST_wt,
                        oTO_wt,oSTL_wt,oBLK_wt,oPF_wt,oeFG_wt,oTS_wt,
                        ORtg_wt,DRtg_wt,Pace_wt)
    
    wt_holder_away <- bind_rows(wt_holder_away,wt_df)
    
}

away_final_wt <- wt_holder_away

colnames(away_final_wt) <- c("Team","FG","SR2","FG3","SR3","FT","FTR","ORB","DRB","TRB",
                             "AST","TO","STL","BLK","PF","eFG","TS",
                             "oFG","oSR2","oFG3","oSR3","oFT","oFTR","oORB","oDRB","oTRB",
                             "oAST","oTO","oSTL","oBLK","oPF","oeFG","oTS",
                             "ORtg","DRtg","Pace")

##### WEIGHTING - HOME ####

wt_holder_home <- data.frame()

a <- 1
g <- nrow(home_final)

for (a in a:g) {
    
    act_id <- as.character(home_final[a,1])
    
    adj_gxg <- home_uw %>%
        filter(teamName == act_id)
    
    ngames <- nrow(adj_gxg)
    
    if (ngames > 20) { weightmax <- 4 } else { weightmax <- 1 + ((ngames - 1) * .157894737)  }
    weightmin <- 1
    weightdist <- (weightmax - weightmin) / (ngames - 1)
    if (ngames < 2) { weightdist <- 0 }
    
    weightcurve <- matrix(0, nrow = ngames, ncol = 1)
    c <- 1
    i <- nrow(weightcurve)
    
    for (c in c:i) {
        
        weightcurve[c] <- weightmin + ((c - 1) * weightdist)
        
    }
    
    weight_sums <- sum(weightcurve)
    weight_avg <- mean(weightcurve)
    
    FG_wt <- (adj_gxg$FG_adj * weightcurve) / weight_sums
    SR2_wt <- (adj_gxg$SR2_adj * weightcurve) / weight_sums
    FG3_wt <- (adj_gxg$FG3_adj * weightcurve) / weight_sums
    SR3_wt <- (adj_gxg$SR3_adj * weightcurve) / weight_sums
    FT_wt <- (adj_gxg$FT_adj * weightcurve) / weight_sums
    FTR_wt <- (adj_gxg$FTR_adj * weightcurve) / weight_sums
    ORB_wt <- (adj_gxg$ORB_adj * weightcurve) / weight_sums
    DRB_wt <- (adj_gxg$DRB_adj * weightcurve) / weight_sums
    TRB_wt <- (adj_gxg$TRB_adj * weightcurve) / weight_sums
    AST_wt <- (adj_gxg$AST_adj * weightcurve) / weight_sums
    TO_wt <- (adj_gxg$TO_adj * weightcurve) / weight_sums
    STL_wt <- (adj_gxg$STL_adj * weightcurve) / weight_sums
    BLK_wt <- (adj_gxg$BLK_adj * weightcurve) / weight_sums
    PF_wt <- (adj_gxg$PF_adj * weightcurve) / weight_sums 
    eFG_wt <- (adj_gxg$eFG_adj * weightcurve) / weight_sums
    TS_wt <- (adj_gxg$TS_adj * weightcurve) / weight_sums
    Pace_wt <- (adj_gxg$Pace_adj * weightcurve) / weight_sums
    ORtg_wt <- (adj_gxg$ORtg_adj * weightcurve) / weight_sums
    DRtg_wt <- (adj_gxg$DRtg_adj * weightcurve) / weight_sums
    
    oFG_wt <- (adj_gxg$oFG_adj * weightcurve) / weight_sums
    oSR2_wt <- (adj_gxg$oSR2_adj * weightcurve) / weight_sums
    oFG3_wt <- (adj_gxg$oFG3_adj * weightcurve) / weight_sums
    oSR3_wt <- (adj_gxg$oSR3_adj * weightcurve) / weight_sums
    oFT_wt <- (adj_gxg$oFT_adj * weightcurve) / weight_sums
    oFTR_wt <- (adj_gxg$oFTR_adj * weightcurve) / weight_sums
    oORB_wt <- (adj_gxg$oORB_adj * weightcurve) / weight_sums
    oDRB_wt <- (adj_gxg$oDRB_adj * weightcurve) / weight_sums
    oTRB_wt <- (adj_gxg$oTRB_adj * weightcurve) / weight_sums
    oAST_wt <- (adj_gxg$oAST_adj * weightcurve) / weight_sums
    oTO_wt <- (adj_gxg$oTO_adj * weightcurve) / weight_sums
    oSTL_wt <- (adj_gxg$oSTL_adj * weightcurve) / weight_sums
    oBLK_wt <- (adj_gxg$oBLK_adj * weightcurve) / weight_sums
    oPF_wt <- (adj_gxg$oPF_adj * weightcurve) / weight_sums
    oeFG_wt <- (adj_gxg$oeFG_adj * weightcurve) / weight_sums
    oTS_wt <- (adj_gxg$oTS_adj * weightcurve) / weight_sums
    
    FG_wt <- sum(FG_wt)
    SR2_wt <- sum(SR2_wt)
    FG3_wt <- sum(FG3_wt)
    SR3_wt <- sum(SR3_wt)
    FT_wt <- sum(FT_wt)
    FTR_wt <- sum(FTR_wt)
    ORB_wt <- sum(ORB_wt)
    DRB_wt <- sum(DRB_wt)
    TRB_wt <- sum(TRB_wt)
    AST_wt <- sum(AST_wt)
    TO_wt <- sum(TO_wt)
    STL_wt <- sum(STL_wt)
    BLK_wt <- sum(BLK_wt)
    PF_wt <- sum(PF_wt)
    eFG_wt <- sum(eFG_wt)
    TS_wt <- sum(TS_wt)
    Pace_wt <- sum(Pace_wt)
    ORtg_wt <- sum(ORtg_wt)
    DRtg_wt <- sum(DRtg_wt)
    
    oFG_wt <- sum(oFG_wt)
    oSR2_wt <- sum(oSR2_wt)
    oFG3_wt <- sum(oFG3_wt)
    oSR3_wt <- sum(oSR3_wt)
    oFT_wt <- sum(oFT_wt)
    oFTR_wt <- sum(oFTR_wt)
    oORB_wt <- sum(oORB_wt)
    oDRB_wt <- sum(oDRB_wt)
    oTRB_wt <- sum(oTRB_wt)
    oAST_wt <- sum(oAST_wt)
    oTO_wt <- sum(oTO_wt)
    oSTL_wt <- sum(oSTL_wt)
    oBLK_wt <- sum(oBLK_wt)
    oPF_wt <- sum(oPF_wt)
    oeFG_wt <- sum(oeFG_wt)
    oTS_wt <- sum(oTS_wt)
    
    wt_df <- data.frame(act_id,FG_wt,SR2_wt,FG3_wt,SR3_wt,FT_wt,
                        FTR_wt,ORB_wt,DRB_wt,TRB_wt,AST_wt,TO_wt,
                        STL_wt,BLK_wt,PF_wt,eFG_wt,TS_wt,oFG_wt,oSR2_wt,oFG3_wt,oSR3_wt,
                        oFT_wt,oFTR_wt,oORB_wt,oDRB_wt,oTRB_wt,oAST_wt,
                        oTO_wt,oSTL_wt,oBLK_wt,oPF_wt,oeFG_wt,oTS_wt,
                        ORtg_wt,DRtg_wt,Pace_wt)
    
    wt_holder_home <- bind_rows(wt_holder_home,wt_df)
    
}

home_final_wt <- wt_holder_home

colnames(home_final_wt) <- c("Team","FG","SR2","FG3","SR3","FT","FTR","ORB","DRB","TRB",
                             "AST","TO","STL","BLK","PF","eFG","TS",
                             "oFG","oSR2","oFG3","oSR3","oFT","oFTR","oORB","oDRB","oTRB",
                             "oAST","oTO","oSTL","oBLK","oPF","oeFG","oTS",
                             "ORtg","DRtg","Pace")

##### WEIGHTING - SEASON ####

wt_holder_season <- data.frame()

a <- 1
g <- nrow(season_final)

for (a in a:g) {
    
    act_id <- as.character(season_final[a,1])
    
    adj_gxg <- season_uw %>%
        filter(teamName == act_id)
    
    ngames <- nrow(adj_gxg)
    
    if (ngames > 20) { weightmax <- 4 } else { weightmax <- 1 + ((ngames - 1) * .157894737)  }
    weightmin <- 1
    weightdist <- (weightmax - weightmin) / (ngames - 1)
    if (ngames < 2) { weightdist <- 0 }
    
    weightcurve <- matrix(0, nrow = ngames, ncol = 1)
    c <- 1
    i <- nrow(weightcurve)
    
    for (c in c:i) {
        
        weightcurve[c] <- weightmin + ((c - 1) * weightdist)
        
    }
    
    weight_sums <- sum(weightcurve)
    weight_avg <- mean(weightcurve)
    
    FG_wt <- (adj_gxg$FG_adj * weightcurve) / weight_sums
    SR2_wt <- (adj_gxg$SR2_adj * weightcurve) / weight_sums
    FG3_wt <- (adj_gxg$FG3_adj * weightcurve) / weight_sums
    SR3_wt <- (adj_gxg$SR3_adj * weightcurve) / weight_sums
    FT_wt <- (adj_gxg$FT_adj * weightcurve) / weight_sums
    FTR_wt <- (adj_gxg$FTR_adj * weightcurve) / weight_sums
    ORB_wt <- (adj_gxg$ORB_adj * weightcurve) / weight_sums
    DRB_wt <- (adj_gxg$DRB_adj * weightcurve) / weight_sums
    TRB_wt <- (adj_gxg$TRB_adj * weightcurve) / weight_sums
    AST_wt <- (adj_gxg$AST_adj * weightcurve) / weight_sums
    TO_wt <- (adj_gxg$TO_adj * weightcurve) / weight_sums
    STL_wt <- (adj_gxg$STL_adj * weightcurve) / weight_sums
    BLK_wt <- (adj_gxg$BLK_adj * weightcurve) / weight_sums
    PF_wt <- (adj_gxg$PF_adj * weightcurve) / weight_sums 
    eFG_wt <- (adj_gxg$eFG_adj * weightcurve) / weight_sums
    TS_wt <- (adj_gxg$TS_adj * weightcurve) / weight_sums
    Pace_wt <- (adj_gxg$Pace_adj * weightcurve) / weight_sums
    ORtg_wt <- (adj_gxg$ORtg_adj * weightcurve) / weight_sums
    DRtg_wt <- (adj_gxg$DRtg_adj * weightcurve) / weight_sums
    
    oFG_wt <- (adj_gxg$oFG_adj * weightcurve) / weight_sums
    oSR2_wt <- (adj_gxg$oSR2_adj * weightcurve) / weight_sums
    oFG3_wt <- (adj_gxg$oFG3_adj * weightcurve) / weight_sums
    oSR3_wt <- (adj_gxg$oSR3_adj * weightcurve) / weight_sums
    oFT_wt <- (adj_gxg$oFT_adj * weightcurve) / weight_sums
    oFTR_wt <- (adj_gxg$oFTR_adj * weightcurve) / weight_sums
    oORB_wt <- (adj_gxg$oORB_adj * weightcurve) / weight_sums
    oDRB_wt <- (adj_gxg$oDRB_adj * weightcurve) / weight_sums
    oTRB_wt <- (adj_gxg$oTRB_adj * weightcurve) / weight_sums
    oAST_wt <- (adj_gxg$oAST_adj * weightcurve) / weight_sums
    oTO_wt <- (adj_gxg$oTO_adj * weightcurve) / weight_sums
    oSTL_wt <- (adj_gxg$oSTL_adj * weightcurve) / weight_sums
    oBLK_wt <- (adj_gxg$oBLK_adj * weightcurve) / weight_sums
    oPF_wt <- (adj_gxg$oPF_adj * weightcurve) / weight_sums
    oeFG_wt <- (adj_gxg$oeFG_adj * weightcurve) / weight_sums
    oTS_wt <- (adj_gxg$oTS_adj * weightcurve) / weight_sums
    
    FG_wt <- sum(FG_wt)
    SR2_wt <- sum(SR2_wt)
    FG3_wt <- sum(FG3_wt)
    SR3_wt <- sum(SR3_wt)
    FT_wt <- sum(FT_wt)
    FTR_wt <- sum(FTR_wt)
    ORB_wt <- sum(ORB_wt)
    DRB_wt <- sum(DRB_wt)
    TRB_wt <- sum(TRB_wt)
    AST_wt <- sum(AST_wt)
    TO_wt <- sum(TO_wt)
    STL_wt <- sum(STL_wt)
    BLK_wt <- sum(BLK_wt)
    PF_wt <- sum(PF_wt)
    eFG_wt <- sum(eFG_wt)
    TS_wt <- sum(TS_wt)
    Pace_wt <- sum(Pace_wt)
    ORtg_wt <- sum(ORtg_wt)
    DRtg_wt <- sum(DRtg_wt)
    
    oFG_wt <- sum(oFG_wt)
    oSR2_wt <- sum(oSR2_wt)
    oFG3_wt <- sum(oFG3_wt)
    oSR3_wt <- sum(oSR3_wt)
    oFT_wt <- sum(oFT_wt)
    oFTR_wt <- sum(oFTR_wt)
    oORB_wt <- sum(oORB_wt)
    oDRB_wt <- sum(oDRB_wt)
    oTRB_wt <- sum(oTRB_wt)
    oAST_wt <- sum(oAST_wt)
    oTO_wt <- sum(oTO_wt)
    oSTL_wt <- sum(oSTL_wt)
    oBLK_wt <- sum(oBLK_wt)
    oPF_wt <- sum(oPF_wt)
    oeFG_wt <- sum(oeFG_wt)
    oTS_wt <- sum(oTS_wt)
    
    wt_df <- data.frame(act_id,FG_wt,SR2_wt,FG3_wt,SR3_wt,FT_wt,
                        FTR_wt,ORB_wt,DRB_wt,TRB_wt,AST_wt,TO_wt,
                        STL_wt,BLK_wt,PF_wt,eFG_wt,TS_wt,oFG_wt,oSR2_wt,oFG3_wt,oSR3_wt,
                        oFT_wt,oFTR_wt,oORB_wt,oDRB_wt,oTRB_wt,oAST_wt,
                        oTO_wt,oSTL_wt,oBLK_wt,oPF_wt,oeFG_wt,oTS_wt,
                        ORtg_wt,DRtg_wt,Pace_wt)
    
    wt_holder_season <- bind_rows(wt_holder_season,wt_df)
    
}

season_final_wt <- wt_holder_season

colnames(season_final_wt) <- c("Team","FG","SR2","FG3","SR3","FT","FTR","ORB","DRB","TRB",
                             "AST","TO","STL","BLK","PF","eFG","TS",
                             "oFG","oSR2","oFG3","oSR3","oFT","oFTR","oORB","oDRB","oTRB",
                             "oAST","oTO","oSTL","oBLK","oPF","oeFG","oTS",
                             "ORtg","DRtg","Pace")

##### PRINTING TO EXCEL #####

# wb <- createWorkbook()
# addWorksheet(wb, sheetName = "Kendall")
# addWorksheet(wb, sheetName = "Tyra")
# 
# writeData(wb, sheet = "Kendall", x = kendall_predict)
# writeData(wb, sheet = "Tyra", x = tyra_predict)
# 
# saveWorkbook(wb, file = u)












