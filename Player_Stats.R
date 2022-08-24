if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, readxl, lubridate, openxlsx, nbastatR, rvest)

rm(list=ls())
setwd("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/")

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

game_logs(seasons = 2022, result_types = c("team","players"))

dataGameLogsPlayer <- dataGameLogsPlayer %>% arrange(dateGame,idGame)

players <- dataGameLogsPlayer %>%
    select(16,42,35,51,30) %>%
    group_by(nameTeam,namePlayer) %>%
    summarise(across(where(is.numeric), ~round(mean(.),1)))

# players <- dataGameLogsPlayer %>%
#     select(16,42,17:36) %>%
#     group_by(nameTeam,namePlayer) %>%
#     summarise(across(where(is.numeric), ~round(mean(.),1)))

pl_pts <- players %>%
    group_by(nameTeam) %>%
    filter(pts == max(pts)) %>%
    distinct(nameTeam, .keep_all = TRUE) %>%
    select(1,2,3)

pl_reb <- players %>%
    group_by(nameTeam) %>%
    filter(treb == max(treb)) %>%
    distinct(nameTeam, .keep_all = TRUE) %>%
    select(1,2,4)

pl_ast <- players %>%
    group_by(nameTeam) %>%
    filter(ast == max(ast)) %>%
    distinct(nameTeam, .keep_all = TRUE) %>%
    select(1,2,5)

##### EXPORT TO EXCEL ######

library(XLConnect)

wb <- loadWorkbook("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/Players.xlsx")
setStyleAction(wb,XLC$"STYLE_ACTION.NONE")
clearSheet(wb, sheet = "Points")
clearSheet(wb, sheet = "Rebounds")
clearSheet(wb, sheet = "Assists")
writeWorksheet(wb, pl_pts, "Points")
writeWorksheet(wb, pl_reb, "Rebounds")
writeWorksheet(wb, pl_ast, "Assists")
saveWorkbook(wb, file = "/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/Players.xlsx")


# wb <- createWorkbook()
# addWorksheet(wb, sheetName = "Points")
# addWorksheet(wb, sheetName = "Rebounds")
# addWorksheet(wb, sheetName = "Assists")
# writeData(wb, sheet = "Points", x = pl_pts)
# writeData(wb, sheet = "Rebounds", x = pl_reb)
# writeData(wb, sheet = "Assists", x = pl_ast)
# 
# saveWorkbook(wb, file = "/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/Players.xlsx")

# new players table ----------------------------------
players <- dataGameLogsPlayer %>%
    select(16,42,17:36) %>%
    group_by(nameTeam,namePlayer) %>%
    summarise(across(numberGamePlayerSeason, max), 
              across(c(minutes:fga, fg3m:fg3a, ftm:fta, oreb:pts), ~round(mean(.),1)),
              across(plusminus, sum)) %>%
    mutate(pctFG = round(fgm/fga,3),
           pctFG3 = round(fg3m/fg3a,3),
           pctFT = round(ftm/fta,3))

