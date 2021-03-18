######## Standings & Scores ######## 2021-2022

if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, readxl, na.tools, caTools, Amelia, 
               ggthemes, ggrepel, ggimage, XML, RCurl, openxlsx, 
               rvest, nflfastR, nbastatR, nbaTools)

rm(list=ls())
setwd("/Users/Jesse/Documents/MyStuff/NBA Database/2020-2021")

y <- "2021-03-14"
cur_date <- "Scores & Standings"
u <- paste0("/Users/Jesse/Documents/MyStuff/NBA Database/2020-2021/",cur_date,".xlsx")

game_logs(seasons = 2021, result_types = c("team","players"))

dataGameLogsTeam <- dataGameLogsTeam %>% arrange(dateGame,idGame)
dataGameLogsPlayer <- dataGameLogsPlayer %>% arrange(dateGame,idGame)

df <- left_join(dataGameLogsTeam, dataGameLogsTeam, by = c("idGame" = "idGame", "slugTeam" = "slugOpponent"))

#### Scores ####

df <- df %>% 
    filter(locationGame.x == "H")

scores <- df %>%
    select(13,8,62,17,90,45)

colnames(scores) <- c("Date","Loc","Away","Home","Away Score","Home Score")

scores$Away <- replace(scores$Away, scores$Away == "LA Clippers","Los Angeles Clippers")
scores$Home <- replace(scores$Home, scores$Home == "LA Clippers","Los Angeles Clippers")

#### Standings ####

standings <- standings(seasons = 2021, return_message = TRUE)

standings$nameTeam <- replace(standings$nameTeam, standings$nameTeam == "LA Clippers","Los Angeles Clippers")

standings <- standings %>% 
    select(4,22) %>%
    separate(recordOverall,c("Wins","Losses"), sep="-")

colnames(standings) [1] <- "Team"

#### Yesterday's Games ####

yesterday <- scores %>%
    filter(Date == y)

a_y <- yesterday %>%
    select(1,3,5) %>%
    mutate(Loc = "A") %>%
    select(1,4,2,3)

colnames(a_y) <- c("Date","Loc","Team","Score")

h_y <- yesterday %>%
    select(1,2,4,6)

colnames(h_y) <- c("Date","Loc","Team","Score")

yesterday <- bind_rows(a_y,h_y)

##### PRINTING TO EXCEL #####

wb <- loadWorkbook('Scores & Standings.xlsx')
writeData(wb, sheet = "Scores", x = scores)
writeData(wb, sheet = "Standings", x = standings)
writeData(wb, sheet = "Yesterday", x = yesterday)

saveWorkbook(wb, file = u, 
             overwrite = T)


# wb <- createWorkbook()
# addWorksheet(wb, sheetName = "Scores")
# addWorksheet(wb, sheetName = "Standings")
# 
# writeData(wb, sheet = "Scores", x = scores)
# writeData(wb, sheet = "Standings", x = standings)
# 
# saveWorkbook(wb, file = u)
