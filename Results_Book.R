######## RESULTS & PERFORMANCE ########

if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, readxl, na.tools, caTools, Amelia, lubridate, hms,
               ggthemes, ggrepel, ggimage, XML, RCurl, openxlsx,
               rvest, nflfastR, nbastatR, nbaTools, data.table,
               here, skimr, janitor, SimDesign, zoo, future,
               corrgram, corrplot)
options(dplyr.summarise.inform = FALSE)

rm(list=ls())
setwd("/Users/Jesse/Documents/MyStuff/NBA Database/Database")

td <- as_date("2020-12-22")
fn <- "Results_Book"
u <- paste0("/Users/Jesse/Documents/MyStuff/NBA Database/Database/",fn,".xlsx")

game_logs(seasons = 2021, result_types = c("team","players"))

dataGameLogsTeam <- dataGameLogsTeam %>% arrange(dateGame,idGame)
dataGameLogsPlayer <- dataGameLogsPlayer %>% arrange(dateGame,idGame)
dataGameLogsTeam$dateGame <- as_date(dataGameLogsTeam$dateGame)

### Attach game logs to itself to get all stats for each game in one row

gl <- left_join(dataGameLogsTeam, dataGameLogsTeam, by = c("idGame" = "idGame", "slugTeam" = "slugOpponent"))

gl <- gl %>% 
    select(13,8,62,17,90,45)

colnames(gl) <- c("Date", "Loc", "oppTeam", "Team", "oppScore", "Score")

today_games <- gl %>%
    filter(Date == td)

### Bring in odds

nba_odds <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Database/Database/NBAodds_mock.xlsx")
nba_odds$Date<- as_date(nba_odds$Date)

results_book <- left_join(today_games, nba_odds, by = c("Date" = "Date", "Team" = "Team"), suffix = c("", ".y"))

#### CREATE RESULTS BOOK ####

results_book <- results_book %>%
    mutate(gameNum = row_number(Date)) %>%
    mutate(Margin = Score - oppScore) %>%
    mutate(ATS_Margin = Margin + Spread) %>%
    mutate(ATS_Result = if_else((Margin + Spread) == 0, 0, if_else(ATS_Margin > 0, 1, -1.1))) %>%
    mutate(ML_Results = case_when(ML > 0 & (Score - oppScore) > 0 ~ ML/100, 
                                  ML > 0 & (Score - oppScore) < 0 ~ -1,
                                  (Score - oppScore) > 0 ~ 1,
                                  (Score - oppScore) < 0 ~ ML/100)) %>%
    select(10,1:6, 8, 9, 11:14)


# bring in model edges





# bring in results book printed to Excel





##### EXPORT TO EXCEL ######

# wb <- createWorkbook()
# addWorksheet(wb, sheetName = "final_db")
# writeData(wb, sheet = "final_db", x = final_db)
# 
# saveWorkbook(wb, file = u)






##### CREATE PERFORMANCE & KEYS ####










