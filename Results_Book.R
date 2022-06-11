######## RESULTS & PERFORMANCE ########

rm(list=ls()[! ls() %in% c("away_final_wt","home_final_wt","league_avg","standings",
                           "kendall_predict", "tyra_predict", "gisele_predict",
                           "kate_predict", "cindy_predict", "naomi_predict",
                           "adriana_predict", "all_models", "slate")])

library(openxlsx)

yd <- as_date("2022-05-10")

#### Keys ####

### spread1
kendall_spread1 <-  .0
tyra_spread1 <-     .0
gisele_spread1 <-   .0
kate_spread1 <-    3.923
cindy_spread1 <-    .0
naomi_spread1 <-    .0
adriana_spread1 <-  .0

### spread2
kendall_spread2 <-  .0
tyra_spread2 <-     .0 # 4.49
gisele_spread2 <-   .0
kate_spread2 <-     .0
cindy_spread2 <-    .0
naomi_spread2 <-    .0
adriana_spread2 <-  .0

### ml
kendall_ml <-   .0 # .053
tyra_ml <-      .0
gisele_ml <-    .098
kate_ml <-      .0
cindy_ml <-     .0
naomi_ml <-     .0
adriana_ml <-   .0

### over
kendall_over <- .0
tyra_over <-    .0
gisele_over <-  .0
kate_over <-  12.939 # 4.07
cindy_over <-   .0
naomi_over <-   .0
adriana_over <- .0

### under
kendall_under <-2.648
tyra_under <-    .0
gisele_under <-  .0 # 8.603
kate_under <-    .0
cindy_under <-   .0
naomi_under <-   .0
adriana_under <- .0

#### RESULTS BOOK ####

xl_results_book <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/Results.xlsx")
xl_results_book$Date <- as_date(xl_results_book$Date)

yesterday_plays <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/Plays.xlsx")

game_logs(seasons = 2022, result_types = c("team", "players"), season_types = "Playoffs")

dataGameLogsTeam <- dataGameLogsTeam %>% arrange(dateGame,idGame)
dataGameLogsTeam$dateGame <- as_date(dataGameLogsTeam$dateGame)
dataGameLogsPlayer <- dataGameLogsPlayer %>% arrange(dateGame,idGame)

### Attach game logs to itself to get all stats for each game in one row

gl <- left_join(dataGameLogsTeam, dataGameLogsTeam, by = c("idGame" = "idGame", "slugTeam" = "slugOpponent"))

gl <- gl %>%
    select(6,13,8,62,17,90,45)
colnames(gl) <- c("idGame", "Date", "Loc", "oppTeam", "Team", "oppScore", "Score")

results_book <- gl %>%
    filter(Date == yd) %>%
    arrange(idGame, Loc)

yesterday_plays <- yesterday_plays[4:71]

results_book <- results_book %>%
    left_join(yesterday_plays, by = "Team")

results_book <- results_book %>%
    mutate(Margin = Score - oppScore) %>%
    mutate(Game_Total = Score + oppScore) %>%
    mutate(ATS_Margin = Margin + Spread) %>%
    mutate(ATS_Result = if_else((Margin + Spread) == 0, 0, if_else(ATS_Margin > 0, 1, -1.1))) %>%
    mutate(ML_Result = case_when(ML > 0 & (Score - oppScore) > 0 ~ ML/100, 
                                 ML > 0 & (Score - oppScore) < 0 ~ -1,
                                 (Score - oppScore) > 0 ~ 1,
                                 (Score - oppScore) < 0 ~ ML/100)) %>%
    mutate(Over_Result = if_else((Game_Total - Total) == 0, 0, if_else(Game_Total > Total, 1, -1.1))) %>%
    mutate(Under_Result = if_else((Game_Total - Total) == 0, 0, if_else(Game_Total < Total, 1, -1.1))) %>%
    select(1:7, 9:11, 75:81, 12:74) %>%
    rename(oppTeam = oppTeam.x)

results_book$Kendall_Spread_Result <- with(results_book, ifelse(Kendall_Spread_Edge>0,ATS_Result,0))
results_book$Kendall_Spread2_Result <- with(results_book, ifelse(Kendall_Spread2_Edge>0,ATS_Result,0))
results_book$Kendall_ML_Result <- with(results_book, ifelse(Kendall_ML_Edge>0,ML_Result,0))
results_book$Kendall_Over_Result <- with(results_book, ifelse(Kendall_Over_Edge>0,Over_Result,0))
results_book$Kendall_Under_Result <- with(results_book, ifelse(Kendall_Under_Edge>0,Under_Result,0))

results_book$Tyra_Spread_Result <- with(results_book, ifelse(Tyra_Spread_Edge>0,ATS_Result,0))
results_book$Tyra_Spread2_Result <- with(results_book, ifelse(Tyra_Spread2_Edge>0,ATS_Result,0))
results_book$Tyra_ML_Result <- with(results_book, ifelse(Tyra_ML_Edge>0,ML_Result,0))
results_book$Tyra_Over_Result <- with(results_book, ifelse(Tyra_Over_Edge>0,Over_Result,0))
results_book$Tyra_Under_Result <- with(results_book, ifelse(Tyra_Under_Edge>0,Under_Result,0))

results_book$Gisele_Spread_Result <- with(results_book, ifelse(Gisele_Spread_Edge>0,ATS_Result,0))
results_book$Gisele_Spread2_Result <- with(results_book, ifelse(Gisele_Spread2_Edge>0,ATS_Result,0))
results_book$Gisele_ML_Result <- with(results_book, ifelse(Gisele_ML_Edge>0,ML_Result,0))
results_book$Gisele_Over_Result <- with(results_book, ifelse(Gisele_Over_Edge>0,Over_Result,0))
results_book$Gisele_Under_Result <- with(results_book, ifelse(Gisele_Under_Edge>0,Under_Result,0))

results_book$Kate_Spread_Result <- with(results_book, ifelse(Kate_Spread_Edge>0,ATS_Result,0))
results_book$Kate_Spread2_Result <- with(results_book, ifelse(Kate_Spread2_Edge>0,ATS_Result,0))
results_book$Kate_ML_Result <- with(results_book, ifelse(Kate_ML_Edge>0,ML_Result,0))
results_book$Kate_Over_Result <- with(results_book, ifelse(Kate_Over_Edge>0,Over_Result,0))
results_book$Kate_Under_Result <- with(results_book, ifelse(Kate_Under_Edge>0,Under_Result,0))

results_book$Cindy_Spread_Result <- with(results_book, ifelse(Cindy_Spread_Edge>0,ATS_Result,0))
results_book$Cindy_Spread2_Result <- with(results_book, ifelse(Cindy_Spread2_Edge>0,ATS_Result,0))
results_book$Cindy_ML_Result <- with(results_book, ifelse(Cindy_ML_Edge>0,ML_Result,0))
results_book$Cindy_Over_Result <- with(results_book, ifelse(Cindy_Over_Edge>0,Over_Result,0))
results_book$Cindy_Under_Result <- with(results_book, ifelse(Cindy_Under_Edge>0,Under_Result,0))

results_book$Naomi_Spread_Result <- with(results_book, ifelse(Naomi_Spread_Edge>0,ATS_Result,0))
results_book$Naomi_Spread2_Result <- with(results_book, ifelse(Naomi_Spread2_Edge>0,ATS_Result,0))
results_book$Naomi_ML_Result <- with(results_book, ifelse(Naomi_ML_Edge>0,ML_Result,0))
results_book$Naomi_Over_Result <- with(results_book, ifelse(Naomi_Over_Edge>0,Over_Result,0))
results_book$Naomi_Under_Result <- with(results_book, ifelse(Naomi_Under_Edge>0,Under_Result,0))

results_book$Adriana_Spread_Result <- with(results_book, ifelse(Adriana_Spread_Edge>0,ATS_Result,0))
results_book$Adriana_Spread2_Result <- with(results_book, ifelse(Adriana_Spread2_Edge>0,ATS_Result,0))
results_book$Adriana_ML_Result <- with(results_book, ifelse(Adriana_ML_Edge>0,ML_Result,0))
results_book$Adriana_Over_Result <- with(results_book, ifelse(Adriana_Over_Edge>0,Over_Result,0))
results_book$Adriana_Under_Result <- with(results_book, ifelse(Adriana_Under_Edge>0,Under_Result,0))

#### Attach to old results book

results_book <- bind_rows(xl_results_book, results_book)

#### GENERATE EDGES ####

plays_a <- slate %>%
    mutate(Loc = "A") %>%
    select(1,4,6,3,2)
colnames(plays_a) <- c("idGame", "Date", "Loc", "oppTeam", "Team")

plays_a <- plays_a %>%
    left_join(kendall_predict, by = c("Team" = "Away")) %>%
    left_join(., tyra_predict, by = c("Team" = "Away")) %>%
    left_join(., gisele_predict, by = c("Team" = "Away")) %>%
    left_join(., kate_predict, by = c("Team" = "Away")) %>%
    left_join(., cindy_predict, by = c("Team" = "Away")) %>%
    left_join(., naomi_predict, by = c("Team" = "Away")) %>%
    left_join(., adriana_predict, by = c("Team" = "Away"))

plays_a <- plays_a %>%
    select(1:5, 
           7, 9, 11, 13,
           16, 18, 20, 22, 
           25, 27, 29, 31, 
           34, 36, 38, 40, 
           43, 45, 47, 49, 
           52, 54, 56, 58,
           61, 63, 65, 67)

colnames(plays_a) <- c("idGame", "Date", "Loc", "oppTeam", "Team", 
                       "Kendall_Margin", "Kendall_Margin2", "Kendall_Win", "Kendall_Total",
                       "Tyra_Margin", "Tyra_Margin2", "Tyra_Win", "Tyra_Total",
                       "Gisele_Margin", "Gisele_Margin2", "Gisele_Win", "Gisele_Total",
                       "Kate_Margin", "Kate_Margin2", "Kate_Win", "Kate_Total",
                       "Cindy_Margin", "Cindy_Margin2", "Cindy_Win", "Cindy_Total",
                       "Naomi_Margin", "Naomi_Margin2", "Naomi_Win", "Naomi_Total",
                       "Adriana_Margin", "Adriana_Margin2", "Adriana_Win", "Adriana_Total")

plays_h <- slate %>%
    mutate(Loc = "H") %>%
    select(1,4,6,2,3)
colnames(plays_h) <- c("idGame", "Date", "Loc", "oppTeam", "Team")

plays_h <- plays_h %>%
    left_join(kendall_predict, by = c("oppTeam" = "Away")) %>%
    left_join(., tyra_predict, by = c("oppTeam" = "Away")) %>%
    left_join(., gisele_predict, by = c("oppTeam" = "Away")) %>%
    left_join(., kate_predict, by = c("oppTeam" = "Away")) %>%
    left_join(., cindy_predict, by = c("oppTeam" = "Away")) %>%
    left_join(., naomi_predict, by = c("oppTeam" = "Away")) %>%
    left_join(., adriana_predict, by = c("oppTeam" = "Away"))

plays_h <- plays_h %>%
    select(1:5, 
           8, 10, 12, 13,
           17, 19, 21, 22, 
           26, 28, 30, 31, 
           35, 37, 39, 40, 
           44, 46, 48, 49, 
           53, 55, 57, 58,
           62, 64, 66, 67)

colnames(plays_h) <- c("idGame", "Date", "Loc", "oppTeam", "Team", 
                       "Kendall_Margin", "Kendall_Margin2", "Kendall_Win", "Kendall_Total",
                       "Tyra_Margin", "Tyra_Margin2", "Tyra_Win", "Tyra_Total",
                       "Gisele_Margin", "Gisele_Margin2", "Gisele_Win", "Gisele_Total",
                       "Kate_Margin", "Kate_Margin2", "Kate_Win", "Kate_Total",
                       "Cindy_Margin", "Cindy_Margin2", "Cindy_Win", "Cindy_Total",
                       "Naomi_Margin", "Naomi_Margin2", "Naomi_Win", "Naomi_Total",
                       "Adriana_Margin", "Adriana_Margin2", "Adriana_Win", "Adriana_Total")

plays <- bind_rows(plays_a, plays_h)

plays <- plays %>%
    arrange(idGame)

# bring in odds
odds <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/NBA Odds and Teams.xlsm", 
                  sheet = "Today's Odds")

odds <- odds %>%
    mutate(across(where(is.character), str_replace_all, pattern = "L.A. Clippers", replacement = "LA Clippers"))

plays <- plays %>%
    left_join(odds, by = "Team") %>%
    select(1:5, 34, 35, 36, 6:33)

plays$Kendall_Spread_Edge <- with(plays, Kendall_Margin + Spread)
plays$Kendall_Spread2_Edge <- with(plays, Kendall_Margin2 + Spread)
plays$Kendall_ML_Edge <- with(plays, Kendall_Win - round((if_else(ML<0,((ML*-1)/((ML*-1)+100)),(100/(ML+100)))), 3))
plays$Kendall_Over_Edge <- with(plays, Kendall_Total - Total)
plays$Kendall_Under_Edge <- with(plays, Total - Kendall_Total)

plays$Tyra_Spread_Edge <- with(plays, Tyra_Margin + Spread)
plays$Tyra_Spread2_Edge <- with(plays, Tyra_Margin2 + Spread)
plays$Tyra_ML_Edge <- with(plays, Tyra_Win - round((if_else(ML<0,((ML*-1)/((ML*-1)+100)),(100/(ML+100)))), 3))
plays$Tyra_Over_Edge <- with(plays, Tyra_Total - Total)
plays$Tyra_Under_Edge <- with(plays, Total - Tyra_Total)

plays$Gisele_Spread_Edge <- with(plays, Gisele_Margin + Spread)
plays$Gisele_Spread2_Edge <- with(plays, Gisele_Margin2 + Spread)
plays$Gisele_ML_Edge <- with(plays, Gisele_Win - round((if_else(ML<0,((ML*-1)/((ML*-1)+100)),(100/(ML+100)))), 3))
plays$Gisele_Over_Edge <- with(plays, Gisele_Total - Total)
plays$Gisele_Under_Edge <- with(plays, Total - Gisele_Total)

plays$Kate_Spread_Edge <- with(plays, Kate_Margin + Spread)
plays$Kate_Spread2_Edge <- with(plays, Kate_Margin2 + Spread)
plays$Kate_ML_Edge <- with(plays, Kate_Win - round((if_else(ML<0,((ML*-1)/((ML*-1)+100)),(100/(ML+100)))), 3))
plays$Kate_Over_Edge <- with(plays, Kate_Total - Total)
plays$Kate_Under_Edge <- with(plays, Total - Kate_Total)

plays$Cindy_Spread_Edge <- with(plays, Cindy_Margin + Spread)
plays$Cindy_Spread2_Edge <- with(plays, Cindy_Margin2 + Spread)
plays$Cindy_ML_Edge <- with(plays, Cindy_Win - round((if_else(ML<0,((ML*-1)/((ML*-1)+100)),(100/(ML+100)))), 3))
plays$Cindy_Over_Edge <- with(plays, Cindy_Total - Total)
plays$Cindy_Under_Edge <- with(plays, Total - Cindy_Total)

plays$Naomi_Spread_Edge <- with(plays, Naomi_Margin + Spread)
plays$Naomi_Spread2_Edge <- with(plays, Naomi_Margin2 + Spread)
plays$Naomi_ML_Edge <- with(plays, Naomi_Win - round((if_else(ML<0,((ML*-1)/((ML*-1)+100)),(100/(ML+100)))), 3))
plays$Naomi_Over_Edge <- with(plays, Naomi_Total - Total)
plays$Naomi_Under_Edge <- with(plays, Total - Naomi_Total)

plays$Adriana_Spread_Edge <- with(plays, Adriana_Margin + Spread)
plays$Adriana_Spread2_Edge <- with(plays, Adriana_Margin2 + Spread)
plays$Adriana_ML_Edge <- with(plays, Adriana_Win - round((if_else(ML<0,((ML*-1)/((ML*-1)+100)),(100/(ML+100)))), 3))
plays$Adriana_Over_Edge <- with(plays, Adriana_Total - Total)
plays$Adriana_Under_Edge <- with(plays, Total - Adriana_Total)

##### MAKE PLAYS ####

plays$Spread_Play <- with(plays, if_else(
                                        # Kendall_Spread_Edge   >= kendall_spread1
                                        # Tyra_Spread_Edge    >= tyra_spread1
                                        # Gisele_Spread_Edge  >= gisele_spread1
                                        Kate_Spread_Edge    >= kate_spread1
                                        # Cindy_Spread_Edge   >= cindy_spread1
                                        # Naomi_Spread_Edge   >= naomi_spread1
                                        # Adriana_Spread_Edge >= adriana_spread1
                                         ,1 , 0))

plays$Spread2_Play <- with(plays, if_else(
                                        #Kendall_Spread2_Edge   >= kendall_spread2
                                        Tyra_Spread2_Edge    >= tyra_spread2
                                        # Gisele_Spread2_Edge  >= gisele_spread2
                                        # Kate_Spread2_Edge    >= kate_spread2
                                        # Cindy_Spread2_Edge   >= cindy_spread2
                                        # Naomi_Spread2_Edge   >= naomi_spread2
                                        # Adriana_Spread2_Edge >= adriana_spread2
                                         ,0 , 0))

plays$ML_Play <- with(plays, if_else(
                                    # Kendall_ML_Edge   > kendall_ml
                                    # Tyra_ML_Edge    > tyra_ml
                                    Gisele_ML_Edge  > gisele_ml
                                    # Kate_ML_Edge    > kate_ml
                                    # Cindy_ML_Edge   > cindy_ml
                                    # Naomi_ML_Edge   > naomi_ml
                                    # Adriana_ML_Edge > adriana_ml
                                     ,1 , 0))

plays$Over_Play <- with(plays, if_else(
                                        # Kendall_Over_Edge   >= kendall_over
                                        # Tyra_Over_Edge    >= tyra_over
                                        # Gisele_Over_Edge  >= gisele_over
                                        Kate_Over_Edge    >= kate_over
                                        # Cindy_Over_Edge   >= cindy_over
                                        # Naomi_Over_Edge   >= naomi_over
                                        # Adriana_Over_Edge >= adriana_over
                                         ,1 , 0))

plays$Under_Play <- with(plays, if_else(
                                      Kendall_Under_Edge   >= kendall_under
                                      # Tyra_Under_Edge    >= tyra_under
                                      # Gisele_Under_Edge  >= gisele_under
                                      # Kate_Under_Edge    >= kate_under
                                      # Cindy_Under_Edge   >= cindy_under
                                      # Naomi_Under_Edge   >= naomi_under
                                      # Adriana_Under_Edge >= adriana_under
                                       ,1, 0))

##### Fix Output #####

plays[, c(29:32,62:66)] <- sapply(plays[, c(29:32,62:66)], as.numeric)

##### Bets #####

bets <- plays %>%
    filter(Spread_Play == 1 | Spread2_Play == 1 | ML_Play == 1 | Over_Play == 1 | Under_Play == 1) %>%
    mutate(Spread_Bet = if_else(Spread_Play == 1 | Spread2_Play == 1, 1, 0)) %>%
    mutate(ML_Bet = if_else(ML_Play == 1, 1, 0)) %>%
    mutate(Over_Bet = if_else(Over_Play == 1, 1, 0)) %>%
    mutate(Under_Bet = if_else(Under_Play == 1, 1, 0)) %>%
    rename(Spread_Odds = Spread, ML_Odds = ML, Total_Odds = Total) %>%
    select(2,3,5:8, 77:80)

bets <- bets %>%
    pivot_longer(cols = 4:10, names_to = c("Bet_Type", ".value"),  names_sep="_") %>%
    filter(Bet == 1) %>%
    select(1:5)

##### EXPORT TO EXCEL ######
detach("package:XLConnect", unload = TRUE)

fn <- "Results"
u <- paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/",fn,".xlsx")

wb <- loadWorkbook("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/Results.xlsx")
writeData(wb, "Results Book", x = results_book)
saveWorkbook(wb, u, overwrite = T)


fn2 <- "Plays"
u2 <- paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/",fn2,".xlsx")

wb <- loadWorkbook("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/Plays.xlsx")
deleteData(wb, "Plays", gridExpand = T, cols = 1:76, rows = 1:50)
writeData(wb, "Plays", x = plays)
saveWorkbook(wb, u2, overwrite = T)

fn <- "Bets"
u <- paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/",fn,".xlsx")

wb <- createWorkbook()
addWorksheet(wb, sheetName = "Website Bets")
writeData(wb, sheet = "Website Bets", x = bets)
saveWorkbook(wb, file = u)

#### Creating workbook

# fn <- "Results"
# u <- paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/",fn,".xlsx")
# 
# wb <- createWorkbook()
# addWorksheet(wb, sheetName = "Results Book")
# writeData(wb, sheet = "Results Book", x = results_book)
# saveWorkbook(wb, file = u)
# 
# fn2 <- "Plays"
# u2 <- paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/",fn2,".xlsx")
# 
# wb <- createWorkbook()
# addWorksheet(wb, sheetName = "Plays")
# writeData(wb, sheet = "Plays", x = plays)
# saveWorkbook(wb, file = u2)


