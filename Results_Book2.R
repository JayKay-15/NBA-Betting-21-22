######## RESULTS & PERFORMANCE ########

rm(list=ls()[! ls() %in% c("away_final_wt","home_final_wt","league_avg",
                           "kendall_predict", "tyra_predict", "gisele_predict",
                           "kate_predict", "cindy_predict", "naomi_predict",
                           "adriana_predict", "all_models", "slate")])

yd <- as_date("2021-05-10")

#### RESULTS BOOK ####

xl_results_book <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/Results2.xlsx")

yesterday_plays <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/Plays2.xlsx")

game_logs(seasons = 2021, result_types = c("team", "players"))

dataGameLogsTeam <- dataGameLogsTeam %>% arrange(dateGame,idGame)
dataGameLogsTeam$dateGame <- as_date(dataGameLogsTeam$dateGame)
dataGameLogsPlayer <- dataGameLogsPlayer %>% arrange(dateGame,idGame)

### Attach game logs to itself to get all stats for each game in one row

gl <- left_join(dataGameLogsTeam, dataGameLogsTeam, by = c("idGame" = "idGame", "slugTeam" = "slugOpponent"))

gl <- gl %>%
    select(6,13,8,62,17,90,45)
colnames(gl) <- c("GameID", "Date", "Loc", "oppTeam", "Team", "oppScore", "Score")

results_book <- gl %>%
    filter(Date == yd) %>%
    arrange(GameID, Loc)

yesterday_plays <- yesterday_plays[4:63]

results_book <- results_book %>%
    left_join(yesterday_plays, by = "Team")

results_book <- results_book %>%
    mutate(Margin = Score - oppScore) %>%
    mutate(Total = Score + oppScore) %>%
    mutate(ATS_Margin = Margin + Spread) %>%
    mutate(ATS_Result = if_else((Margin + Spread) == 0, 0, if_else(ATS_Margin > 0, 1, -1.1))) %>%
    mutate(ML_Result = case_when(ML > 0 & (Score - oppScore) > 0 ~ ML/100, 
                                 ML > 0 & (Score - oppScore) < 0 ~ -1,
                                 (Score - oppScore) > 0 ~ 1,
                                 (Score - oppScore) < 0 ~ ML/100)) %>%
    mutate(Over_Result = if_else(Total > OU, 1, -1.1)) %>%
    mutate(Under_Result = if_else(Total < OU, 1, -1.1))
    # select(1:9, 38:41, 10:37)



results_book$Kendall_Spread_Result <- with(results_book, ifelse(Kendall_Spread_Edge>0,ATS_Result,0))
results_book$Kendall_ML_Result <- with(results_book, ifelse(Kendall_ML_Edge>0,ML_Result,0))

results_book$Tyra_Spread_Result <- with(results_book, ifelse(Tyra_Spread_Edge>0,ATS_Result,0))
results_book$Tyra_ML_Result <- with(results_book, ifelse(Tyra_ML_Edge>0,ML_Result,0))

results_book$Gisele_Spread_Result <- with(results_book, ifelse(Gisele_Spread_Edge>0,ATS_Result,0))
results_book$Gisele_ML_Result <- with(results_book, ifelse(Gisele_ML_Edge>0,ML_Result,0))

results_book$Kate_Spread_Result <- with(results_book, ifelse(Kate_Spread_Edge>0,ATS_Result,0))
results_book$Kate_ML_Result <- with(results_book, ifelse(Kate_ML_Edge>0,ML_Result,0))

results_book$Cindy_Spread_Result <- with(results_book, ifelse(Cindy_Spread_Edge>0,ATS_Result,0))
results_book$Cindy_ML_Result <- with(results_book, ifelse(Cindy_ML_Edge>0,ML_Result,0))

results_book$Naomi_Spread_Result <- with(results_book, ifelse(Naomi_Spread_Edge>0,ATS_Result,0))
results_book$Naomi_ML_Result <- with(results_book, ifelse(Naomi_ML_Edge>0,ML_Result,0))

results_book$Adriana_Spread_Result <- with(results_book, ifelse(Adriana_Spread_Edge>0,ATS_Result,0))
results_book$Adriana_ML_Result <- with(results_book, ifelse(Adriana_ML_Edge>0,ML_Result,0))

#### Attach to old results book

results_book <- bind_rows(xl_results_book, results_book)

##### CREATE PERFORMANCE & KEYS ####

report_peak <- function(x) {
    x[c(1:max(x$Cume)),]
}

report_builder <- function(Num, Result1, Model1, Model2, Model3, Model4, Model5, Model6, Model7) {
    
    if (Num == 1) {
        results_book %>% 
            filter(.data[[Model1]] > 0) %>%
            mutate(Value = .data[[Model1]]) %>%
            mutate(Result = .data[[Result1]]) %>%
            select(3,56,57) %>%
            arrange(desc(Value)) %>%
            mutate(Cume = cumsum(Result)) %>%
            mutate(gameNum = row_number()) %>%
            select(1,5,2,3,4)
        
    } else if (Num == 2) {
        
        results_book %>% 
            filter(.data[[Model1]] > 0 & .data[[Model2]] > 0) %>%
            mutate(Value = .data[[Model1]]) %>%
            mutate(Result = .data[[Result1]]) %>%
            select(3,56,57) %>%
            arrange(desc(Value)) %>%
            mutate(Cume = cumsum(Result)) %>%
            mutate(gameNum = row_number()) %>%
            select(1,5,2,3,4)
        
    } else if (Num == 3) {
        
        results_book %>% 
            filter(.data[[Model1]] > 0 & .data[[Model2]] > 0 & .data[[Model3]] > 0) %>%
            mutate(Value = .data[[Model1]]) %>%
            mutate(Result = .data[[Result1]]) %>%
            select(3,56,57) %>%
            arrange(desc(Value)) %>%
            mutate(Cume = cumsum(Result)) %>%
            mutate(gameNum = row_number()) %>%
            select(1,5,2,3,4)
        
    } else if (Num == 4) {
        
        results_book %>% 
            filter(.data[[Model1]] > 0 & .data[[Model2]] > 0
                   & .data[[Model3]] > 0
                   & .data[[Model4]] > 0) %>%
            mutate(Value = .data[[Model1]]) %>%
            mutate(Result = .data[[Result1]]) %>%
            select(3,56,57) %>%
            arrange(desc(Value)) %>%
            mutate(Cume = cumsum(Result)) %>%
            mutate(gameNum = row_number()) %>%
            select(1,5,2,3,4)
        
    } else if (Num == 5) {
        
        results_book %>% 
            filter(.data[[Model1]] > 0 & .data[[Model2]] > 0
                   & .data[[Model3]] > 0
                   & .data[[Model4]] > 0
                   & .data[[Model5]] > 0) %>%
            mutate(Value = .data[[Model1]]) %>%
            mutate(Result = .data[[Result1]]) %>%
            select(3,56,57) %>%
            arrange(desc(Value)) %>%
            mutate(Cume = cumsum(Result)) %>%
            mutate(gameNum = row_number()) %>%
            select(1,5,2,3,4)
        
    } else if (Num == 6) {
        
        results_book %>% 
            filter(.data[[Model1]] > 0 & .data[[Model2]] > 0
                   & data[[Model3]] > 0
                   & .data[[Model4]] > 0 
                   & .data[[Model5]] > 0 
                   & .data[[Model6]] > 0) %>%
            mutate(Value = .data[[Model1]]) %>%
            mutate(Result = .data[[Result1]]) %>%
            select(3,56,57) %>%
            arrange(desc(Value)) %>%
            mutate(Cume = cumsum(Result)) %>%
            mutate(gameNum = row_number()) %>%
            select(1,5,2,3,4)
        
    } else if (Num == 7) {
        
        results_book %>% 
            filter(.data[[Model1]] > 0 & .data[[Model2]] > 0
                   & .data[[Model3]] > 0
                   & .data[[Model4]] > 0
                   & .data[[Model5]] > 0 
                   & .data[[Model6]] > 0 
                   & .data[[Model7]] > 0) %>%
            mutate(Value = .data[[Model1]]) %>%
            mutate(Result = .data[[Result1]]) %>%
            select(3,56,57) %>%
            arrange(desc(Value)) %>%
            mutate(Cume = cumsum(Result)) %>%
            mutate(gameNum = row_number()) %>%
            select(1,5,2,3,4)
        
    }
    
}

spread_rb <- report_builder(7, "Kendall_Spread_Result", 
                            "Kendall_Spread_Edge", "Tyra_Spread_Edge","Gisele_Spread_Result",
                            "Kate_Spread_Edge", "Cindy_Spread_Edge", "Naomi_Spread_Edge",
                            "Adriana_Spread_Edge")
spread_rb

spread_rp <- report_peak(rb)
spread_rp

ml_rb <- report_builder(7, "Kendall_ML_Result", 
                        "Kendall_ML_Edge", "Tyra_ML_Edge","Gisele_ML_Result",
                        "Kate_ML_Edge", "Cindy_ML_Edge", "Naomi_ML_Edge",
                        "Adriana_ML_Edge")
ml_rb

ml_rp <- report_peak(rb)
ml_rp


model_key <- tail(spread_rp, 1) %>%
    select(3)

#### GENERATE EDGES ####

plays_a <- slate %>%
    mutate(Loc = "A") %>%
    mutate(Game = row_number()) %>%
    select(5,1,4,3,2)
colnames(plays_a) <- c("game", "Date", "Loc", "oppTeam", "Team")

plays_a <- plays_a %>%
    left_join(kendall_predict, by = c("Team" = "away")) %>%
    left_join(., tyra_predict, by = c("Team" = "away")) %>%
    left_join(., gisele_predict, by = c("Team" = "away")) %>%
    left_join(., kate_predict, by = c("Team" = "away")) %>%
    left_join(., cindy_predict, by = c("Team" = "away")) %>%
    left_join(., naomi_predict, by = c("Team" = "away")) %>%
    left_join(., adriana_predict, by = c("Team" = "away"))

plays_a <- plays_a %>%
    select(1:5, 7, 9, 11, 15, 17, 19, 23, 25, 27, 31, 33, 35, 39, 41, 43, 47, 49, 51, 55, 57, 59)

colnames(plays_a) <- c("game", "Date", "Loc", "oppTeam", "Team", 
                       "Kendall_Margin", "Kendall_Win", "Tyra_Margin", "Tyra_Win",
                       "Gisele_Margin", "Gisele_Win", "Kate_Margin", "Kate_Win",
                       "Cindy_Margin", "Cindy_Win", "Naomi_Margin", "Naomi_Win",
                       "Adriana_Margin", "Adriana_Win")

plays_h <- slate %>%
    mutate(Loc = "H") %>%
    mutate(Game = row_number()) %>%
    select(5,1,4,2,3)
colnames(plays_h) <- c("game", "Date", "Loc", "oppTeam", "Team")

plays_h <- plays_h %>%
    left_join(kendall_predict, by = c("oppTeam" = "away")) %>%
    left_join(., tyra_predict, by = c("oppTeam" = "away")) %>%
    left_join(., gisele_predict, by = c("oppTeam" = "away")) %>%
    left_join(., kate_predict, by = c("oppTeam" = "away")) %>%
    left_join(., cindy_predict, by = c("oppTeam" = "away")) %>%
    left_join(., naomi_predict, by = c("oppTeam" = "away")) %>%
    left_join(., adriana_predict, by = c("oppTeam" = "away"))

plays_h <- plays_h %>%
    select(1:5, 8, 10, 14, 16, 20, 22, 26, 28, 32, 34, 38, 40, 44, 46)

colnames(plays_h) <- c("game", "Date", "Loc", "oppTeam", "Team", 
                       "Kendall_Margin", "Kendall_Win", "Tyra_Margin", "Tyra_Win",
                       "Gisele_Margin", "Gisele_Win", "Kate_Margin", "Kate_Win",
                       "Cindy_Margin", "Cindy_Win", "Naomi_Margin", "Naomi_Win",
                       "Adriana_Margin", "Adriana_Win")

plays <- bind_rows(plays_a, plays_h)

plays <- plays %>%
    arrange(game) %>%
    select(-1)

# bring in odds
odds <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/NBA Odds and Teams.xlsm", 
                  sheet = "Today's Odds")

plays <- plays %>%
    left_join(odds, by = "Team") %>%
    select(1:4, 19, 20, 5:18)

plays$Kendall_Spread_Edge <- with(plays, Kendall_Margin + Spread)
plays$Kendall_ML_Edge <- with(plays, Kendall_Win - round((if_else(ML<0,((ML*-1)/((ML*-1)+100)),(100/(ML+100)))), 3))

plays$Tyra_Spread_Edge <- with(plays, Tyra_Margin + Spread)
plays$Tyra_ML_Edge <- with(plays, Tyra_Win - round((if_else(ML<0,((ML*-1)/((ML*-1)+100)),(100/(ML+100)))), 3))

plays$Gisele_Spread_Edge <- with(plays, Gisele_Margin + Spread)
plays$Gisele_ML_Edge <- with(plays, Gisele_Win - round((if_else(ML<0,((ML*-1)/((ML*-1)+100)),(100/(ML+100)))), 3))

plays$Kate_Spread_Edge <- with(plays, Kate_Margin + Spread)
plays$Kate_ML_Edge <- with(plays, Kate_Win - round((if_else(ML<0,((ML*-1)/((ML*-1)+100)),(100/(ML+100)))), 3))

plays$Cindy_Spread_Edge <- with(plays, Cindy_Margin + Spread)
plays$Cindy_ML_Edge <- with(plays, Cindy_Win - round((if_else(ML<0,((ML*-1)/((ML*-1)+100)),(100/(ML+100)))), 3))

plays$Naomi_Spread_Edge <- with(plays, Naomi_Margin + Spread)
plays$Naomi_ML_Edge <- with(plays, Naomi_Win - round((if_else(ML<0,((ML*-1)/((ML*-1)+100)),(100/(ML+100)))), 3))

plays$Adriana_Spread_Edge <- with(plays, Adriana_Margin + Spread)
plays$Adriana_ML_Edge <- with(plays, Adriana_Win - round((if_else(ML<0,((ML*-1)/((ML*-1)+100)),(100/(ML+100)))), 3))

##### MAKE PLAYS ####

plays$Spread_Play <- with(plays, if_else(Kendall_Spread_Edge   > 0 
                                         & Tyra_Spread_Edge    > 0 
                                         & Gisele_Spread_Edge  > 0
                                         & Kate_Spread_Edge    > 0 
                                         & Cindy_Spread_Edge   > 0 
                                         & Naomi_Spread_Edge   > 0
                                         & Adriana_Spread_Edge > 0
                                         , 1, 0))

plays$ML_Play <- with(plays, if_else(Kendall_ML_Edge   > 0 
                                     & Tyra_ML_Edge    > 0 
                                     & Gisele_ML_Edge  > 0 
                                     & Kate_ML_Edge    > 0 
                                     & Cindy_ML_Edge   > 0 
                                     & Naomi_ML_Edge   > 0 
                                     & Adriana_ML_Edge > 0
                                     , 1, 0))

plays %>%
    filter(Spread_Play == 1) %>%
    select(1,2,4,5)

plays %>%
    filter(ML_Play ==1 ) %>%
    select(1,2,4,6)

##### EXPORT TO EXCEL ######

fn <- "Results"
u <- paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/",fn,".xlsx")

wb <- loadWorkbook("Results.xlsx")
writeData(wb, sheet = "Results Book", x = results_book)
saveWorkbook(wb, file = u, overwrite = TRUE)


fn2 <- "Plays"
u2 <- paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/",fn2,".xlsx")

wb <- loadWorkbook("Plays.xlsx")
writeData(wb, sheet = "Plays", x = plays)
saveWorkbook(wb, file = u2, overwrite = TRUE)


















