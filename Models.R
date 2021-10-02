################
#### MODELS ####
################

rm(list=ls()[! ls() %in% c("away_final_wt","home_final_wt","league_avg","standings")])

library(progress)

#### Slate ####

td <- as_date("2021-05-07")

get_slate <- function(year, month) {
    
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, 
                  "_games-", month, ".html")
    
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_2021_games-may.html")

    webpage <- read_html(url)
    
    col_names <- webpage %>% 
        html_nodes("table#schedule > thead > tr > th") %>% 
        html_attr("data-stat")    
    col_names <- c("game_id", col_names)
    
    dates <- webpage %>% 
        html_nodes("table#schedule > tbody > tr > th") %>% 
        html_text()
    dates <- dates[dates != "Playoffs"]
    
    game_id <- webpage %>% 
        html_nodes("table#schedule > tbody > tr > th") %>%
        html_attr("csk")
    game_id <- game_id[!is.na(game_id)]
    
    data <- webpage %>% 
        html_nodes("table#schedule > tbody > tr > td") %>% 
        html_text() %>%
        matrix(ncol = length(col_names) - 2, byrow = TRUE)
    
    slate <- as.data.frame(cbind(game_id, dates, data), stringsAsFactors = FALSE)
    names(slate) <- col_names
    
    slate <- slate %>% select(2,3,4,6)
    slate$date_game <- mdy(slate$date_game)
    colnames(slate) <- c("date","time", "away", "home")
    assign("slate", slate, envir = .GlobalEnv)
    
    # # change columns to the correct types
    # df$visitor_pts <- as.numeric(df$visitor_pts)
    # df$home_pts    <- as.numeric(df$home_pts)
    # df$attendance  <- as.numeric(gsub(",", "", df$attendance))
    # df$date_game   <- mdy(df$date_game)
    # 
    # # drop boxscore column
    # df$box_score_text <- NULL
    
}

slate_all <- get_slate("2021", "may") ## add date to function? don't have to filter then?
slate_all <- slate %>% filter(date == td)
slate <- slate_all %>% select(1,3,4)


master_db <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb1721.xlsx") 

#### Kendall #### - Rating and Pythag formulas

pb <- progress_bar$new(
    format = "  running model... [:bar] :percent eta: :eta",
    total = nrow(slate), clear = FALSE, width = 60, show_after = 0)
invisible(pb$tick(0))

kendall_predict <- data.frame() # Predictions frame

a <- 1
g <- nrow(slate)

for (a in a:g) {
    
    slate_away <- as.character(slate[a,2])
    slate_home <- as.character(slate[a,3])
    
    kendall_away <- away_final_wt %>%
        filter(Team == slate_away)
    
    kendall_home <- home_final_wt %>%
        filter(Team == slate_home)
    
    kendall_away_oe <- as.numeric(kendall_away[,34])
    kendall_away_de <- as.numeric(kendall_away[,35])
    kendall_away_pa <- as.numeric(kendall_away[,36])
    
    kendall_home_oe <- as.numeric(kendall_home[,34])
    kendall_home_de <- as.numeric(kendall_home[,35])
    kendall_home_pa <- as.numeric(kendall_home[,36])
    
    lg_pace <- as.numeric(league_avg[1,34])
    lg_oe <- as.numeric(league_avg[1,36])
    
    away_pace_vslg <- kendall_away_pa - lg_pace
    home_pace_vslg <- kendall_home_pa - lg_pace
    vslg_sums <- away_pace_vslg + home_pace_vslg
    
    expected_pace <- (lg_pace + vslg_sums)
    
    away_oe_vslg <- kendall_away_oe - lg_oe
    away_de_vslg <- kendall_away_de - lg_oe
    home_oe_vslg <- kendall_home_oe - lg_oe
    home_de_vslg <- kendall_home_de - lg_oe
    
    away_oe_vslgsums <- away_oe_vslg + home_de_vslg
    home_oe_vslgsums <- home_oe_vslg + away_de_vslg
    
    away_proj_oe <- lg_oe + away_oe_vslgsums
    home_proj_oe <- lg_oe + home_oe_vslgsums
    
    # away_proj_oe <- away_proj_oe * as.numeric(advantage_percent[1,2])
    # home_proj_oe <- home_proj_oe * as.numeric(advantage_percent[1,1])
    
    away_proj_oe <- away_proj_oe / 100
    home_proj_oe <- home_proj_oe / 100
    
    away_kendall_score <- (away_proj_oe * expected_pace) #+ as.numeric(advantage_mx[1,2])
    home_kendall_score <- (home_proj_oe * expected_pace) #+ as.numeric(advantage_mx[1,2])
    
    away_kendall_win <- (away_proj_oe ^ 14.23) / ((away_proj_oe ^ 14.23) + (home_proj_oe ^ 14.23))
    home_kendall_win <- 1 - away_kendall_win
    
    holder <- slate[a,2:3]
    holder$Away_Margin <- away_kendall_score - home_kendall_score
    holder$Home_Margin <- home_kendall_score - away_kendall_score
    holder$Away_Win <- away_kendall_win
    holder$Home_Win <- home_kendall_win
    # holder$Away_score <- away_kendall_score
    # holder$Home_score <- home_kendall_score
    
    kendall_predict <- bind_rows(kendall_predict,holder)
    
    pb$tick()
    Sys.sleep(1 / nrow(slate))
    
}

kendall_predict <- kendall_predict %>%
    mutate(across(where(is.numeric), round, 3))


#### Tyra #### - Least Squares

pb <- progress_bar$new(
    format = "  running model... [:bar] :percent eta: :eta",
    total = nrow(slate), clear = FALSE, width = 60, show_after = 0)
invisible(pb$tick(0))

lin_fit <- lm(Margin ~ ORtg_away + DRtg_away + ORtg_home + DRtg_home + SR2_away + FG_home + oBLK_home + 
              DRB_home + oORB_home + oeFG_home + oDRB_away + PF_home + oFG3_away + oAST_away + oTRB_away + 
              oORB_away, data = master_db)

log_fit <- glm(Win ~ ORtg_away + DRtg_away + ORtg_home + DRtg_home + Pace_home + ORB_home + 
               oeFG_home + DRB_home + oTOV_home, data = master_db, family = "binomial")

tyra_predict <- data.frame() # Predictions frame

a <- 1
g <- nrow(slate)

for (a in a:g) {
    
    slate_away <- as.character(slate[a,2])
    slate_home <- as.character(slate[a,3])
    
    tyra_away_lin <- away_final_wt %>%
        filter(Team == slate_away) %>%
        select(ORtg, DRtg, SR2, oDRB, oFG3, oAST, oTRB, oORB)
    
    tyra_home_lin <- home_final_wt %>%
        filter(Team == slate_home) %>%
        select(ORtg, DRtg, FG, oBLK, DRB, oORB, oeFG, PF)
    
    tyra_away_log <- away_final_wt %>%
        filter(Team == slate_away) %>%
        select(ORtg, DRtg)
    
    tyra_home_log <- home_final_wt %>%
        filter(Team == slate_home) %>%
        select(ORtg, DRtg, Pace, ORB, oeFG, DRB, oTOV)
    
    lin_input <- cbind(tyra_away_lin, tyra_home_lin)
    
    colnames(lin_input) <- c("ORtg_away", "DRtg_away", "SR2_away", "oDRB_away", 
                              "oFG3_away", "oAST_away", "oTRB_away", "oORB_away",
                              "ORtg_home", "DRtg_home",
                              "FG_home", "oBLK_home", "DRB_home", "oORB_home",
                              "oeFG_home", "PF_home")
    
    log_input <- cbind(tyra_away_log, tyra_home_log)
    
    colnames(log_input) <- c("ORtg_away", "DRtg_away",
                              "ORtg_home", "DRtg_home", "Pace_home",
                              "ORB_home", "oeFG_home", "DRB_home", "oTOV_home")
    
    tyra_margin <- as.numeric(predict(lin_fit, newdata = lin_input))
    # tyra_ascore <- as.numeric(predict(ascore_fit, newdata = lin_input))
    # tyra_hscore <- as.numeric(predict(hscore_fit, newdata = lin_input))
    
    tyra_awin <- as.numeric(predict(log_fit, newdata = log_input, type = "response"))
    tyra_hwin <- 1 - tyra_awin
    
    holder <- slate[a,2:3]
    holder$Away_Margin <- tyra_margin
    holder$Home_Margin <- tyra_margin*-1
    holder$Away_Win <- tyra_awin
    holder$Home_Win <- tyra_hwin
    # holder$Away_score <- tyra_ascore
    # holder$Home_score <- tyra_hscore

    tyra_predict <- bind_rows(tyra_predict,holder)
    
    pb$tick()
    Sys.sleep(1 / nrow(slate))
    
}

tyra_predict <- tyra_predict %>%
    mutate(across(where(is.numeric), round, 3))


#### Gisele #### - K Nearest Neighbors

# library(class)
library(caret)

pb <- progress_bar$new(
    format = "  running model... [:bar] :percent eta: :eta",
    total = nrow(slate), clear = FALSE, width = 60, show_after = 0)
invisible(pb$tick(0))

knnr_fit <- master_db %>%
    select(Margin, ORtg_away, DRtg_away, ORtg_home, DRtg_home, ORB_home, TOV_home, oeFG_home, DRB_home, oTOV_home)

knn_fit <- master_db %>%
    select(Win, ORtg_home, ORtg_away, DRtg_home, DRtg_away, ORB_home, TOV_home, oeFG_home, DRB_home, oTOV_home)

gisele_predict <- data.frame() # Predictions frame

a <- 1
g <- nrow(slate)

for (a in a:g) {
    
    slate_away <- as.character(slate[a,2])
    slate_home <- as.character(slate[a,3])
    
    gisele_away_knnr <- away_final_wt %>%
        filter(Team == slate_away) %>%
        select(ORtg, DRtg)
    
    gisele_home_knnr <- home_final_wt %>%
        filter(Team == slate_home) %>%
        select(ORtg, DRtg, ORB, TOV, oeFG, DRB, oTOV)
    
    gisele_away_knn <- away_final_wt %>%
        filter(Team == slate_away) %>%
        select(ORtg, DRtg)
    
    gisele_home_knn <- home_final_wt %>%
        filter(Team == slate_home) %>%
        select(ORtg, DRtg, ORB, TOV, oeFG, DRB, oTOV)
    
    knnr_input <- cbind(gisele_away_knnr, gisele_home_knnr)
    colnames(knnr_input) <- c("ORtg_away", "DRtg_away",
                              "ORtg_home", "DRtg_home",
                              "ORB_home", "TOV_home", "oeFG_home", "DRB_home", "oTOV_home")
    
    knn_input <- cbind(gisele_away_knn, gisele_home_knn)
    colnames(knn_input) <- c("ORtg_away", "DRtg_away",
                             "ORtg_home", "DRtg_home",
                             "ORB_home", "TOV_home", "oeFG_home", "DRB_home", "oTOV_home")
    
    marg_scaled <- knnr_fit[,1]
    maxs <- apply(marg_scaled, 2, max) 
    mins <- apply(marg_scaled, 2, min)
    marg_scaled <- as.data.frame(scale(marg_scaled, center = mins, scale = maxs - mins))
    
    knnr_train <- bind_rows(knnr_fit[,-1], knnr_input)
    maxs <- apply(knnr_train, 2, max) 
    mins <- apply(knnr_train, 2, min)
    knnr_train <- as.data.frame(scale(knnr_train, center = mins, scale = maxs - mins))
    knnr_input <- tail(knnr_train,1)
    knnr_train <- slice(knnr_train, 1:(n()-1))
    knnr_train <- bind_cols(marg_scaled, knnr_train)
    
    
    knn_train <- bind_rows(knn_fit[,-1], knn_input)
    maxs <- apply(knn_train, 2, max) 
    mins <- apply(knn_train, 2, min)
    knn_train <- as.data.frame(scale(knn_train, center = mins, scale = maxs - mins))
    knn_input <- tail(knn_train,1)
    knn_train <- slice(knn_train, 1:(n()-1))
    knn_train <- bind_cols(knn_fit[,1], knn_train)
    knn_train$Win <- as_factor(knn_train$Win)
    
    knn_margin_scaled <- FNN::knn.reg(knnr_train[,-1], knnr_input, as.numeric(knnr_train$Margin), k=21)
    knn_margin <- knn_margin_scaled[["pred"]] * (max(knnr_fit$Margin) - min(knnr_fit$Margin)) + min(knnr_fit$Margin)
    
    knn_win <- predict(knn3(knn_train[,-1], knn_train$Win, k=42), knn_input)
    
    holder <- slate[a,2:3]
    holder$Away_Margin <- knn_margin
    holder$Home_Margin <- knn_margin*-1
    holder$Away_Win <- knn_win[,2]
    holder$Home_Win <- knn_win[,1]
    # holder$Away_score <- gisele_ascore
    # holder$Home_score <- gisele_hscore
    
    gisele_predict <- bind_rows(gisele_predict,holder)
    
    pb$tick()
    Sys.sleep(1 / nrow(slate))
    
}

gisele_predict <- gisele_predict %>%
    mutate(across(where(is.numeric), round, 3))


#### Kate #### - Random Forest

library(randomForest)

pb <- progress_bar$new(
    format = "  running model... [:bar] :percent eta: :eta",
    total = nrow(slate), clear = FALSE, width = 60, show_after = 0)
invisible(pb$tick(0))

rfr_train <- master_db %>%
    select(Margin, ORtg_away, DRtg_away, Pace_away, eFG_away, FTR_away,
               ORB_away, TOV_away, oeFG_away, oFTR_away, DRB_away, oTOV_away,
               ORtg_home, DRtg_home, Pace_home, eFG_home, FTR_home,
               ORB_home, TOV_home, oeFG_home, oFTR_home, DRB_home, oTOV_home)

mtry_reg <- tuneRF(rfr_train[-1], rfr_train$Margin, ntreeTry=500,
               stepFactor=1.5, improve=0.01, trace=TRUE, plot=FALSE)
best_m_reg <- mtry_reg[mtry_reg[, 2] == min(mtry_reg[, 2]), 1]

rfr_fit <- randomForest(Margin ~ ., data = rfr_train, mtry = best_m_reg)

rf_train <- master_db %>%
    select(Win, ORtg_away, DRtg_away, ORtg_home, DRtg_home, Pace_home, ORB_home, oeFG_home, DRB_home, oTOV_home)
rf_train$Win <- as_factor(rf_train$Win)

mtry <- tuneRF(rf_train[-1], as_factor(rf_train$Win), ntreeTry=500,
               stepFactor=1.5, improve=0.01, trace=TRUE, plot=FALSE)
best_m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

rf_fit <-randomForest(Win ~ ., data = rf_train, mtry = best_m)

kate_predict <- data.frame() # Predictions frame

a <- 1
g <- nrow(slate)

for (a in a:g) {
    
    slate_away <- as.character(slate[a,2])
    slate_home <- as.character(slate[a,3])
    
    kate_away_rfr <- away_final_wt %>%
        filter(Team == slate_away) %>%
        select(ORtg, DRtg, Pace, eFG, FTR, ORB, TOV, oeFG, oFTR, DRB, oTOV)
    
    kate_home_rfr <- home_final_wt %>%
        filter(Team == slate_home) %>%
        select(ORtg, DRtg, Pace, eFG, FTR, ORB, TOV, oeFG, oFTR, DRB, oTOV)
    
    kate_away_rf <- away_final_wt %>%
        filter(Team == slate_away) %>%
        select(ORtg, DRtg)
    
    kate_home_rf <- home_final_wt %>%
        filter(Team == slate_home) %>%
        select(ORtg, DRtg, Pace, ORB, oeFG, DRB, oTOV)
    
    rfr_input <- cbind(kate_away_rfr, kate_home_rfr)
    colnames(rfr_input) <- c("ORtg_away","DRtg_away","Pace_away",
                             "eFG_away","FTR_away","ORB_away","TOV_away",
                             "oeFG_away","oFTR_away","DRB_away","oTOV_away",
                             "ORtg_home","DRtg_home","Pace_home",
                             "eFG_home","FTR_home","ORB_home","TOV_home",
                             "oeFG_home","oFTR_home","DRB_home","oTOV_home")

    rf_input <- cbind(kate_away_rf, kate_home_rf)
    colnames(rf_input) <- c("ORtg_away", "DRtg_away",
                             "ORtg_home", "DRtg_home",
                             "Pace_home", "ORB_home", "oeFG_home", "DRB_home", "oTOV_home")

    rf_margin <- predict(rfr_fit, rfr_input)
    
    rf_win <- predict(rf_fit, rf_input, type = "prob")
    
    holder <- slate[a,2:3]
    holder$Away_Margin <- rf_margin
    holder$Home_Margin <- rf_margin*-1
    holder$Away_Win <- rf_win[,2]
    holder$Home_Win <- rf_win[,1]
    # holder$Away_score <- kate_ascore
    # holder$Home_score <- kate_hscore
    
    kate_predict <- bind_rows(kate_predict,holder)
    
    pb$tick()
    Sys.sleep(1 / nrow(slate))
    
}

kate_predict <- kate_predict %>%
    mutate(across(where(is.numeric), round, 3))


#### Cindy #### - Support Vector Machine

library(e1071)

pb <- progress_bar$new(
    format = "  running model... [:bar] :percent eta: :eta",
    total = nrow(slate), clear = FALSE, width = 60, show_after = 0)
invisible(pb$tick(0))

svmr_train <- master_db %>%
    select(Margin, ORtg_home, DRtg_home, ORtg_away, DRtg_away, ORB_home, TOV_home, oeFG_home, DRB_home, oTOV_home)

svmr_fit <- svm(Margin ~ ., data = svmr_train, cost = .4, gamma = .005, kernal = 'linear')

svm_train <- master_db %>%
    select(Win, ORtg_away, DRtg_away, ORtg_home, DRtg_home, Pace_home, ORB_home, oeFG_home, DRB_home, oTOV_home)

svm_fit <- svm(as_factor(Win) ~ ., data = svm_train, cost = .4, gamma = .005, kernal = 'radial', probability=TRUE)

cindy_predict <- data.frame() # Predictions frame

a <- 1
g <- nrow(slate)

for (a in a:g) {
    
    slate_away <- as.character(slate[a,2])
    slate_home <- as.character(slate[a,3])
    
    cindy_away_svmr <- away_final_wt %>%
        filter(Team == slate_away) %>%
        select(ORtg, DRtg)
    
    cindy_home_svmr <- home_final_wt %>%
        filter(Team == slate_home) %>%
        select(ORtg, DRtg, ORB, TOV, oeFG, DRB, oTOV)
    
    cindy_away_svm <- away_final_wt %>%
        filter(Team == slate_away) %>%
        select(ORtg, DRtg)
    
    cindy_home_svm <- home_final_wt %>%
        filter(Team == slate_home) %>%
        select(ORtg, DRtg, Pace, ORB, oeFG, DRB, oTOV)
    
    svmr_input <- cbind(cindy_away_svmr, cindy_home_svmr)
    colnames(svmr_input) <- c("ORtg_away","DRtg_away",
                             "ORtg_home","DRtg_home",
                             "ORB_home","TOV_home", "oeFG_home","DRB_home","oTOV_home")

    
    svm_input <- cbind(cindy_away_svm, cindy_home_svm)
    colnames(svm_input) <- c("ORtg_away", "DRtg_away",
                            "ORtg_home", "DRtg_home",
                            "Pace_home", "ORB_home", "oeFG_home", "DRB_home", "oTOV_home")

    
    svm_margin <- predict(svmr_fit, svmr_input)
    
    svm_win <- predict(svm_fit, svm_input, probability=TRUE)
    
    holder <- slate[a,2:3]
    holder$Away_Margin <- svm_margin
    holder$Home_Margin <- svm_margin*-1
    holder$Away_Win <- attr(svm_win, "probabilities")[,2]
    holder$Home_Win <- attr(svm_win, "probabilities")[,1]
    # holder$Away_score <- cindy_ascore
    # holder$Home_score <- cindy_hscore
    
    cindy_predict <- bind_rows(cindy_predict,holder)
    
    pb$tick()
    Sys.sleep(1 / nrow(slate))
    
}

cindy_predict <- cindy_predict %>%
    mutate(across(where(is.numeric), round, 3))


#### Naomi #### - Artificial Neural Network

library(neuralnet)

pb <- progress_bar$new(
    format = "  running model... [:bar] :percent eta: :eta",
    total = nrow(slate), clear = FALSE, width = 60, show_after = 0)
invisible(pb$tick(0))

nnr_train <- master_db %>%
    select(Margin, ORtg_away, DRtg_away, ORtg_home, DRtg_home, ORB_home, TOV_home, oeFG_home, DRB_home, oTOV_home)

nn_train <- master_db %>%
    select(Win, ORtg_away, DRtg_away, eFG_away, oeFG_away, ORtg_home, DRtg_home, eFG_home, oeFG_home)

naomi_predict <- data.frame() # Predictions frame

a <- 1
g <- nrow(slate)

for (a in a:g) {
    
    slate_away <- as.character(slate[a,2])
    slate_home <- as.character(slate[a,3])
    
    naomi_away_nnr <- away_final_wt %>%
        filter(Team == slate_away) %>%
        select(ORtg, DRtg)
    
    naomi_home_nnr <- home_final_wt %>%
        filter(Team == slate_home) %>%
        select(ORtg, DRtg, ORB, TOV, oeFG, DRB, oTOV)
    
    naomi_away_nn <- away_final_wt %>%
        filter(Team == slate_away) %>%
        select(ORtg, DRtg, eFG, oeFG)
    
    naomi_home_nn <- home_final_wt %>%
        filter(Team == slate_home) %>%
        select(ORtg, DRtg, FG, oeFG)
    
    nnr_input <- cbind(naomi_away_nnr, naomi_home_nnr)
    colnames(nnr_input) <- c("ORtg_away","DRtg_away",
                              "ORtg_home","DRtg_home",
                              "ORB_home","TOV_home", "oeFG_home","DRB_home","oTOV_home")
    
    nn_input <- cbind(naomi_away_nn, naomi_home_nn)
    colnames(nn_input) <- c("ORtg_away", "DRtg_away",
                            "eFG_away", "oeFG_away",
                            "ORtg_home", "DRtg_home",
                            "eFG_home", "oeFG_home")
    
    marg_scaled <- nnr_train[,1]
    maxs <- apply(marg_scaled, 2, max) 
    mins <- apply(marg_scaled, 2, min)
    marg_scaled <- as.data.frame(scale(marg_scaled, center = mins, scale = maxs - mins))
    
    nnr_fit <- bind_rows(nnr_train[,-1], nnr_input)
    maxs <- apply(nnr_fit, 2, max) 
    mins <- apply(nnr_fit, 2, min)
    nnr_fit <- as.data.frame(scale(nnr_fit, center = mins, scale = maxs - mins))
    nnr_input <- tail(nnr_fit,1)
    nnr_fit <- slice(nnr_fit, 1:(n()-1))
    nnr_fit <- bind_cols(marg_scaled, nnr_fit)
    
    nr <- names(nnr_train)
    fr <- as.formula(paste("Margin ~", paste(nr[!nr %in% "Margin"], collapse = " + ")))
    
    nn_fit <- bind_rows(nn_train[,-1], nn_input)
    maxs <- apply(nn_fit, 2, max) 
    mins <- apply(nn_fit, 2, min)
    nn_fit <- as.data.frame(scale(nn_fit, center = mins, scale = maxs - mins))
    nn_input <- tail(nn_fit,1)
    nn_fit <- slice(nn_fit, 1:(n()-1))
    nn_fit <- bind_cols(nn_train[,1], nn_fit)
    
    n <- names(nn_train)
    f <- as.formula(paste("Win ~", paste(n[!n %in% "Win"], collapse = " + ")))
    
    nn_margin_scaled <- compute(neuralnet(fr, data = nnr_fit, hidden = 5, linear.output = TRUE, threshold = .1), nnr_input)
    nn_margin <- nn_margin_scaled$net.result * (max(nnr_train$Margin) - min(nnr_train$Margin)) + min(nnr_train$Margin)
    
    nn_win <- compute(neuralnet(f, data = nn_fit, hidden = 2, linear.output = FALSE, threshold = .1), nn_input)
    
    holder <- slate[a,2:3]
    holder$Away_Margin <- nn_margin
    holder$Home_Margin <- nn_margin*-1
    holder$Away_Win <- nn_win$net.result
    holder$Home_Win <- 1-nn_win$net.result
    # holder$Away_score <- naomi_ascore
    # holder$Home_score <- naomi_hscore
    
    naomi_predict <- bind_rows(naomi_predict,holder)
    
    pb$tick()
    Sys.sleep(1 / nrow(slate))
    
}

naomi_predict <- naomi_predict %>%
    mutate(across(where(is.numeric), round, 3))

#### Adriana #### - Combo

adriana_predict <- slate[,-1]

adriana_predict$Away_Margin <- round(rowMeans(cbind(kendall_predict[,3], tyra_predict[,3], gisele_predict[,3], 
                                                  kate_predict[,3], cindy_predict[,3], naomi_predict[,3])),3)

adriana_predict$Home_Margin <- round(rowMeans(cbind(kendall_predict[,4], tyra_predict[,4], gisele_predict[,4], 
                                            kate_predict[,4], cindy_predict[,4], naomi_predict[,4])),3)

adriana_predict$Away_Win <- round(rowMeans(cbind(kendall_predict[,5], tyra_predict[,5], gisele_predict[,5], 
                                         kate_predict[,5], cindy_predict[,5], naomi_predict[,5])),3)

adriana_predict$Home_Win <- round(rowMeans(cbind(kendall_predict[,6], tyra_predict[,6], gisele_predict[,6], 
                                         kate_predict[,6], cindy_predict[,6], naomi_predict[,6])),3)

kendall_predict$Model <- "Kendall Jenner - Simple Model"
tyra_predict$Model <- "Tyra Banks - Least Squares"
gisele_predict$Model <- "Gisele Bundchen - KNN"
kate_predict$Model <- "Kate Moss - Random Forest"
cindy_predict$Model <- "Cindy Crawford - SVM"
naomi_predict$Model <- "Naomi Campbell - Neural Network"
adriana_predict$Model <- "Adriana Lima - Combination"

all_models <- rbind(kendall_predict, tyra_predict, gisele_predict, 
                    kate_predict, cindy_predict, naomi_predict, adriana_predict)

# #### Removing Outliers ####
# 
# # boxplot of winning margin
# boxplot(master_db$Margin)$out
# 
# # removing outliers 
# x_out_rm <- master_db[!master_db$Margin %in% boxplot.stats(master_db$Margin)$out,]
# 
# # number and % of games removed
# nrow(master_db) - nrow(x_out_rm)
# (nrow(master_db) - nrow(x_out_rm))/nrow(master_db)
# 
# # boxplot of new data
# boxplot(x_out_rm$Margin)

a_list <- list("TOV","STL","BLK","PF","oFG","oSR2","oFG3","oSR3","oFT","oFTR","oORB",
               "oDRB","oTRB","oAST","oeFG","oTS","DRtg")
away_rank_a <- away_final_wt %>%
    mutate_if(grepl(paste(a_list, collapse = "|"), names(.)), list(rank=~rank( .)))

d_list <- list("FG","SR2","FG3","SR3","FT","FTR","ORB","DRB","TRB","AST",
               "eFG","TS","ORtg","Pace","oTOV","oSTL","oBLK","oPF")
away_rank_d <- away_final_wt %>%
    mutate_if(grepl(paste(d_list, collapse = "|"), names(.)), list(rank=~rank(-.)))

away_rank <- away_final_wt %>%
    left_join(away_rank_d[,c(1,37:48,59:62,65,66)]) %>%
    left_join(.,away_rank_a[,c(1,37:50,55:57)], by = "Team") %>%
    select(1,2,37,3,38,4,39,5,40,6,41,7,42,8,43,9,44,10,45,11,46,12,55,13,56,14,57,15,58,16,47,17,48,
           18,59,19,60,20,61,21,62,22,63,23,64,24,65,25,66,26,67,27,68,28,49,29,50,30,51,31,52,32,69,33,70,
           34,53,35,71,36,54)



a_list <- list("TOV","STL","BLK","PF","oFG","oSR2","oFG3","oSR3","oFT","oFTR","oORB",
               "oDRB","oTRB","oAST","oeFG","oTS","DRtg")
home_rank_a <- home_final_wt %>%
    mutate_if(grepl(paste(a_list, collapse = "|"), names(.)), list(rank=~rank( .)))

d_list <- list("FG","SR2","FG3","SR3","FT","FTR","ORB","DRB","TRB","AST",
               "eFG","TS","ORtg","Pace","oTOV","oSTL","oBLK","oPF")
home_rank_d <- home_final_wt %>%
    mutate_if(grepl(paste(d_list, collapse = "|"), names(.)), list(rank=~rank(-.)))

home_rank <- home_final_wt %>%
    left_join(home_rank_d[,c(1,37:48,59:62,65,66)]) %>%
    left_join(.,home_rank_a[,c(1,37:50,55:57)], by = "Team") %>%
    select(1,2,37,3,38,4,39,5,40,6,41,7,42,8,43,9,44,10,45,11,46,12,55,13,56,14,57,15,58,16,47,17,48,
           18,59,19,60,20,61,21,62,22,63,23,64,24,65,25,66,26,67,27,68,28,49,29,50,30,51,31,52,32,69,33,70,
           34,53,35,71,36,54)


game_times <- slate_all %>% select(3,4,2)
game_times$time <- str_sub(string = game_times$time, start = 1, end = str_length(game_times$time)-1)
game_times[,4:5] <- as.numeric(str_split_fixed(string = game_times$time, pattern = ":", 2))

game_times <- game_times %>%
    mutate(time = hms::hms(hours = (game_times$V4-1), min = game_times$V5)) %>%
    select(1:3)
    
game_times$time <- format(as.POSIXct(game_times$time), format = "%H:%M")

##### EXPORT TO EXCEL ######

library(XLConnect)

wb <- loadWorkbook("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/viz.xlsx")
setStyleAction(wb,XLC$"STYLE_ACTION.NONE")
clearSheet(wb, sheet = "all models")
clearSheet(wb, sheet = "away rank")
clearSheet(wb, sheet = "home rank")
# clearSheet(wb, sheet = "game times")
writeWorksheet(wb, all_models, "all models")
writeWorksheet(wb, away_rank, "away rank")
writeWorksheet(wb, home_rank, "home rank")
writeWorksheet(wb, standings, "standings")
writeWorksheet(wb, league_avg, "league")
writeWorksheet(wb, game_times, "game times")
saveWorkbook(wb, file = "/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/viz.xlsx")


# wb <- createWorkbook()
# addWorksheet(wb, sheetName = "all models")
# addWorksheet(wb, sheetName = "away rank")
# addWorksheet(wb, sheetName = "home rank")
# writeData(wb, sheet = "all models", x = all_models)
# writeData(wb, sheet = "away rank", x = away_rank)
# writeData(wb, sheet = "home rank", x = home_rank)
# 
# saveWorkbook(wb, file = "/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/viz.xlsx")

