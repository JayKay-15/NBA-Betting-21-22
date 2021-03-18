#### Slate ####

slate <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Database/2021-2022/Slate.xlsx") # Today's games
slate <- drop_na(slate)

#### Kendall ####

kendall_predict <- data.frame() # Predictions frame

a <- 1
g <- nrow(slate)

for (a in a:g) {
    
    slate_away <- as.character(slate[a,1])
    slate_home <- as.character(slate[a,2])
    
    kendall_away <- away_final_wt %>%
        filter(Team == slate_away)
    
    kendall_home <- home_final_wt %>%
        filter(Team == slate_home)
    
    kendall_away_oe <- as.numeric(kendall_away[,19])
    kendall_away_de <- as.numeric(kendall_away[,20])
    kendall_away_pa <- as.numeric(kendall_away[,18])
    
    kendall_home_oe <- as.numeric(kendall_home[,19])
    kendall_home_de <- as.numeric(kendall_home[,20])
    kendall_home_pa <- as.numeric(kendall_home[,18])
    
    lg_pace <- as.numeric(league_avg[1,18])
    lg_oe <- as.numeric(league_avg[1,19])
    
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
    
    away_kendall_score <- (away_proj_oe * expected_pace) + as.numeric(advantage_mx[1,2])
    home_kendall_score <- (home_proj_oe * expected_pace) + as.numeric(advantage_mx[1,2])
    
    away_kendall_win <- (away_proj_oe ^ 14.23) / ((away_proj_oe ^ 14.23) + (home_proj_oe ^ 14.23))
    home_kendall_win <- 1 - away_kendall_win
    
    holder <- slate[a,1:2]
    holder$Margin <- away_kendall_score - home_kendall_score
    holder$Away_score <- away_kendall_score
    holder$Home_score <- home_kendall_score
    holder$Away_win <- away_kendall_win
    holder$Home_win <- home_kendall_win
    
    kendall_predict <- bind_rows(kendall_predict,holder)
    
}

kendall_predict <- kendall_predict %>%
    mutate_if(is.numeric, round, 3)







#### Tyra ####

## Tyra Fitting

df_16_17 <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Database/Database/YTD - Weighted/2016-2017 - YTD Weighted - v3 - weekly.xlsx", sheet = "final_ytd")
colnames(df_16_17)[9:78] <- c("FG_away","SR2_away","FG3_away","SR3_away","FT_away","FTR_away","ORB_away","DRB_away","TRB_away",
                              "AST_away","TO_away","STL_away","BLK_away","PF_away","eFG_away","TS_away","Pace_away","ORtg_away",
                              "DRtg_away","oFG_away","oSR2away","oFG3_away","oSR3_away","oFT_away","oFTR_away","oORB_away",
                              "oDRB_away","oTRB_away","oAST_away","oTO_away","oSTL_away","oBLK_away","oPF_away","oeFG_away",
                              "oTS_away",
                              "FG_home","SR2_home","FG3_home","SR3_home","FT_home","FTR_home","ORB_home","DRB_home","TRB_home",
                              "AST_home","TO_home","STL_home","BLK_home","PF_home","eFG_home","TS_home","Pace_home","ORtg_home",
                              "DRtg_home","oFG_home","oSR2home","oFG3_home","oSR3_home","oFT_home","oFTR_home","oORB_home",
                              "oDRB_home","oTRB_home","oAST_home","oTO_home","oSTL_home","oBLK_home","oPF_home","oeFG_home",
                              "oTS_home")

df_17_18 <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Database/Database/YTD - Weighted/2017-2018 - YTD Weighted - v3 - weekly.xlsx", sheet = "final_ytd")
colnames(df_17_18)[9:78] <- c("FG_away","SR2_away","FG3_away","SR3_away","FT_away","FTR_away","ORB_away","DRB_away","TRB_away",
                              "AST_away","TO_away","STL_away","BLK_away","PF_away","eFG_away","TS_away","Pace_away","ORtg_away",
                              "DRtg_away","oFG_away","oSR2away","oFG3_away","oSR3_away","oFT_away","oFTR_away","oORB_away",
                              "oDRB_away","oTRB_away","oAST_away","oTO_away","oSTL_away","oBLK_away","oPF_away","oeFG_away",
                              "oTS_away",
                              "FG_home","SR2_home","FG3_home","SR3_home","FT_home","FTR_home","ORB_home","DRB_home","TRB_home",
                              "AST_home","TO_home","STL_home","BLK_home","PF_home","eFG_home","TS_home","Pace_home","ORtg_home",
                              "DRtg_home","oFG_home","oSR2home","oFG3_home","oSR3_home","oFT_home","oFTR_home","oORB_home",
                              "oDRB_home","oTRB_home","oAST_home","oTO_home","oSTL_home","oBLK_home","oPF_home","oeFG_home",
                              "oTS_home")

df_18_19 <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Database/Database/YTD - Weighted/2018-2019 - YTD Weighted - v3 - weekly.xlsx", sheet = "final_ytd")
colnames(df_18_19)[9:78] <- c("FG_away","SR2_away","FG3_away","SR3_away","FT_away","FTR_away","ORB_away","DRB_away","TRB_away",
                              "AST_away","TO_away","STL_away","BLK_away","PF_away","eFG_away","TS_away","Pace_away","ORtg_away",
                              "DRtg_away","oFG_away","oSR2away","oFG3_away","oSR3_away","oFT_away","oFTR_away","oORB_away",
                              "oDRB_away","oTRB_away","oAST_away","oTO_away","oSTL_away","oBLK_away","oPF_away","oeFG_away",
                              "oTS_away",
                              "FG_home","SR2_home","FG3_home","SR3_home","FT_home","FTR_home","ORB_home","DRB_home","TRB_home",
                              "AST_home","TO_home","STL_home","BLK_home","PF_home","eFG_home","TS_home","Pace_home","ORtg_home",
                              "DRtg_home","oFG_home","oSR2home","oFG3_home","oSR3_home","oFT_home","oFTR_home","oORB_home",
                              "oDRB_home","oTRB_home","oAST_home","oTO_home","oSTL_home","oBLK_home","oPF_home","oeFG_home",
                              "oTS_home")

df_19_20 <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Database/Database/YTD - Weighted/2019-2020 - YTD Weighted - v3 - weekly - No Bubble.xlsx", sheet = "final_ytd")
colnames(df_19_20)[9:78] <- c("FG_away","SR2_away","FG3_away","SR3_away","FT_away","FTR_away","ORB_away","DRB_away","TRB_away",
                              "AST_away","TO_away","STL_away","BLK_away","PF_away","eFG_away","TS_away","Pace_away","ORtg_away",
                              "DRtg_away","oFG_away","oSR2away","oFG3_away","oSR3_away","oFT_away","oFTR_away","oORB_away",
                              "oDRB_away","oTRB_away","oAST_away","oTO_away","oSTL_away","oBLK_away","oPF_away","oeFG_away",
                              "oTS_away",
                              "FG_home","SR2_home","FG3_home","SR3_home","FT_home","FTR_home","ORB_home","DRB_home","TRB_home",
                              "AST_home","TO_home","STL_home","BLK_home","PF_home","eFG_home","TS_home","Pace_home","ORtg_home",
                              "DRtg_home","oFG_home","oSR2home","oFG3_home","oSR3_home","oFT_home","oFTR_home","oORB_home",
                              "oDRB_home","oTRB_home","oAST_home","oTO_home","oSTL_home","oBLK_home","oPF_home","oeFG_home",
                              "oTS_home")

master_reg <- bind_rows(df_16_17,df_17_18,df_18_19,df_19_20)

margin_fit <- lm(Margin ~ ORtg_away + DRtg_away + Pace_away + eFG_away + ORB_away +
                     TO_away + FTR_away + oeFG_away + DRB_away + oTO_away + 
                     oFTR_away + ORtg_home + DRtg_home + Pace_home + eFG_home + ORB_home +
                     TO_home + FTR_home + oeFG_home + DRB_home + oTO_home + 
                     oFTR_home, data = master_reg)

ascore_fit <- lm(AS ~ ORtg_away + DRtg_away + Pace_away + eFG_away + ORB_away +
                     TO_away + FTR_away + oeFG_away + DRB_away + oTO_away + 
                     oFTR_away + ORtg_home + DRtg_home + Pace_home + eFG_home + ORB_home +
                     TO_home + FTR_home + oeFG_home + DRB_home + oTO_home + 
                     oFTR_home, data = master_reg)

hscore_fit <- lm(HS ~ ORtg_away + DRtg_away + Pace_away + eFG_away + ORB_away +
                     TO_away + FTR_away + oeFG_away + DRB_away + oTO_away + 
                     oFTR_away + ORtg_home + DRtg_home + Pace_home + eFG_home + ORB_home +
                     TO_home + FTR_home + oeFG_home + DRB_home + oTO_home + 
                     oFTR_home, data = master_reg)

win_fit <- glm(Win ~ ORtg_away + DRtg_away + Pace_away + eFG_away + ORB_away +
                   TO_away + FTR_away + oeFG_away + DRB_away + oTO_away + 
                   oFTR_away + ORtg_home + DRtg_home + Pace_home + eFG_home + ORB_home +
                   TO_home + FTR_home + oeFG_home + DRB_home + oTO_home + 
                   oFTR_home, data = master_reg, family = "binomial")

### Tyra Predictions ###

tyra_predict <- data.frame() # Predictions frame

a <- 1
g <- nrow(slate)

for (a in a:g) {
    
    slate_away <- as.character(slate[a,1])
    slate_home <- as.character(slate[a,2])
    
    tyra_away <- away_final_wt %>%
        filter(Team == slate_away)
    
    tyra_home <- home_final_wt %>%
        filter(Team == slate_home)
    
    tyra_away_oe <- as.numeric(tyra_away[,19])
    tyra_away_de <- as.numeric(tyra_away[,20])
    tyra_away_pa <- as.numeric(tyra_away[,18])
    
    tyra_home_oe <- as.numeric(tyra_home[,19])
    tyra_home_de <- as.numeric(tyra_home[,20])
    tyra_home_pa <- as.numeric(tyra_home[,18])
    
    tyra_away_efg <- as.numeric(tyra_away[,16])
    tyra_away_ftr <- as.numeric(tyra_away[,7])
    tyra_away_orb <- as.numeric(tyra_away[,8])
    tyra_away_tov <- as.numeric(tyra_away[,12])
    tyra_away_oefg <- as.numeric(tyra_away[,35])
    tyra_away_oftr <- as.numeric(tyra_away[,26])
    tyra_away_drb <- as.numeric(tyra_away[,9])
    tyra_away_oto <- as.numeric(tyra_away[,31])
    
    tyra_home_efg <- as.numeric(tyra_home[,16])
    tyra_home_ftr <- as.numeric(tyra_home[,7])
    tyra_home_orb <- as.numeric(tyra_home[,8])
    tyra_home_tov <- as.numeric(tyra_home[,12])
    tyra_home_oefg <- as.numeric(tyra_home[,35])
    tyra_home_oftr <- as.numeric(tyra_home[,26])
    tyra_home_drb <- as.numeric(tyra_home[,9])
    tyra_home_oto <- as.numeric(tyra_home[,31])
    
    tyra_input <- data.frame(tyra_away_oe,tyra_away_de,tyra_away_pa,
                             tyra_away_efg,tyra_away_ftr,tyra_away_orb,tyra_away_to,
                             tyra_away_oefg,tyra_away_oftr,tyra_away_drb,tyra_away_oto,
                             tyra_home_oe,tyra_home_de,tyra_home_pa,
                             tyra_home_efg,tyra_home_ftr,tyra_home_orb,tyra_home_to,
                             tyra_home_oefg,tyra_home_oftr,tyra_home_drb,tyra_home_oto)
    
    colnames(tyra_input) <- c("ORtg_away","DRtg_away","Pace_away",
                              "eFG_away","FTR_away","ORB_away","TO_away",
                              "oeFG_away","oFTR_away","DRB_away","oTO_away",
                              "ORtg_home","DRtg_home","Pace_home",
                              "eFG_home","FTR_home","ORB_home","TO_home",
                              "oeFG_home","oFTR_home","DRB_home","oTO_home")
    
    tyra_margin <- as.numeric(predict(margin_fit, newdata = tyra_input, type = "response"))
    tyra_ascore <- as.numeric(predict(ascore_fit, newdata = tyra_input, type = "response"))
    tyra_hscore <- as.numeric(predict(hscore_fit, newdata = tyra_input, type = "response"))
    
    tyra_awin <- as.numeric(predict(win_fit, newdata = tyra_input, type = "response"))
    tyra_hwin <- 1 - tyra_awin
    
    holder <- slate[a,1:2]
    holder$Margin <- tyra_margin
    holder$Away_score <- tyra_ascore
    holder$Home_score <- tyra_hscore
    holder$Away_win <- tyra_awin
    holder$Home_win <- tyra_hwin
    
    tyra_predict <- bind_rows(tyra_predict,holder)
    
}

tyra_predict <- tyra_predict %>%
    mutate_if(is.numeric, round, 3)





