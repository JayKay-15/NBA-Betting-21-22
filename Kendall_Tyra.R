#### Slate ####

slate <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/Slate.xlsx") # Today's games
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
    
    holder <- slate[a,1:2]
    holder$Away_Margin <- away_kendall_score - home_kendall_score
    holder$Home_Margin <- home_kendall_score - away_kendall_score
    holder$Away_score <- away_kendall_score
    holder$Home_score <- home_kendall_score
    holder$Away_win <- away_kendall_win
    holder$Home_win <- home_kendall_win
    
    kendall_predict <- bind_rows(kendall_predict,holder)
    
}

kendall_predict <- kendall_predict %>%
    mutate(across(where(is.numeric), round, 3))


#### Tyra ####

## Tyra Fitting

master_reg <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb1721.xlsx")  

margin_fit <- lm(Margin ~ ORtg_away + DRtg_away + Pace_away + eFG_away + ORB_away +
                     TOV_away + FTR_away + oeFG_away + DRB_away + oTOV_away + 
                     oFTR_away + ORtg_home + DRtg_home + Pace_home + eFG_home + ORB_home +
                     TOV_home + FTR_home + oeFG_home + DRB_home + oTOV_home + 
                     oFTR_home, data = master_reg)

ascore_fit <- lm(AS ~ ORtg_away + DRtg_away + Pace_away + eFG_away + ORB_away +
                     TOV_away + FTR_away + oeFG_away + DRB_away + oTOV_away + 
                     oFTR_away + ORtg_home + DRtg_home + Pace_home + eFG_home + ORB_home +
                     TOV_home + FTR_home + oeFG_home + DRB_home + oTOV_home + 
                     oFTR_home, data = master_reg)

hscore_fit <- lm(HS ~ ORtg_away + DRtg_away + Pace_away + eFG_away + ORB_away +
                     TOV_away + FTR_away + oeFG_away + DRB_away + oTOV_away + 
                     oFTR_away + ORtg_home + DRtg_home + Pace_home + eFG_home + ORB_home +
                     TOV_home + FTR_home + oeFG_home + DRB_home + oTOV_home + 
                     oFTR_home, data = master_reg)

win_fit <- glm(Win ~ ORtg_away + DRtg_away + Pace_away + eFG_away + ORB_away +
                   TOV_away + FTR_away + oeFG_away + DRB_away + oTOV_away + 
                   oFTR_away + ORtg_home + DRtg_home + Pace_home + eFG_home + ORB_home +
                   TOV_home + FTR_home + oeFG_home + DRB_home + oTOV_home + 
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
    
    tyra_away_oe <- as.numeric(tyra_away[,34])
    tyra_away_de <- as.numeric(tyra_away[,35])
    tyra_away_pa <- as.numeric(tyra_away[,36])
    
    tyra_home_oe <- as.numeric(tyra_home[,34])
    tyra_home_de <- as.numeric(tyra_home[,35])
    tyra_home_pa <- as.numeric(tyra_home[,36])
    
    tyra_away_efg <- as.numeric(tyra_away[,16])
    tyra_away_ftr <- as.numeric(tyra_away[,7])
    tyra_away_orb <- as.numeric(tyra_away[,8])
    tyra_away_tov <- as.numeric(tyra_away[,12])
    tyra_away_oefg <- as.numeric(tyra_away[,32])
    tyra_away_oftr <- as.numeric(tyra_away[,23])
    tyra_away_drb <- as.numeric(tyra_away[,9])
    tyra_away_otov <- as.numeric(tyra_away[,28])
    
    tyra_home_efg <- as.numeric(tyra_home[,16])
    tyra_home_ftr <- as.numeric(tyra_home[,7])
    tyra_home_orb <- as.numeric(tyra_home[,8])
    tyra_home_tov <- as.numeric(tyra_home[,12])
    tyra_home_oefg <- as.numeric(tyra_home[,32])
    tyra_home_oftr <- as.numeric(tyra_home[,23])
    tyra_home_drb <- as.numeric(tyra_home[,9])
    tyra_home_otov <- as.numeric(tyra_home[,28])
    
    tyra_input <- data.frame(tyra_away_oe,tyra_away_de,tyra_away_pa,
                             tyra_away_efg,tyra_away_ftr,tyra_away_orb,tyra_away_tov,
                             tyra_away_oefg,tyra_away_oftr,tyra_away_drb,tyra_away_otov,
                             tyra_home_oe,tyra_home_de,tyra_home_pa,
                             tyra_home_efg,tyra_home_ftr,tyra_home_orb,tyra_home_tov,
                             tyra_home_oefg,tyra_home_oftr,tyra_home_drb,tyra_home_otov)
    
    colnames(tyra_input) <- c("ORtg_away","DRtg_away","Pace_away",
                              "eFG_away","FTR_away","ORB_away","TOV_away",
                              "oeFG_away","oFTR_away","DRB_away","oTOV_away",
                              "ORtg_home","DRtg_home","Pace_home",
                              "eFG_home","FTR_home","ORB_home","TOV_home",
                              "oeFG_home","oFTR_home","DRB_home","oTOV_home")
    
    tyra_margin <- as.numeric(predict(margin_fit, newdata = tyra_input, type = "response"))
    tyra_ascore <- as.numeric(predict(ascore_fit, newdata = tyra_input, type = "response"))
    tyra_hscore <- as.numeric(predict(hscore_fit, newdata = tyra_input, type = "response"))
    
    tyra_awin <- as.numeric(predict(win_fit, newdata = tyra_input, type = "response"))
    tyra_hwin <- 1 - tyra_awin
    
    holder <- slate[a,1:2]
    holder$Away_Margin <- tyra_margin
    holder$Home_Margin <- tyra_margin*-1
    holder$Away_score <- tyra_ascore
    holder$Home_score <- tyra_hscore
    holder$Away_win <- tyra_awin
    holder$Home_win <- tyra_hwin
    
    tyra_predict <- bind_rows(tyra_predict,holder)
    
}

tyra_predict <- tyra_predict %>%
    mutate(across(where(is.numeric), round, 3))






