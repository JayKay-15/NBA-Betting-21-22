library(ggrepel)
library(ggimage)
devtools::install_github("beanumber/teamcolors")
library(teamcolors)

logos <- teamcolors %>%
    filter(league == "nba") %>%
    select(1,3,11) %>%
    mutate(across(where(is.character), str_replace_all, 
                  pattern = "Los Angeles Clippers", replacement = "LA Clippers"))


bound.label <- 115
df.text <- data.frame(lab.text = c("+Off, -Def", "+Off, +Def", "-Off, +Def", "-Off, -Def"), 
                      x = c(bound.label, bound.label, bound.label-15, bound.label-15), 
                      y = c(bound.label-15, bound.label, bound.label, bound.label-15))

# geom_text(data = df.text, aes(x, y, label = lab.text), colour = "red")



### Season Rating Viz

season_viz <- left_join(season_final_wt, logos, by = c("Team" = "name"))

asp_ratio <- 1.618 

season_viz %>%
    ggplot(aes(ORtg, DRtg)) +
    geom_image(aes(image = logo), size=.035, by = "width", asp = asp_ratio) +
    geom_hline(yintercept=median(season_final_wt$DRtg), linetype = "dashed", alpha=.5) + 
    geom_vline(xintercept=median(season_final_wt$ORtg), linetype = "dashed", alpha=.5) + 
    xlab("Offensive Rating (points scored per 100 possessions)") + 
    ylab("Defensive Rating (points allowed per 100 possessions)") +
    labs(title = "Offensive vs Defensive Rating", subtitle = "Data courtesy: nbastatR") +
    geom_text(data = df.text, aes(x, y, label = lab.text), colour = "red", size=5) +
    theme_bw()


### Home Rating Viz

home_viz <- left_join(home_final_wt, logos, by = c("Team" = "name"))

asp_ratio <- 1.618 

home_viz %>%
    ggplot(aes(ORtg, DRtg)) +
    geom_image(aes(image = logo), size=.035, by = "width", asp = asp_ratio) +
    geom_hline(yintercept=median(home_final_wt$DRtg), linetype = "dashed", alpha=.5) + 
    geom_vline(xintercept=median(home_final_wt$ORtg), linetype = "dashed", alpha=.5) + 
    theme_bw()
    

### Away Rating Viz

away_viz <- left_join(away_final_wt, logos, by = c("Team" = "name"))

asp_ratio <- 1.618 

away_viz %>%
    ggplot(aes(ORtg, DRtg)) + 
    geom_image(aes(image = logo), size=.035, by = "width", asp = asp_ratio) +
    geom_hline(yintercept=median(away_final_wt$DRtg), linetype = "dashed", alpha=.5) + 
    geom_vline(xintercept=median(away_final_wt$ORtg), linetype = "dashed", alpha=.5) + 
    theme_bw()







### Season eFG Viz

season_viz <- left_join(season_final_wt, logos, by = c("Team" = "name"))

asp_ratio <- 1.618 

season_viz %>%
    ggplot(aes(eFG, oeFG)) +
    geom_image(aes(image = logo), size=.035, by = "width", asp = asp_ratio) +
    geom_hline(yintercept=median(season_final_wt$oeFG), linetype = "dashed", alpha=.5) + 
    geom_vline(xintercept=median(season_final_wt$eFG), linetype = "dashed", alpha=.5) + 
    theme_bw()


### Home eFG Viz

home_viz <- left_join(home_final_wt, logos, by = c("Team" = "name"))

asp_ratio <- 1.618 

home_viz %>%
    ggplot(aes(eFG, oeFG)) +
    geom_image(aes(image = logo), size=.035, by = "width", asp = asp_ratio) +
    geom_hline(yintercept=median(home_final_wt$oeFG), linetype = "dashed", alpha=.5) + 
    geom_vline(xintercept=median(home_final_wt$eFG), linetype = "dashed", alpha=.5) + 
    theme_bw()


### Away eFG Viz

away_viz <- left_join(away_final_wt, logos, by = c("Team" = "name"))

asp_ratio <- 1.618 

away_viz %>%
    ggplot(aes(eFG, oeFG)) + 
    geom_image(aes(image = logo), size=.035, by = "width", asp = asp_ratio) +
    geom_hline(yintercept=median(away_final_wt$oeFG), linetype = "dashed", alpha=.5) + 
    geom_vline(xintercept=median(away_final_wt$eFG), linetype = "dashed", alpha=.5) + 
    theme_bw()








### Season FTR Viz

season_viz <- left_join(season_final_wt, logos, by = c("Team" = "name"))

asp_ratio <- 1.618 

season_viz %>%
    ggplot(aes(FTR, oFTR)) +
    geom_image(aes(image = logo), size=.035, by = "width", asp = asp_ratio) +
    geom_hline(yintercept=median(season_final_wt$oFTR), linetype = "dashed", alpha=.5) + 
    geom_vline(xintercept=median(season_final_wt$FTR), linetype = "dashed", alpha=.5) + 
    theme_bw()


### Home FTR Viz

home_viz <- left_join(home_final_wt, logos, by = c("Team" = "name"))

asp_ratio <- 1.618 

home_viz %>%
    ggplot(aes(FTR, oFTR)) +
    geom_image(aes(image = logo), size=.035, by = "width", asp = asp_ratio) +
    geom_hline(yintercept=median(home_final_wt$oFTR), linetype = "dashed", alpha=.5) + 
    geom_vline(xintercept=median(home_final_wt$FTR), linetype = "dashed", alpha=.5) + 
    theme_bw()


### Away FTR Viz

away_viz <- left_join(away_final_wt, logos, by = c("Team" = "name"))

asp_ratio <- 1.618 

away_viz %>%
    ggplot(aes(FTR, oFTR)) + 
    geom_image(aes(image = logo), size=.035, by = "width", asp = asp_ratio) +
    geom_hline(yintercept=median(away_final_wt$oFTR), linetype = "dashed", alpha=.5) + 
    geom_vline(xintercept=median(away_final_wt$FTR), linetype = "dashed", alpha=.5) + 
    theme_bw()








### Season ORB Viz

season_viz <- left_join(season_final_wt, logos, by = c("Team" = "name"))

asp_ratio <- 1.618 

season_viz %>%
    ggplot(aes(ORB, DRB)) +
    geom_image(aes(image = logo), size=.035, by = "width", asp = asp_ratio) +
    geom_hline(yintercept=median(season_final_wt$DRB), linetype = "dashed", alpha=.5) + 
    geom_vline(xintercept=median(season_final_wt$ORB), linetype = "dashed", alpha=.5) + 
    theme_bw()


### Home ORB Viz

home_viz <- left_join(home_final_wt, logos, by = c("Team" = "name"))

asp_ratio <- 1.618 

home_viz %>%
    ggplot(aes(ORB, DRB)) +
    geom_image(aes(image = logo), size=.035, by = "width", asp = asp_ratio) +
    geom_hline(yintercept=median(home_final_wt$DRB), linetype = "dashed", alpha=.5) + 
    geom_vline(xintercept=median(home_final_wt$ORB), linetype = "dashed", alpha=.5) + 
    theme_bw()


### Away ORB Viz

away_viz <- left_join(away_final_wt, logos, by = c("Team" = "name"))

asp_ratio <- 1.618 

away_viz %>%
    ggplot(aes(ORB, DRB)) + 
    geom_image(aes(image = logo), size=.035, by = "width", asp = asp_ratio) +
    geom_hline(yintercept=median(away_final_wt$DRB), linetype = "dashed", alpha=.5) + 
    geom_vline(xintercept=median(away_final_wt$ORB), linetype = "dashed", alpha=.5) + 
    theme_bw()








### Season TOV Viz

season_viz <- left_join(season_final_wt, logos, by = c("Team" = "name"))

asp_ratio <- 1.618 

season_viz %>%
    ggplot(aes(TOV, oTOV)) +
    geom_image(aes(image = logo), size=.035, by = "width", asp = asp_ratio) +
    geom_hline(yintercept=median(season_final_wt$oTOV), linetype = "dashed", alpha=.5) + 
    geom_vline(xintercept=median(season_final_wt$TOV), linetype = "dashed", alpha=.5) + 
    theme_bw()


### Home TOV Viz

home_viz <- left_join(home_final_wt, logos, by = c("Team" = "name"))

asp_ratio <- 1.618 

home_viz %>%
    ggplot(aes(TOV, oTOV)) +
    geom_image(aes(image = logo), size=.035, by = "width", asp = asp_ratio) +
    geom_hline(yintercept=median(home_final_wt$oTOV), linetype = "dashed", alpha=.5) + 
    geom_vline(xintercept=median(home_final_wt$TOV), linetype = "dashed", alpha=.5) + 
    theme_bw()


### Away TOV Viz

away_viz <- left_join(away_final_wt, logos, by = c("Team" = "name"))

asp_ratio <- 1.618 

away_viz %>%
    ggplot(aes(TOV, oTOV)) + 
    geom_image(aes(image = logo), size=.035, by = "width", asp = asp_ratio) +
    geom_hline(yintercept=median(away_final_wt$oTOV), linetype = "dashed", alpha=.5) + 
    geom_vline(xintercept=median(away_final_wt$TOV), linetype = "dashed", alpha=.5) + 
    theme_bw()








### Key Analysis
results_book <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/Results.xlsx")
results_book$Date <- as_date(results_book$Date)

# spread_analysis <- results_book %>%
#     select("Kendall_Spread_Result", "Tyra_Spread_Result", "Gisele_Spread_Result", "Kate_Spread_Result",
#            "Cindy_Spread_Result", "Naomi_Spread_Result", "Adriana_Spread_Result",
#            "Gisele_Spread2_Result", "Kate_Spread2_Result",
#            "Cindy_Spread2_Result", "Naomi_Spread2_Result", "Adriana_Spread2_Result") %>%
#     filter(!is.na(.)) %>%
#     summarise(across(.cols = everything(),  sum))


results_book %>%
    filter(Kendall_Spread_Edge > 0 & Kendall_Spread_Result != 0) %>%
    mutate(WL = ifelse(Kendall_Spread_Result > 0, "Win", "Loss")) %>%
    ggplot(aes(x=WL, y=Kendall_Spread_Edge, group = WL)) + geom_boxplot()




results_book %>%
    filter(Kendall_ML_Edge > 0 & Kendall_ML_Result != 0) %>%
    mutate(WL = ifelse(Kendall_ML_Result > 0, "Win", "Loss")) %>%
    ggplot(aes(x=WL, y=Kendall_ML_Edge, group = WL)) + geom_boxplot()




results_book %>%
    filter(Kendall_Spread_Edge > 0 & Kendall_Spread_Result == -1.1) %>%
    # mutate(WL = ifelse(Kendall_Spread_Result > 0, "Win", "Loss")) %>%
    ggplot(aes(x=Spread, y=Kendall_Spread_Edge, group = Spread)) + geom_boxplot()




### Team Performance ###





