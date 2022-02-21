if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, readxl, lubridate, openxlsx, nbastatR, ggrepel, ggimage)
devtools::install_github("beanumber/teamcolors", force=TRUE)
library(teamcolors)

results_book <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/Results.xlsx")
results_book$Date <- as_date(results_book$Date)

logos <- teamcolors %>%
    filter(league == "nba") %>%
    select(1,3,11) %>%
    mutate(across(where(is.character), str_replace_all, 
                  pattern = "Los Angeles Clippers", replacement = "LA Clippers"))


bound.label <- 115
df.text <- data.frame(lab.text = c("+Off, +Def", "+Off, -Def", "-Off, -Def", "-Off, +Def"), 
                      x = c(bound.label, bound.label, bound.label-15, bound.label-15), 
                      y = c(bound.label-15, bound.label, bound.label, bound.label-15))

#### Season Ratings by Team ####

season_viz <- left_join(season_final_wt, logos, by = c("Team" = "name"))

asp_ratio <- 1.618 

season_viz %>%
    ggplot(aes(ORtg, DRtg)) +
    geom_image(aes(image = logo), size=.035, by = "width", asp = asp_ratio) +
    geom_hline(yintercept=median(season_final_wt$DRtg), linetype = "dashed", alpha=.5) + 
    geom_vline(xintercept=median(season_final_wt$ORtg), linetype = "dashed", alpha=.5) + 
    xlab("Offensive Rating (points scored per 100 possessions)") + 
    ylab("Defensive Rating (points allowed per 100 possessions)") +
    labs(title = "Offensive vs Defensive Rating", subtitle = "Updated 2/23/2022") +
    geom_text(data = df.text, aes(x, y, label = lab.text), colour = "red", size=6, fontface=2) +
    theme_bw()

### Home Rating Viz

home_viz <- left_join(home_final_wt, logos, by = c("Team" = "name"))

asp_ratio <- 1.618 

home_viz %>%
    ggplot(aes(ORtg, DRtg)) +
    geom_image(aes(image = logo), size=.035, by = "width", asp = asp_ratio) +
    geom_hline(yintercept=median(home_final_wt$DRtg), linetype = "dashed", alpha=.5) + 
    geom_vline(xintercept=median(home_final_wt$ORtg), linetype = "dashed", alpha=.5) + 
    xlab("Offensive Rating (points scored per 100 possessions)") + 
    ylab("Defensive Rating (points allowed per 100 possessions)") +
    labs(title = "Offensive vs Defensive Rating", subtitle = "Data courtesy: nbastatR - Updated 2/3/2022") +
    geom_text(data = df.text, aes(x, y, label = lab.text), colour = "red", size=6, fontface=2) +
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

#### Net Rating by Team ####
season_net <- season_final_wt %>%
    mutate(net_rating = ORtg - DRtg) %>%
    select(1,37)

season_net %>%
    group_by(Team) %>%
    left_join(., logos, by = c("Team" = "name")) %>%
    ggplot(aes(x=reorder(Team, net_rating), y=net_rating, fill = primary)) +
    geom_bar(stat = "identity") + scale_fill_identity() + coord_flip() +
    labs(title = "Season Net Rating", subtitle = "Adjusted") +
    scale_y_continuous(breaks = seq(-10, 10, 2)) +
    xlab("Team") +
    ylab("Net Rating") +
    theme_bw()
    
#### Times Series - Individual Models ####
results_book %>%
    filter(Kendall_Spread_Edge > 2.776) %>%
    group_by(Date) %>%
    summarise(day_total= sum(Kendall_Spread_Result)) %>%
    replace_na(list(day_total=0)) %>%
    mutate(cume = cumsum(day_total)) %>%
    ggplot(aes(x=Date, y=cume)) +
    geom_line() +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d")

results_book %>%
    filter(Tyra_Spread_Edge > 1.503) %>%
    group_by(Date) %>%
    summarise(day_total= sum(Tyra_Spread_Result)) %>%
    replace_na(list(day_total=0)) %>%
    mutate(cume = cumsum(day_total)) %>%
    ggplot(aes(x=Date, y=cume)) +
    geom_line() +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d")

results_book %>%
    filter(Gisele_Spread_Edge > 1.98) %>%
    group_by(Date) %>%
    summarise(day_total= sum(Gisele_Spread_Result)) %>%
    replace_na(list(day_total=0)) %>%
    mutate(cume = cumsum(day_total)) %>%
    ggplot(aes(x=Date, y=cume)) +
    geom_line() +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d")

results_book %>%
    filter(Kate_Spread_Edge > 4.413) %>%
    group_by(Date) %>%
    summarise(day_total= sum(Kate_Spread_Result)) %>%
    replace_na(list(day_total=0)) %>%
    mutate(cume = cumsum(day_total)) %>%
    ggplot(aes(x=Date, y=cume)) +
    geom_line() +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d")

results_book %>%
    filter(Cindy_Spread_Edge > 4.294) %>%
    group_by(Date) %>%
    summarise(day_total= sum(Cindy_Spread_Result)) %>%
    replace_na(list(day_total=0)) %>%
    mutate(cume = cumsum(day_total)) %>%
    ggplot(aes(x=Date, y=cume)) +
    geom_line() +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d")

results_book %>%
    filter(Naomi_Spread_Edge > 7.697) %>%
    group_by(Date) %>%
    summarise(day_total= sum(Naomi_Spread_Result)) %>%
    replace_na(list(day_total=0)) %>%
    mutate(cume = cumsum(day_total)) %>%
    ggplot(aes(x=Date, y=cume)) +
    geom_line() +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d")

results_book %>%
    filter(Adriana_Spread_Edge > 4.264) %>%
    group_by(Date) %>%
    summarise(day_total= sum(Adriana_Spread_Result)) %>%
    replace_na(list(day_total=0)) %>%
    mutate(cume = cumsum(day_total)) %>%
    ggplot(aes(x=Date, y=cume)) +
    geom_line() +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d")

#### Time Series - All Models ####
# Spread
kendall_spread_line <- results_book %>%
    filter(Kendall_Spread_Edge >= 2.607) %>%
    select(2,81) %>%
    mutate(model = "Kendall") %>%
    rename(value = Kendall_Spread_Result)

tyra_spread_line <- results_book %>%
    filter(Tyra_Spread_Edge >= 1.503) %>%
    select(2,86) %>%
    mutate(model = "Tyra") %>%
    rename(value = Tyra_Spread_Result)

gisele_spread_line <- results_book %>%
    filter(Gisele_Spread_Edge >= 1.941) %>%
    select(2,91) %>%
    mutate(model = "Gisele") %>%
    rename(value = Gisele_Spread_Result)

kate_spread_line <- results_book %>%
    filter(Kate_Spread_Edge >= 7.151) %>%
    select(2,96) %>%
    mutate(model = "Kate") %>%
    rename(value = Kate_Spread_Result)

cindy_spread_line <- results_book %>%
    filter(Cindy_Spread_Edge >= 5.269) %>%
    select(2,101) %>%
    mutate(model = "Cindy") %>%
    rename(value = Cindy_Spread_Result)

naomi_spread_line <- results_book %>%
    filter(Naomi_Spread_Edge >= 7.378) %>%
    select(2,106) %>%
    mutate(model = "Naomi") %>%
    rename(value = Naomi_Spread_Result)

adriana_spread_line <- results_book %>%
    filter(Adriana_Spread_Edge >= 4.735) %>%
    select(2,111) %>%
    mutate(model = "Adriana") %>%
    rename(value = Adriana_Spread_Result)

spread_line <- bind_rows(kendall_spread_line,tyra_spread_line,gisele_spread_line,kate_spread_line,
                         cindy_spread_line,naomi_spread_line,adriana_spread_line)

spread_line$model <- factor(spread_line$model, 
                           levels=c("Kendall","Tyra","Gisele","Kate","Cindy","Naomi","Adriana"))

# spread_line <- results_book %>%
#     select(2,81,86,91,96,101,106,111)
# 
# spread_line <- pivot_longer(spread_line, cols = c(2:8))

spread_line %>%
    replace_na(list(value=0)) %>%
    group_by(Date, model) %>%
    summarise(day_total = sum(value)) %>%
    group_by(model) %>%
    mutate(cume = cumsum(day_total)) %>%
    ggplot(aes(x=Date, y=cume, color=model)) +
    geom_line(size=1) +
    labs(title = "Spread", subtitle = "YTD Performance") +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    xlab("Date") + 
    ylab("Units") +
    theme_bw()

#Spread2
kendall_spread2_line <- results_book %>%
    filter(Kendall_Spread2_Edge >= 2.607) %>%
    select(2,82) %>%
    mutate(model = "Kendall") %>%
    rename(value = Kendall_Spread2_Result)

tyra_spread2_line <- results_book %>%
    filter(Tyra_Spread2_Edge >= 1.503) %>%
    select(2,87) %>%
    mutate(model = "Tyra") %>%
    rename(value = Tyra_Spread2_Result)

gisele_spread2_line <- results_book %>%
    filter(Gisele_Spread2_Edge >= 9.528) %>%
    select(2,92) %>%
    mutate(model = "Gisele") %>%
    rename(value = Gisele_Spread2_Result)

kate_spread2_line <- results_book %>%
    filter(Kate_Spread2_Edge >= 4.439) %>%
    select(2,97) %>%
    mutate(model = "Kate") %>%
    rename(value = Kate_Spread2_Result)

cindy_spread2_line <- results_book %>%
    filter(Cindy_Spread2_Edge >= 5.998) %>%
    select(2,102) %>%
    mutate(model = "Cindy") %>%
    rename(value = Cindy_Spread2_Result)

naomi_spread2_line <- results_book %>%
    filter(Naomi_Spread2_Edge >= 7.663) %>%
    select(2,107) %>%
    mutate(model = "Naomi") %>%
    rename(value = Naomi_Spread2_Result)

adriana_spread2_line <- results_book %>%
    filter(Adriana_Spread2_Edge >= 4.449) %>%
    select(2,112) %>%
    mutate(model = "Adriana") %>%
    rename(value = Adriana_Spread2_Result)

spread2_line <- bind_rows(kendall_spread2_line,tyra_spread2_line,gisele_spread2_line,kate_spread2_line,
                         cindy_spread2_line,naomi_spread2_line,adriana_spread2_line)

spread2_line$model <- factor(spread2_line$model, 
                           levels=c("Kendall","Tyra","Gisele","Kate","Cindy","Naomi","Adriana"))

spread2_line %>%
    replace_na(list(value=0)) %>%
    group_by(Date, model) %>%
    summarise(day_total = sum(value)) %>%
    group_by(model) %>%
    mutate(cume = cumsum(day_total)) %>%
    ggplot(aes(x=Date, y=cume, color=model)) +
    geom_line(size=1) +
    labs(title = "Spread2", subtitle = "YTD Performance") +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    xlab("Date") + 
    ylab("Units") +
    theme_bw()

#ML
kendall_ml_line <- results_book %>%
    filter(Kendall_ML_Edge >= 0) %>%
    select(2,83) %>%
    mutate(model = "Kendall") %>%
    rename(value = Kendall_ML_Result)

tyra_ml_line <- results_book %>%
    filter(Tyra_ML_Edge >= 0) %>%
    select(2,88) %>%
    mutate(model = "Tyra") %>%
    rename(value = Tyra_ML_Result)

gisele_ml_line <- results_book %>%
    filter(Gisele_ML_Edge >= 0) %>%
    select(2,93) %>%
    mutate(model = "Gisele") %>%
    rename(value = Gisele_ML_Result)

kate_ml_line <- results_book %>%
    filter(Kate_ML_Edge >= 0) %>%
    select(2,98) %>%
    mutate(model = "Kate") %>%
    rename(value = Kate_ML_Result)

cindy_ml_line <- results_book %>%
    filter(Cindy_ML_Edge >= 0) %>%
    select(2,103) %>%
    mutate(model = "Cindy") %>%
    rename(value = Cindy_ML_Result)

naomi_ml_line <- results_book %>%
    filter(Naomi_ML_Edge >= 0) %>%
    select(2,108) %>%
    mutate(model = "Naomi") %>%
    rename(value = Naomi_ML_Result)

adriana_ml_line <- results_book %>%
    filter(Adriana_ML_Edge >= 0) %>%
    select(2,113) %>%
    mutate(model = "Adriana") %>%
    rename(value = Adriana_ML_Result)

ml_line <- bind_rows(kendall_ml_line,tyra_ml_line,gisele_ml_line,kate_ml_line,
                        cindy_ml_line,naomi_ml_line,adriana_ml_line)

ml_line$model <- factor(ml_line$model, 
                           levels=c("Kendall","Tyra","Gisele","Kate","Cindy","Naomi","Adriana"))

ml_line %>%
    replace_na(list(value=0)) %>%
    group_by(Date, model) %>%
    summarise(day_total = sum(value)) %>%
    group_by(model) %>%
    mutate(cume = cumsum(day_total)) %>%
    ggplot(aes(x=Date, y=cume, color=model)) +
    geom_line(size=1) +
    labs(title = "ML", subtitle = "YTD Performance") +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    xlab("Date") + 
    ylab("Units") +
    theme_bw()

#Over
kendall_over_line <- results_book %>%
    filter(Kendall_Over_Edge >= 19.313) %>%
    select(2,84) %>%
    mutate(model = "Kendall") %>%
    rename(value = Kendall_Over_Result)

tyra_over_line <- results_book %>%
    filter(Tyra_Over_Edge >= 18.61) %>%
    select(2,89) %>%
    mutate(model = "Tyra") %>%
    rename(value = Tyra_Over_Result)

gisele_over_line <- results_book %>%
    filter(Gisele_Over_Edge >= 8.665) %>%
    select(2,94) %>%
    mutate(model = "Gisele") %>%
    rename(value = Gisele_Over_Result)

kate_over_line <- results_book %>%
    filter(Kate_Over_Edge >= 12.711) %>%
    select(2,99) %>%
    mutate(model = "Kate") %>%
    rename(value = Kate_Over_Result)

cindy_over_line <- results_book %>%
    filter(Cindy_Over_Edge >= 14.185) %>%
    select(2,104) %>%
    mutate(model = "Cindy") %>%
    rename(value = Cindy_Over_Result)

naomi_over_line <- results_book %>%
    filter(Naomi_Over_Edge >= 14.16) %>%
    select(2,109) %>%
    mutate(model = "Naomi") %>%
    rename(value = Naomi_Over_Result)

adriana_over_line <- results_book %>%
    filter(Adriana_Over_Edge >= 11.443) %>%
    select(2,114) %>%
    mutate(model = "Adriana") %>%
    rename(value = Adriana_Over_Result)

over_line <- bind_rows(kendall_over_line,tyra_over_line,gisele_over_line,kate_over_line,
                          cindy_over_line,naomi_over_line,adriana_over_line)

over_line$model <- factor(over_line$model, 
                           levels=c("Kendall","Tyra","Gisele","Kate","Cindy","Naomi","Adriana"))

over_line %>%
    replace_na(list(value=0)) %>%
    group_by(Date, model) %>%
    summarise(day_total = sum(value)) %>%
    group_by(model) %>%
    mutate(cume = cumsum(day_total)) %>%
    ggplot(aes(x=Date, y=cume, color=model)) +
    geom_line(size=1) +
    labs(title = "Over", subtitle = "YTD Performance") +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    xlab("Date") + 
    ylab("Units") +
    theme_bw()

#Under
kendall_under_line <- results_book %>%
    filter(Kendall_Under_Edge >= 14.177) %>%
    select(2,85) %>%
    mutate(model = "Kendall") %>%
    rename(value = Kendall_Under_Result)

tyra_under_line <- results_book %>%
    filter(Tyra_Under_Edge >= 4.831) %>%
    select(2,90) %>%
    mutate(model = "Tyra") %>%
    rename(value = Tyra_Under_Result)

gisele_under_line <- results_book %>%
    filter(Gisele_Under_Edge >= 1.643) %>%
    select(2,95) %>%
    mutate(model = "Gisele") %>%
    rename(value = Gisele_Under_Result)

kate_under_line <- results_book %>%
    filter(Kate_Under_Edge >= 4.271) %>%
    select(2,100) %>%
    mutate(model = "Kate") %>%
    rename(value = Kate_Under_Result)

cindy_under_line <- results_book %>%
    filter(Cindy_Under_Edge >= 3.27) %>%
    select(2,105) %>%
    mutate(model = "Cindy") %>%
    rename(value = Cindy_Under_Result)

naomi_under_line <- results_book %>%
    filter(Naomi_Under_Edge >= 2.016) %>%
    select(2,110) %>%
    mutate(model = "Naomi") %>%
    rename(value = Naomi_Under_Result)

adriana_under_line <- results_book %>%
    filter(Adriana_Under_Edge >= 12.027) %>%
    select(2,115) %>%
    mutate(model = "Adriana") %>%
    rename(value = Adriana_Under_Result)

under_line <- bind_rows(kendall_under_line,tyra_under_line,gisele_under_line,kate_under_line,
                       cindy_under_line,naomi_under_line,adriana_under_line)

under_line$model <- factor(under_line$model, 
                           levels=c("Kendall","Tyra","Gisele","Kate","Cindy","Naomi","Adriana"))

under_line %>%
    replace_na(list(value=0)) %>%
    group_by(Date, model) %>%
    summarise(day_total = sum(value)) %>%
    group_by(model) %>%
    mutate(cume = cumsum(day_total)) %>%
    ggplot(aes(x=Date, y=cume, color=model)) +
    geom_line(size=1) +
    labs(title = "Under", subtitle = "YTD Performance") +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    xlab("Date") + 
    ylab("Units") +
    theme_bw()

#### Times Series - Best Combos ####
#Spread
kendall_kate_spread_line <- results_book %>%
    filter(Kendall_Spread_Edge > 2.236 & Kate_Spread_Edge > .599) %>%
    select(2,81) %>%
    mutate(model = "Kendall & Kate") %>%
    rename(value = Kendall_Spread_Result)

kendall_spread_line <- results_book %>%
    filter(Kendall_Spread_Edge >= 2.607) %>%
    select(2,81) %>%
    mutate(model = "Kendall") %>%
    rename(value = Kendall_Spread_Result)

tyra_spread_line <- results_book %>%
    filter(Tyra_Spread_Edge >= 1.503) %>%
    select(2,86) %>%
    mutate(model = "Tyra") %>%
    rename(value = Tyra_Spread_Result)

tyra_cindy_spread_line <- results_book %>%
    filter(Tyra_Spread_Edge > 1.522 & Cindy_Spread_Edge > 1.104) %>%
    select(2,86) %>%
    mutate(model = "Tyra & Cindy") %>%
    rename(value = Tyra_Spread_Result)

adriana_spread_line <- results_book %>%
    filter(Adriana_Spread_Edge > 4.264) %>%
    select(2,111) %>%
    mutate(model = "Adriana") %>%
    rename(value = Adriana_Spread_Result)

spread_combos_line <- bind_rows(kendall_kate_spread_line, kendall_spread_line, tyra_spread_line,
                         tyra_cindy_spread_line, adriana_spread_line)
spread_combos_line %>%
    replace_na(list(value=0)) %>%
    group_by(Date, model) %>%
    summarise(day_total = sum(value)) %>%
    group_by(model) %>%
    mutate(cume = cumsum(day_total)) %>%
    ggplot(aes(x=Date, y=cume, color=model)) +
    geom_line(size=1) +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    xlab("Date") + 
    ylab("Units") +
    theme_bw()

# Spread2
kendall_kate_spread2_line <- results_book %>%
    filter(Kendall_Spread2_Edge > 2.785 & Kate_Spread2_Edge > 2.416) %>%
    select(2,82) %>%
    mutate(model = "Kendall & Kate") %>%
    rename(value = Kendall_Spread2_Result)

kendall_spread2_line <- results_book %>%
    filter(Kendall_Spread2_Edge > 2.776) %>%
    select(2,82) %>%
    mutate(model = "Kendall") %>%
    rename(value = Kendall_Spread2_Result)

kate_spread2_line <- results_book %>%
    filter(Kate_Spread2_Edge > 4.439) %>%
    select(2,97) %>%
    mutate(model = "Kate") %>%
    rename(value = Kate_Spread2_Result)

tyra_adriana_spread2_line <- results_book %>%
    filter(Tyra_Spread2_Edge > 1.522 & Adriana_Spread2_Edge > 1.16) %>%
    select(2,87) %>%
    mutate(model = "Tyra & Adriana") %>%
    rename(value = Tyra_Spread2_Result)

tyra_spread2_line <- results_book %>%
    filter(Tyra_Spread2_Edge > 1.503) %>%
    select(2,87) %>%
    mutate(model = "Tyra") %>%
    rename(value = Tyra_Spread2_Result)

spread2_combos_line <- bind_rows(kendall_kate_spread2_line, kendall_spread2_line, tyra_adriana_spread2_line,
                                 tyra_spread2_line, kate_spread2_line)

spread2_combos_line %>%
    replace_na(list(value=0)) %>%
    group_by(Date, model) %>%
    summarise(day_total = sum(value)) %>%
    group_by(model) %>%
    mutate(cume = cumsum(day_total)) %>%
    ggplot(aes(x=Date, y=cume, color=model)) +
    geom_line() +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    xlab("Date") + 
    ylab("Units") +
    theme_bw()

# Over
gisele_kate_over_line <- results_book %>%
    filter(Gisele_Over_Edge > 8.665) %>%
    select(2,94) %>%
    mutate(model = "Gisele") %>%
    rename(value = Gisele_Over_Result)

kendall_gisele_kate_over_line <- results_book %>%
    filter(Kendall_Over_Edge > 11.345 & Gisele_Over_Edge > 4.417 & Kate_Over_Edge > 2.57) %>%
    select(2,84) %>%
    mutate(model = "Kendall, Gisele & Kate") %>%
    rename(value = Kendall_Over_Result)

over_combos_line <- bind_rows(gisele_kate_over_line, kendall_gisele_kate_over_line)

over_combos_line %>%
    replace_na(list(value=0)) %>%
    group_by(Date, model) %>%
    summarise(day_total = sum(value)) %>%
    group_by(model) %>%
    mutate(cume = cumsum(day_total)) %>%
    ggplot(aes(x=Date, y=cume, color=model)) +
    geom_line() +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    xlab("Date") + 
    ylab("Units") +
    theme_bw()

# Under
kendall_gisele_kate_adriana_under_line <- results_book %>%
    filter(Kendall_Under_Edge >= 2.767 & Gisele_Under_Edge >= 3.069
           & Kate_Under_Edge >= 0.241 & Adriana_Under_Edge >= 2.306) %>%
    select(2,95) %>%
    mutate(model = "Gisele, Kate & Adriana") %>%
    rename(value = Gisele_Under_Result)

gisele_under_line <- results_book %>%
    filter(Gisele_Under_Edge >= 1.643) %>%
    select(2, 95) %>%
    mutate(model = "Gisele") %>%
    rename(value = Gisele_Under_Result)

kendall_naomi_under_line <- results_book %>%
    filter(Kendall_Under_Edge >= 2.216 & Naomi_Under_Edge >= 7.278) %>%
    select(2,85) %>%
    mutate(model = "Kendall & Naomi") %>%
    rename(value = Kendall_Under_Result)

tyra_kate_under_line <- results_book %>%
    filter(Tyra_Under_Edge >= 1.688 & Kate_Under_Edge >= .739) %>%
    select(2,90) %>%
    mutate(model = "Tyra & Kate") %>%
    rename(value = Tyra_Under_Result)

under_combos_line <- bind_rows(kendall_gisele_kate_adriana_under_line, gisele_under_line, kendall_naomi_under_line,
                               tyra_kate_under_line)

under_combos_line %>%
    replace_na(list(value=0)) %>%
    group_by(Date, model) %>%
    summarise(day_total = sum(value)) %>%
    group_by(model) %>%
    mutate(cume = cumsum(day_total)) %>%
    ggplot(aes(x=Date, y=cume, color=model)) +
    geom_line() +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    xlab("Date") + 
    ylab("Units") +
    theme_bw()

#### Win/Loss Analysis ####
results_book %>%
    filter(Kendall_Spread_Edge > 0 & Kendall_Spread_Result != 0) %>%
    mutate(WL = ifelse(Kendall_Spread_Result > 0, "Win", "Loss")) %>%
    ggplot(aes(x=reorder(WL, -Kendall_Spread_Edge), y=Kendall_Spread_Edge, group = WL)) + geom_boxplot()


#### Team Performance - Tables ####
results_book %>%
    filter(Kendall_Spread_Edge > 0) %>%
    select(Team, Kendall_Spread_Result) %>%
    group_by(Team) %>%
    summarise(n = n(), profit = sum(Kendall_Spread_Result), 
              wins = sum(Kendall_Spread_Result>0), losses = sum(Kendall_Spread_Result<0)) %>%
    arrange(desc(profit))

results_book %>%
    filter(Tyra_Spread_Edge > 0) %>%
    select(Team, Tyra_Spread_Result) %>%
    group_by(Team) %>%
    summarise(n = n(), profit = sum(Tyra_Spread_Result), 
              wins = sum(Tyra_Spread_Result>0), losses = sum(Tyra_Spread_Result<0)) %>%
    arrange(desc(profit))

results_book %>%
    filter(Gisele_Spread2_Edge > 0) %>%
    select(Team, Gisele_Spread2_Result) %>%
    group_by(Team) %>%
    summarise(n = n(), profit = sum(Gisele_Spread2_Result), 
              wins = sum(Gisele_Spread2_Result>0), losses = sum(Gisele_Spread2_Result<0)) %>%
    arrange(desc(profit))

results_book %>%
    filter(Kate_Spread2_Edge > 0) %>%
    select(Team, Kate_Spread2_Result) %>%
    group_by(Team) %>%
    summarise(n = n(), profit = sum(Kate_Spread2_Result), 
              wins = sum(Kate_Spread2_Result>0), losses = sum(Kate_Spread2_Result<0)) %>%
    arrange(desc(profit))

results_book %>%
    filter(Cindy_Spread2_Edge > 0) %>%
    select(Team, Cindy_Spread2_Result) %>%
    group_by(Team) %>%
    summarise(n = n(), profit = sum(Cindy_Spread2_Result), 
              wins = sum(Cindy_Spread2_Result>0), losses = sum(Cindy_Spread2_Result<0)) %>%
    arrange(desc(profit))

results_book %>%
    filter(Naomi_Spread2_Edge > 0) %>%
    select(Team, Naomi_Spread2_Result) %>%
    group_by(Team) %>%
    summarise(n = n(), profit = sum(Naomi_Spread2_Result), 
              wins = sum(Naomi_Spread2_Result>0), losses = sum(Naomi_Spread2_Result<0)) %>%
    arrange(desc(profit))

results_book %>%
    filter(Adriana_Spread2_Edge > 0) %>%
    select(Team, Adriana_Spread2_Result) %>%
    group_by(Team) %>%
    summarise(n = n(), profit = sum(Adriana_Spread2_Result), 
              wins = sum(Adriana_Spread2_Result>0), losses = sum(Adriana_Spread2_Result<0)) %>%
    arrange(desc(profit))


#### Team ROI ####
results_book %>%
    filter(Kendall_Spread_Edge > 2.236 & Kate_Spread_Edge > 0.599) %>%
    group_by(Team) %>%
    summarise(n = n(), profit = sum(Kendall_Spread_Result)) %>%
    mutate(roi = (profit/(n*1.1))) %>%
    arrange(desc(profit)) %>%
    left_join(., logos, by = c("Team" = "name")) %>%
    ggplot(aes(x=reorder(Team, roi), y=roi, fill = primary)) +
    geom_bar(stat = "identity") + scale_fill_identity() + coord_flip() +
    labs(title = "Kendall & Kate Performance", subtitle = "vs the spread") +
    scale_y_continuous(breaks = seq(-1, 1, .1)) +
    xlab("Team") +
    ylab("ROI") +
    theme_bw()


results_book %>%
    filter(Kendall_Spread2_Edge > 2.785 & Kate_Spread2_Edge > 2.416) %>%
    group_by(Team) %>%
    summarise(n = n(), profit = sum(Kendall_Spread2_Result)) %>%
    mutate(roi = (profit/(n*1.1))) %>%
    arrange(desc(profit)) %>%
    left_join(., logos, by = c("Team" = "name")) %>%
    ggplot(aes(x=reorder(Team, roi), y=roi, fill = primary)) +
    geom_bar(stat = "identity") + scale_fill_identity() + coord_flip() +
    labs(title = "Kendall & Kate Performance", subtitle = "vs the spread2") +
    scale_y_continuous(breaks = seq(-1, 1, .1)) +
    xlab("Team") +
    ylab("ROI") +
    theme_bw()


results_book %>%
    filter(Gisele_Over_Edge > 8.665) %>%
    group_by(Team) %>%
    summarise(n = n(), profit = sum(Gisele_Over_Result)) %>%
    mutate(roi = (profit/(n*1.1))) %>%
    arrange(desc(profit)) %>%
    left_join(., logos, by = c("Team" = "name")) %>%
    ggplot(aes(x=reorder(Team, roi), y=roi, fill = primary)) +
    geom_bar(stat = "identity") + scale_fill_identity() + coord_flip() +
    labs(title = "Gisele Performance", subtitle = "vs the over") +
    scale_y_continuous(breaks = seq(-1, 1, .1)) +
    xlab("Team") +
    ylab("ROI") +
    theme_bw()


results_book %>%
    filter(Kendall_Under_Edge > 2.767 & Gisele_Under_Edge > 3.069 & 
               Kate_Under_Edge > 0.241 & Adriana_Under_Edge > 2.306) %>%
    group_by(Team) %>%
    summarise(n = n(), profit = sum(Kendall_Under_Result)) %>%
    mutate(roi = (profit/(n*1.1))) %>%
    arrange(desc(profit)) %>%
    left_join(., logos, by = c("Team" = "name")) %>%
    ggplot(aes(x=reorder(Team, roi), y=roi, fill = primary)) +
    geom_bar(stat = "identity") + scale_fill_identity() + coord_flip() +
    labs(title = "Kendall, Gisele, Kate, & Adriana Performance", subtitle = "vs the under") +
    scale_y_continuous(breaks = seq(-1, 1, .1)) +
    xlab("Team") +
    ylab("ROI") +
    theme_bw()

#### Expect vs Actual Score by team ####



