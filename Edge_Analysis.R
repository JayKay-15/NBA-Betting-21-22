#### EDGE ANALYSIS ####

if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, readxl, lubridate, openxlsx, nbastatR, rvest, data.table)

rm(list=ls())

setwd("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/")

options(scipen = 999)

results_book <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/Results.xlsx")
results_book$Date <- as_date(results_book$Date)

# fil <- 21
# results_book <- results_book %>%
#     filter(Date >= max(Date) - fil)
# results_book <- results_book %>%
#     tail(300)
results_book <- results_book %>%
    filter(Date >= '2022-04-15')

### Add Wager columns for calculating ML ROI
results_book <- results_book %>%
    mutate(ML_Wager = ifelse(ML < 100, ML/-100, 1))

combos_s <- lapply(1:7, function(x) combn(c("Kendall_Spread_Edge","Tyra_Spread_Edge","Gisele_Spread_Edge",
                                             "Kate_Spread_Edge","Cindy_Spread_Edge","Naomi_Spread_Edge",
                                             "Adriana_Spread_Edge"), x, simplify = FALSE))
combos_s <- unlist(combos_s, recursive = F)

combos_result_s <- lapply(1:7, function(x) combn(c("Kendall_Spread_Result","Tyra_Spread_Result","Gisele_Spread_Result",
                                                    "Kate_Spread_Result","Cindy_Spread_Result","Naomi_Spread_Result",
                                                    "Adriana_Spread_Result"), x, simplify = FALSE))
combos_result_s <- unlist(combos_result_s, recursive = F)

combos_s2 <- lapply(1:7, function(x) combn(c("Kendall_Spread2_Edge","Tyra_Spread2_Edge","Gisele_Spread2_Edge",
                                          "Kate_Spread2_Edge","Cindy_Spread2_Edge","Naomi_Spread2_Edge",
                                          "Adriana_Spread2_Edge"), x, simplify = FALSE))
combos_s2 <- unlist(combos_s2, recursive = F)

combos_result_s2 <- lapply(1:7, function(x) combn(c("Kendall_Spread2_Result","Tyra_Spread2_Result","Gisele_Spread2_Result",
                                                 "Kate_Spread2_Result","Cindy_Spread2_Result","Naomi_Spread2_Result",
                                                 "Adriana_Spread2_Result"), x, simplify = FALSE))
combos_result_s2 <- unlist(combos_result_s2, recursive = F)

combos_m <- lapply(1:7, function(x) combn(c("Kendall_ML_Edge","Tyra_ML_Edge","Gisele_ML_Edge",
                                             "Kate_ML_Edge","Cindy_ML_Edge","Naomi_ML_Edge",
                                             "Adriana_ML_Edge"), x, simplify = FALSE))
combos_m <- unlist(combos_m, recursive = F)

combos_result_m <- lapply(1:7, function(x) combn(c("Kendall_ML_Result","Tyra_ML_Result","Gisele_ML_Result",
                                                    "Kate_ML_Result","Cindy_ML_Result","Naomi_ML_Result",
                                                    "Adriana_ML_Result"), x, simplify = FALSE))
combos_result_m <- unlist(combos_result_m, recursive = F)

combos_o <- lapply(1:7, function(x) combn(c("Kendall_Over_Edge","Tyra_Over_Edge","Gisele_Over_Edge",
                                             "Kate_Over_Edge","Cindy_Over_Edge","Naomi_Over_Edge",
                                             "Adriana_Over_Edge"), x, simplify = FALSE))
combos_o <- unlist(combos_o, recursive = F)


combos_result_o <- lapply(1:7, function(x) combn(c("Kendall_Over_Result","Tyra_Over_Result","Gisele_Over_Result",
                                                    "Kate_Over_Result","Cindy_Over_Result","Naomi_Over_Result",
                                                    "Adriana_Over_Result"), x, simplify = FALSE))
combos_result_o <- unlist(combos_result_o, recursive = F)

combos_u <- lapply(1:7, function(x) combn(c("Kendall_Under_Edge","Tyra_Under_Edge","Gisele_Under_Edge",
                                             "Kate_Under_Edge","Cindy_Under_Edge","Naomi_Under_Edge",
                                             "Adriana_Under_Edge"), x, simplify = FALSE))
combos_u <- unlist(combos_u, recursive = F)


combos_result_u <- lapply(1:7, function(x) combn(c("Kendall_Under_Result","Tyra_Under_Result","Gisele_Under_Result",
                                                    "Kate_Under_Result","Cindy_Under_Result","Naomi_Under_Result",
                                                    "Adriana_Under_Result"), x, simplify = FALSE))
combos_result_u <- unlist(combos_result_u, recursive = F)




### Spread
results_s <- list()
for (i in seq_along(combos_s)) {
    nms <- combos_s[[i]]
    rslt <- combos_result_s[[i]]
    results_s[[paste0(nms, collapse = "_")]] <- results_book %>% 
        filter(if_all(all_of(nms), ~ .> 0)) %>%
        select(combos_s[[i]], combos_result_s[[i]], "Loc") %>%
        mutate(Result = rowMeans(select(.,ends_with("Result")), na.rm = TRUE)) %>%
        select("Loc","Result", ends_with("Edge")) %>%
        arrange(desc(select(.,ends_with("Edge")))) %>%
        mutate(Cume = round(cumsum(Result),1)) %>%
        mutate(gameNum = row_number()) %>%
        mutate(ROI = round((Cume /(gameNum*1.1))*100,2)) %>%
        select("gameNum","Loc","Result","Cume","ROI",ends_with("Edge"))
    
}

peaker_s <- list()
for (j in seq_along(results_s)) {
    for (i in 6:ncol(results_s[[j]])) {
        x <- results_s[[j]][order(-results_s[[j]][,4]), ]
        peaker_s[[j]] <- head(x,1)
    }
}

peak_list_s <- map(peaker_s, as.data.table)
spread_peak <- rbindlist(peak_list_s, fill = TRUE, idcol = F)
spread_peak <- spread_peak %>% select(1,4:12) %>% arrange(desc(Cume))

spread_peak_filtered <- spread_peak %>%
    mutate(num_models = rowSums(is.na(spread_peak))) %>%
    filter(num_models == 6) %>%
    select(1:10)

spread_peak_filtered <- spread_peak_filtered %>%
    pivot_longer(cols = !gameNum:ROI, names_to = "Model", values_to = "Key") %>%
    drop_na()


### Spread2
results_s2 <- list()
for (i in seq_along(combos_s2)) {
    nms <- combos_s2[[i]]
    rslt <- combos_result_s2[[i]]
    results_s2[[paste0(nms, collapse = "_")]] <- results_book %>% 
        filter(if_all(all_of(nms), ~ .> 0)) %>%
        select(combos_s2[[i]], combos_result_s2[[i]], "Loc") %>%
        mutate(Result = rowMeans(select(.,ends_with("Result")), na.rm = TRUE)) %>%
        select("Loc","Result", ends_with("Edge")) %>%
        arrange(desc(select(.,ends_with("Edge")))) %>%
        mutate(Cume = round(cumsum(Result),1)) %>%
        mutate(gameNum = row_number()) %>%
        mutate(ROI = round((Cume /(gameNum*1.1))*100,2)) %>%
        select("gameNum","Loc","Result","Cume","ROI",ends_with("Edge"))
    
}

peaker_s2 <- list()
for (j in seq_along(results_s2)) {
    for (i in 6:ncol(results_s2[[j]])) {
        x <- results_s2[[j]][order(-results_s2[[j]][,4]), ]
        peaker_s2[[j]] <- head(x,1)
    }
}

peak_list_s2 <- map(peaker_s2, as.data.table)
spread2_peak <- rbindlist(peak_list_s2, fill = TRUE, idcol = F)
spread2_peak <- spread2_peak %>% select(1,4:12) %>% arrange(desc(Cume))

spread2_peak_filtered <- spread2_peak %>%
    mutate(num_models = rowSums(is.na(spread2_peak))) %>%
    filter(num_models == 6) %>%
    select(1:10)

spread2_peak_filtered <- spread2_peak_filtered %>%
    pivot_longer(cols = !gameNum:ROI, names_to = "Model", values_to = "Key") %>%
    drop_na()



### ML
results_m <- list()
for (i in seq_along(combos_m)) {
    nms <- combos_m[[i]]
    rslt <- combos_result_m[[i]]
    results_m[[paste0(nms, collapse = "_")]] <- results_book %>% 
        filter(if_all(all_of(nms), ~ .> 0)) %>%
        select(combos_m[[i]], combos_result_m[[i]], "Loc", "ML_Wager") %>%
        mutate(Result = rowMeans(select(.,ends_with("Result")), na.rm = TRUE)) %>%
        select("Loc","Result", ends_with("Edge"), "ML_Wager") %>%
        arrange(desc(select(.,ends_with("Edge"), "ML_Wager"))) %>%
        mutate(Cume = round(cumsum(Result),1)) %>%
        mutate(gameNum = row_number()) %>%
        mutate(ROI = round((Cume / cumsum(ML_Wager))*100,2)) %>%
        select("gameNum","Loc","Result","Cume", "ROI", ends_with("Edge"))
    
}

peaker_m <- list()
for (j in seq_along(results_m)) {
    for (i in 6:ncol(results_m[[j]])) {
        x <- results_m[[j]][order(-results_m[[j]][,4]), ]
        peaker_m[[j]] <- head(x,1)
    }
}

peak_list_m <- map(peaker_m, as.data.table)
ml_peak <- rbindlist(peak_list_m, fill = TRUE, idcol = F)
ml_peak <- ml_peak %>% select(1,4:12) %>% arrange(desc(Cume))

ml_peak_filtered <- ml_peak %>%
    mutate(num_models = rowSums(is.na(ml_peak))) %>%
    filter(num_models == 6) %>%
    select(1:10)

ml_peak_filtered <- ml_peak_filtered %>%
    pivot_longer(cols = !gameNum:ROI, names_to = "Model", values_to = "Key") %>%
    drop_na()



### Over
results_o <- list()
for (i in seq_along(combos_o)) {
    nms <- combos_o[[i]]
    rslt <- combos_result_o[[i]]
    results_o[[paste0(nms, collapse = "_")]] <- results_book %>% 
        filter(if_all(all_of(nms), ~ .> 0)) %>%
        select(combos_o[[i]], combos_result_o[[i]], "Loc") %>%
        mutate(Result = rowMeans(select(.,ends_with("Result")), na.rm = TRUE)) %>%
        select("Loc","Result", ends_with("Edge")) %>%
        arrange(desc(select(.,ends_with("Edge")))) %>%
        mutate(Cume = round(cumsum(Result),1)) %>%
        mutate(gameNum = row_number()) %>%
        mutate(ROI = round((Cume /(gameNum*1.1))*100,2)) %>%
        select("gameNum","Loc","Result","Cume","ROI",ends_with("Edge"))
    
}

peaker_o <- list()
for (j in seq_along(results_o)) {
    for (i in 6:ncol(results_o[[j]])) {
        x <- results_o[[j]][order(-results_o[[j]][,4]), ]
        peaker_o[[j]] <- head(x,1)
    }
}

peak_list_o <- map(peaker_o, as.data.table)
over_peak <- rbindlist(peak_list_o, fill = TRUE, idcol = F)
over_peak <- over_peak %>% select(1,4:12) %>% arrange(desc(Cume))

over_peak_filtered <- over_peak %>%
    mutate(num_models = rowSums(is.na(over_peak))) %>%
    filter(num_models == 6) %>%
    select(1:10)

over_peak_filtered <- over_peak_filtered %>%
    pivot_longer(cols = !gameNum:ROI, names_to = "Model", values_to = "Key") %>%
    drop_na()


### Under
results_u <- list()
for (i in seq_along(combos_u)) {
    nms <- combos_u[[i]]
    rslt <- combos_result_u[[i]]
    results_u[[paste0(nms, collapse = "_")]] <- results_book %>% 
        filter(if_all(all_of(nms), ~ .> 0)) %>%
        select(combos_u[[i]], combos_result_u[[i]], "Loc") %>%
        mutate(Result = rowMeans(select(.,ends_with("Result")), na.rm = TRUE)) %>%
        select("Loc","Result", ends_with("Edge")) %>%
        arrange(desc(select(.,ends_with("Edge")))) %>%
        mutate(Cume = round(cumsum(Result),1)) %>%
        mutate(gameNum = row_number()) %>%
        mutate(ROI = round((Cume /(gameNum*1.1))*100,2)) %>%
        select("gameNum","Loc","Result","Cume","ROI",ends_with("Edge"))
    
}

peaker_u <- list()
for (j in seq_along(results_u)) {
    for (i in 6:ncol(results_u[[j]])) {
        x <- results_u[[j]][order(-results_u[[j]][,4]), ]
        peaker_u[[j]] <- head(x,1)
    }
}

peak_list_u <- map(peaker_u, as.data.table)
under_peak <- rbindlist(peak_list_u, fill = TRUE, idcol = F)
under_peak <- under_peak %>% select(1,4:12) %>% arrange(desc(Cume))

under_peak_filtered <- under_peak %>%
    mutate(num_models = rowSums(is.na(under_peak))) %>%
    filter(num_models == 6) %>%
    select(1:10)

under_peak_filtered <- under_peak_filtered %>%
    pivot_longer(cols = !gameNum:ROI, names_to = "Model", values_to = "Key") %>%
    drop_na()



#### Print to Excel ####
# fn <- "Keys_14"
# u <- paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/",fn,".xlsx")
# 
# wb <- loadWorkbook("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/Keys.xlsx")
# deleteData(wb, "Spread Peak", gridExpand = F, cols = 1:130, rows = 1:130)
# deleteData(wb, "Spread2 Peak", gridExpand = F, cols = 1:130, rows = 1:130)
# deleteData(wb, "ML Peak", gridExpand = F, cols = 1:130, rows = 1:130)
# deleteData(wb, "Over Peak", gridExpand = F, cols = 1:130, rows = 1:130)
# deleteData(wb, "Under Peak", gridExpand = F, cols = 1:130, rows = 1:130)
# deleteData(wb, "Spread Peak Filtered", gridExpand = F, cols = 1:130, rows = 1:130)
# deleteData(wb, "Spread2 Peak Filtered", gridExpand = F, cols = 1:130, rows = 1:130)
# deleteData(wb, "ML Peak Filtered", gridExpand = F, cols = 1:130, rows = 1:130)
# deleteData(wb, "Over Peak Filtered", gridExpand = F, cols = 1:130, rows = 1:130)
# deleteData(wb, "Under Peak Filtered", gridExpand = F, cols = 1:130, rows = 1:130)
# writeData(wb, "Spread Peak", x = spread_peak)
# writeData(wb, "Spread2 Peak", x = spread2_peak)
# writeData(wb, "ML Peak", x = ml_peak)
# writeData(wb, "Over Peak", x = over_peak)
# writeData(wb, "Under Peak", x = under_peak)
# writeData(wb, "Spread Peak Filtered", x = spread_peak_filtered)
# writeData(wb, "Spread2 Peak Filtered", x = spread2_peak_filtered)
# writeData(wb, "ML Peak Filtered", x = ml_peak_filtered)
# writeData(wb, "Over Peak Filtered", x = over_peak_filtered)
# writeData(wb, "Under Peak Filtered", x = under_peak_filtered)
# saveWorkbook(wb, u, overwrite = T)


#### Creating workbook

fn <- "Keys"
u <- paste0("/Users/Jesse/Documents/MyStuff/NBA Betting/NBA-Betting-21-22/",fn,".xlsx")

wb <- createWorkbook()
addWorksheet(wb, sheetName = "Spread Peak Filtered")
addWorksheet(wb, sheetName = "Spread2 Peak Filtered")
addWorksheet(wb, sheetName = "ML Peak Filtered")
addWorksheet(wb, sheetName = "Over Peak Filtered")
addWorksheet(wb, sheetName = "Under Peak Filtered")
addWorksheet(wb, sheetName = "Spread Peak")
addWorksheet(wb, sheetName = "Spread2 Peak")
addWorksheet(wb, sheetName = "ML Peak")
addWorksheet(wb, sheetName = "Over Peak")
addWorksheet(wb, sheetName = "Under Peak")
writeData(wb, "Spread Peak Filtered", x = spread_peak_filtered)
writeData(wb, "Spread2 Peak Filtered", x = spread2_peak_filtered)
writeData(wb, "ML Peak Filtered", x = ml_peak_filtered)
writeData(wb, "Over Peak Filtered", x = over_peak_filtered)
writeData(wb, "Under Peak Filtered", x = under_peak_filtered)
writeData(wb, sheet = "Spread Peak", x = spread_peak)
writeData(wb, sheet = "Spread2 Peak", x = spread2_peak)
writeData(wb, sheet = "ML Peak", x = ml_peak)
writeData(wb, sheet = "Over Peak", x = over_peak)
writeData(wb, sheet = "Under Peak", x = under_peak)
saveWorkbook(wb, file = u)


