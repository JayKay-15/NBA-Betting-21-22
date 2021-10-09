if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse, readxl, na.tools, caTools, Amelia, lubridate, hms,
               ggthemes, ggrepel, ggimage, XML, RCurl, openxlsx,
               rvest, nflfastR, nbastatR, nbaTools, data.table,
               here, skimr, janitor, SimDesign, zoo, future,
               corrgram, corrplot, tidymodels)
               # nortest, multcomp, agricolae, coin, DTK, mutoss)

# install.packages("devtools")
# library(devtools)
# devtools::install_github("abresler/nbastatR")
# devtools::install_github("ccagrawal/nbaTools")

rm(list=ls())
setwd("/Users/Jesse/Documents/MyStuff/NBA Analysis")

nba <- read_xlsx("/Users/Jesse/Documents/MyStuff/NBA Betting/NBAdb/NBAdb1721.xlsx")


######
# LM # - DONE
######

nba_lin <- nba %>%
    select(7,9:78)

nba_lin <- nba %>% # 0.9697489 - 164.7134
    select(Margin, ORtg_away, DRtg_away , Pace_away , eFG_away , ORB_away ,
           TOV_away , FTR_away , oeFG_away , DRB_away , oTOV_away , 
           oFTR_away , ORtg_home , DRtg_home , Pace_home , eFG_home , ORB_home ,
           TOV_home , FTR_home , oeFG_home , DRB_home , oTOV_home , 
           oFTR_home)

set.seed(214)

sample <- sample.split(nba_lin$Margin, SplitRatio = .70)
train <- nba_lin %>% filter(sample == TRUE)
test <- nba_lin %>% filter(sample == FALSE)

margin_fit <- lm(Margin ~ ., data = train)
summary(margin_fit)

# corrgram(nba_lin)
cor_mx <- cor(nba_lin)
corrplot(cor_mx, method = "color", title = "Correlation Matrix", 
         mar=c(0,0,1,0))

# use this for team analysis
# ggplot(aes(eFG, oeFG, label = Team)) + geom_point(color = "blue") + geom_label_repel(max.overlaps = Inf) + theme_bw()

res_mf <- as.data.frame(residuals(margin_fit))
ggplot(res_mf,aes(residuals(margin_fit))) +  geom_histogram(fill='blue', alpha=0.5, bins = 30)
plot(margin_fit)

anova(margin_fit)
coef(margin_fit)


reg_pred <- predict(margin_fit, newdata = test)
test$pred <- reg_pred
test$dif <- with(test, Margin - round(pred,0))
mean(test$Margin != round(reg_pred,0)) # 0.966895
sum((test$Margin - reg_pred)^2)/nrow(test)


### feature selection four factors

base.mod <- lm(Margin ~ 1, data = train)
all.mod <- lm(Margin ~ ., data = train)

stepMod <- stats::step(base.mod, scope = list(lower = base.mod, upper = all.mod), direction = "both", trace = 0, steps = 1000)  

shortlistedVars <- names(unlist(stepMod[[1]])) 
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"] # remove intercept

# Show
print(shortlistedVars)

nba_lin <- nba %>% # 0.966895 - 164.622
    select(Margin, ORtg_home, ORtg_away, DRtg_home, DRtg_away, ORB_home, TOV_home, oeFG_home, DRB_home, oTOV_home)


### feature selection all features

nba_test <- nba %>%
    select(7,9:78)

set.seed(214)

sample <- sample.split(nba_test$Margin, SplitRatio = .70)
train <- nba_test %>% filter(sample == TRUE)
test <- nba_test %>% filter(sample == FALSE)

base.mod <- lm(Margin ~ 1, data = train)
all.mod <- lm(Margin ~ ., data = train)

stepMod <- stats::step(base.mod, scope = list(lower = base.mod, upper = all.mod), direction = "both", trace = 0, steps = 1000)  

shortlistedVars <- names(unlist(stepMod[[1]])) 
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"] # remove intercept

# Show
print(shortlistedVars)

nba_lin <- nba %>% # 0.9754566 - 163.5436
    select(Margin, ORtg_home, ORtg_away, DRtg_home, DRtg_away, SR2_away, FG_home, oBLK_home, DRB_home, oORB_home,
           oeFG_home, oDRB_away, PF_home, oFG3_away, oAST_away, oTRB_away, oORB_away)


## other method for features
library(caret)

set.seed(214)
rPartMod <- train(Margin ~ ., data = train, method = "rpart")
rpartImp <- varImp(rPartMod)
print(rpartImp)

nba_lin <- nba %>% # 0.9748858 - 167.6115
    select(Margin, oeFG_away, ORtg_away, oeFG_home, ORtg_home, eFG_away, eFG_home, DRtg_away)

# set.seed(214)
# rrfMod <- train(Margin ~ ., data = train, method = "RRF")
# rrfImp <- varImp(rrfMod, scale=F)
# rrfImp


### ML

nba_log <- nba %>%
    select(8,9:78)

nba_log <- nba %>%
    select(Win, ORtg_away, DRtg_away , Pace_away , eFG_away , ORB_away ,
           TOV_away , FTR_away , oeFG_away , DRB_away , oTOV_away , 
           oFTR_away , ORtg_home , DRtg_home , Pace_home , eFG_home , ORB_home ,
           TOV_home , FTR_home , oeFG_home , DRB_home , oTOV_home , 
           oFTR_home)

set.seed(214)

sample <- sample.split(nba_log$Win, SplitRatio = .70)
train <- nba_log %>% filter(sample == TRUE)
test <- nba_log %>% filter(sample == FALSE)

win_fit <- glm(Win ~ ., data = train, family = "binomial")
summary(win_fit)

reg_pred <- predict(win_fit, newdata = test, type = "response")
mean(test$Win != round(reg_pred,0)) # 0.3259005


### different features

nba_log <- nba %>%
    select(Win, ORtg_away, DRtg_away, ORtg_home, DRtg_home, Pace_home, ORB_home, 
           oeFG_home, DRB_home, oTOV_home)

set.seed(214)

sample <- sample.split(nba_log$Win, SplitRatio = .70)
train <- nba_log %>% filter(sample == TRUE)
test <- nba_log %>% filter(sample == FALSE)

win_fit <- glm(Win ~ ., data = train, family = "binomial")
summary(win_fit)

reg_pred <- predict(win_fit, newdata = test, type = "response")
mean(test$Win != round(reg_pred,0)) # 0.3213265


### feature selection

step_model <- win_fit %>% MASS::stepAIC(trace = FALSE)
coef(step_model)


# plots
res_wf <- as.data.frame(residuals(win_fit))
ggplot(res_wf,aes(residuals(win_fit))) +  geom_histogram(fill='blue',alpha=0.5, bins = 30)
plot(win_fit)

anova(win_fit)
coef(win_fit)


########
# KNN # - DONE
#######

library(class)

### Spread

nba_knn <- nba %>%
    select(7,9:78)


nba_knn <- nba %>% # 173.8898 k = 15
    select(Margin, ORtg_away, DRtg_away , Pace_away , eFG_away , ORB_away ,
           TOV_away , FTR_away , oeFG_away , DRB_away , oTOV_away , 
           oFTR_away , ORtg_home , DRtg_home , Pace_home , eFG_home , ORB_home ,
           TOV_home , FTR_home , oeFG_home , DRB_home , oTOV_home , 
           oFTR_home)

nba_knn <- nba %>% # 170.8837 k = 20
    select(Margin, ORtg_home, ORtg_away, DRtg_home, DRtg_away, ORB_home, TOV_home, oeFG_home, DRB_home, oTOV_home)

nba_knn <- nba %>% # 173.3237 k = 25
    select(Margin, ORtg_away, DRtg_away, eFG_away, oeFG_away, ORtg_home, DRtg_home, eFG_home, oeFG_home)

nba_knn <- nba %>% # 170.3282  k = 21
    select(Margin, ORtg_home, ORtg_away, DRtg_home, DRtg_away, ORB_home, TOV_home, oeFG_home, DRB_home, oTOV_home)

knnas_fit <- nba %>% # 170.3282  k = 21
    select(AS, ORtg_home, ORtg_away, DRtg_home, DRtg_away, ORB_home, TOV_home, oeFG_home, DRB_home, oTOV_home)
knnhs_fit <- nba %>% # 170.3282  k = 21
    select(HS, ORtg_home, ORtg_away, DRtg_home, DRtg_away, ORB_home, TOV_home, oeFG_home, DRB_home, oTOV_home)


#### STANDARDIZING STATS ####

##### Margin

marg_scaled <- nba_knn[,1]
maxs <- apply(marg_scaled, 2, max) 
mins <- apply(marg_scaled, 2, min)
marg_scaled <- as.data.frame(scale(marg_scaled, center = mins, scale = maxs - mins))

knnr_train <- nba_knn[,-1]
maxs <- apply(knnr_train, 2, max) 
mins <- apply(knnr_train, 2, min)
knnr_train <- as.data.frame(scale(knnr_train, center = mins, scale = maxs - mins))
# knnr_input <- tail(knnr_train,1)
# knnr_train <- slice(knnr_train, 1:(n()-1))
knnr_train <- bind_cols(marg_scaled, knnr_train)

set.seed(214)
sample <- sample.split(knnr_train$Margin, SplitRatio = .70)
train <- knnr_train %>% filter(sample == TRUE)
test <- knnr_train %>% filter(sample == FALSE)
test$Margin <- test$Margin * (max(nba_knn$Margin) - min(nba_knn$Margin)) + min(nba_knn$Margin)


# predicted_margin <- FNN::knn.reg(train[-1], test[-1], train$Margin, k=20)
# knn_margin <- predicted_margin[["pred"]] * (max(nba_knn$Margin) - min(nba_knn$Margin)) + min(nba_knn$Margin)
# mean((knn_margin - test$Margin ) ^ 2)


predicted_margin = NULL
error_rate = NULL

for(i in 1:500){
    set.seed(214)
    predicted_margin = FNN::knn.reg(train[-1], test[-1], as.numeric(train$Margin), k=i)
    knn_margin <- predicted_margin[["pred"]] * (max(nba_knn$Margin) - min(nba_knn$Margin)) + min(nba_knn$Margin)
    error_rate[i] = mean((knn_margin - test$Margin ) ^ 2)
}

print(error_rate)


k_values <- 1:500
error_df <- data.frame(error_rate, k_values)
order(error_df$error_rate)

ggplot(error_df, aes(k_values, error_rate)) + geom_point()+ geom_line(lty="dotted", color='red')

# ggplot(test,aes(x=Margin, y=pred)) + geom_point() + stat_smooth(method = "lm") + coord_obs_pred()


## alt finding best model

bootControl <- trainControl(number = 1)
knnGrid <- expand.grid(.k=c(2:50))

knnFit1 <- train(train[,-1], train[,1], method = "knn", trControl = bootControl, verbose = FALSE, tuneGrid = knnGrid)

knnFit1$results$Rsquared

knnFit1.sorted <- results[order(results$Rsquared),]
knnFit1.sorted[1,'Rsquared']



##### AS



as_scaled <- knnas_fit[,1]
maxs <- apply(as_scaled, 2, max) 
mins <- apply(as_scaled, 2, min)
as_scaled <- as.data.frame(scale(as_scaled, center = mins, scale = maxs - mins))

knnas_train <- knnas_fit[,-1]
maxs <- apply(knnas_train, 2, max) 
mins <- apply(knnas_train, 2, min)
knnas_train <- as.data.frame(scale(knnas_train, center = mins, scale = maxs - mins))
# knnas_input <- tail(knnas_train,1)
# knnas_train <- slice(knnas_train, 1:(n()-1))
knnas_train <- bind_cols(as_scaled, knnas_train)

set.seed(214)
sample <- sample.split(knnas_train$AS, SplitRatio = .70)
train <- knnas_train %>% filter(sample == TRUE)
test <- knnas_train %>% filter(sample == FALSE)
test$AS <- test$AS * (max(knnas_fit$AS) - min(knnas_fit$AS)) + min(knnas_fit$AS)



# predicted_as <- FNN::knn.reg(train[-1], test[-1], train$AS, k=20)
# knn_as <- predicted_as[["pred"]] * (max(knnas_fit$AS) - min(knnas_fit$AS)) + min(knnas_fit$AS)
# mean((knn_as - test$test_as ) ^ 2)
# test$pred <- predicted_as$pred


predicted_margin = NULL
error_rate = NULL

for(i in 1:500){
    set.seed(214)
    predicted_as = FNN::knn.reg(train[-1], test[-1], as.numeric(train$AS), k=i)
    knn_as <- predicted_as[["pred"]] * (max(knnas_fit$AS) - min(knnas_fit$AS)) + min(knnas_fit$AS)
    error_rate[i] = mean((knn_as - test$AS ) ^ 2)
}

print(error_rate)


k_values <- 1:500
error_df <- data.frame(error_rate, k_values)
error_df
order(error_df$error_rate)

ggplot(error_df, aes(k_values, error_rate)) + geom_point()+ geom_line(lty="dotted", color='red')

ggplot(test,aes(x=AS, y=pred)) + geom_point() + stat_smooth(method = "lm") + coord_obs_pred()


##### HS


hs_scaled <- knnhs_fit[,1]
maxs <- apply(hs_scaled, 2, max) 
mins <- apply(hs_scaled, 2, min)
hs_scaled <- as.data.frame(scale(hs_scaled, center = mins, scale = maxs - mins))

knnhs_train <- knnhs_fit[,-1]
maxs <- apply(knnhs_train, 2, max) 
mins <- apply(knnhs_train, 2, min)
knnhs_train <- as.data.frame(scale(knnhs_train, center = mins, scale = maxs - mins))
# knnas_input <- tail(knnas_train,1)
# knnas_train <- slice(knnas_train, 1:(n()-1))
knnhs_train <- bind_cols(hs_scaled, knnhs_train)

set.seed(214)
sample <- sample.split(knnhs_train$HS, SplitRatio = .70)
train <- knnhs_train %>% filter(sample == TRUE)
test <- knnhs_train %>% filter(sample == FALSE)
test$HS <- test$HS * (max(knnhs_fit$HS) - min(knnhs_fit$HS)) + min(knnhs_fit$HS)



# predicted_hs <- FNN::knn.reg(train[-1], test[-1], train$HS, k=21)
# knn_hs <- predicted_hs[["pred"]] * (max(knnhs_fit$HS) - min(knnhs_fit$HS)) + min(knnhs_fit$HS)
# mean((knn_hs - test$test_hs ) ^ 2)
# test$pred <- predicted_as$pred


predicted_margin = NULL
error_rate = NULL

for(i in 1:500){
    set.seed(214)
    predicted_hs = FNN::knn.reg(train[-1], test[-1], as.numeric(train$HS), k=i)
    knn_hs <- predicted_hs[["pred"]] * (max(knnhs_fit$HS) - min(knnhs_fit$HS)) + min(knnhs_fit$HS)
    error_rate[i] = mean((knn_hs - test$HS ) ^ 2)
}

print(error_rate)


k_values <- 1:500
error_df <- data.frame(error_rate, k_values)
error_df
order(error_df$error_rate)

ggplot(error_df, aes(k_values, error_rate)) + geom_point()+ geom_line(lty="dotted", color='red')

ggplot(test,aes(x=HS, y=pred)) + geom_point() + stat_smooth(method = "lm") + coord_obs_pred()



### ML

nba_knn <- nba %>%
    select(8,9:78)

nba_knn <- nba %>% # 0.3373356 k = 41
    select(Win, ORtg_away, DRtg_away , Pace_away , eFG_away , ORB_away ,
           TOV_away , FTR_away , oeFG_away , DRB_away , oTOV_away , 
           oFTR_away , ORtg_home , DRtg_home , Pace_home , eFG_home , ORB_home ,
           TOV_home , FTR_home , oeFG_home , DRB_home , oTOV_home , 
           oFTR_home)

nba_knn <- nba %>% # 0.3310463 k = 42
    select(Win, ORtg_home, ORtg_away, DRtg_home, DRtg_away, ORB_home, TOV_home, oeFG_home, DRB_home, oTOV_home)

nba_knn <- nba %>% # 0.3350486 k = 22
    select(Win, ORtg_away, DRtg_away, ORtg_home, DRtg_home, Pace_home, ORB_home, oeFG_home, DRB_home, oTOV_home)



win_scaled <- nba_knn[,1]

knnwin_train <- nba_knn[,-1]
maxs <- apply(knnwin_train, 2, max) 
mins <- apply(knnwin_train, 2, min)
knnwin_train <- as.data.frame(scale(knnwin_train, center = mins, scale = maxs - mins))
# knnas_input <- tail(knnas_train,1)
# knnas_train <- slice(knnas_train, 1:(n()-1))
knnwin_train <- bind_cols(win_scaled, knnwin_train)

set.seed(214)
sample <- sample.split(knnwin_train$Win, SplitRatio = .70)
train <- knnwin_train %>% filter(sample == TRUE)
test <- knnwin_train %>% filter(sample == FALSE)




# predicted_win <- knn(train[-1], test[-1], train$Win, k=1)
# mean(test$Win != predicted_win)


predicted_win = NULL
error_rate = NULL

for(i in 1:500){
    set.seed(214)
    predicted_win = predict(knn3(train[,-1], as.factor(train$Win), k=i), test[,-1])
    error_rate[i] = mean(test$Win != predicted_win)
}

print(error_rate)


k_values <- 1:500
error_df <- data.frame(error_rate, k_values)
error_df
order(error_df$error_rate)

ggplot(error_df, aes(k_values, error_rate)) + geom_point()+ geom_line(lty="dotted", color='red')


#################
# Random Forest # - DONE
#################

### Spread

library(randomForest)

nba_rf <- nba %>%
    select(7,9:78)

nba_rf <- nba %>% # 16.62 - 168.3352 - 0.9663242
    select(Margin, ORtg_away, DRtg_away, Pace_away, eFG_away, ORB_away,
           TOV_away, FTR_away, oeFG_away, DRB_away, oTOV_away, 
           oFTR_away, ORtg_home, DRtg_home, Pace_home, eFG_home, ORB_home,
           TOV_home, FTR_home, oeFG_home, DRB_home, oTOV_home, 
           oFTR_home)

nba_rf <- nba %>% # 16.26 - 169.0674 - 0.9708904
    select(Margin, ORtg_away, DRtg_away, eFG_away, oeFG_away, ORtg_home, DRtg_home, eFG_home, oeFG_home, DRB_home)

nba_rf <- nba %>% # 15.8 - 169.993 - 0.9703196
    select(Margin, ORtg_away, DRtg_away, eFG_away, oeFG_away, ORtg_home, DRtg_home, eFG_home, oeFG_home)

nba_rf <- nba %>% # 15.96 - 169.6689 - 0.9674658
    select(Margin, ORtg_home, ORtg_away, DRtg_home, DRtg_away, ORB_home, TOV_home, oeFG_home, DRB_home, oTOV_home)

set.seed(214)

sample <- sample.split(nba_rf$Margin, SplitRatio = .70)
train <- nba_rf %>% filter(sample == TRUE)
test <- nba_rf %>% filter(sample == FALSE)

mtry <- tuneRF(train[-1], train$Margin, ntreeTry=500,
       stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE)
best_m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
# print(mtry)
# print(best_m)

rf_model <- randomForest(Margin ~ ., data = train, mtry = best_m, importance = T)

print(rf_model)

rf_model$importance

importance(rf_model)
varImpPlot(rf_model)

p <- predict(rf_model, test)

test$preds <- p

mean(test$Margin != round(p, 0))

ggplot(test,aes(x=Margin, y=preds)) + geom_point() + stat_smooth(method = "lm") + coord_obs_pred()

### ML

nba_rf <- nba %>%
    select(8,9:78)
nba_rf$Win <- as_factor(nba_rf$Win)

nba_rf <- nba %>% # 34.73% - 0.2170742 - 0.5208691 - 0.329331
    select(Win, ORtg_away, DRtg_away , Pace_away , eFG_away , ORB_away ,
           TOV_away , FTR_away , oeFG_away , DRB_away , oTOV_away , 
           oFTR_away , ORtg_home , DRtg_home , Pace_home , eFG_home , ORB_home ,
           TOV_home , FTR_home , oeFG_home , DRB_home , oTOV_home , 
           oFTR_home)
nba_rf$Win <- as_factor(nba_rf$Win)

nba_rf <- nba %>% # 34.31% - 0.2346632 - 0.4877073 - 0.3470555
    select(Win, ORtg_away, DRtg_away, eFG_away, oeFG_away, ORtg_home, DRtg_home, eFG_home, oeFG_home, DRB_home)
nba_rf$Win <- as_factor(nba_rf$Win)

nba_rf <- nba %>% # 35.56% - 0.2466752 - 0.5008576 - 0.352773
    select(Win, ORtg_away, DRtg_away, eFG_away, oeFG_away, ORtg_home, DRtg_home, eFG_home, oeFG_home)
nba_rf$Win <- as_factor(nba_rf$Win)

nba_rf <- nba %>% # 34% - 0.2256542 - 0.4922813 - 0.3453402
    select(Win, ORtg_away, DRtg_away, ORtg_home, DRtg_home, Pace_home, ORB_home, oeFG_home, DRB_home, oTOV_home)
nba_rf$Win <- as_factor(nba_rf$Win)


set.seed(214)

sample <- sample.split(nba_rf$Win, SplitRatio = .70)
train <- nba_rf %>% filter(sample == TRUE)
test <- nba_rf %>% filter(sample == FALSE)

mtry <- tuneRF(train[-1], train$Win, ntreeTry=500,
       stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE)
best_m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
# print(mtry)
# print(best_m)

rf_model <- randomForest(as_factor(Win) ~ ., data = train, mtry = best_m, importance = T)

print(rf_model)

rf_model$confusion
rf_model$importance

importance(rf_model)
varImpPlot(rf_model)

p <- predict(rf_model, test, type = "prob")
p <- predict(rf_model, test)

test$preds <- p

mean(test$Win != p)

########
# SVM # - DONE
#######

library(e1071)

nba_svm <- nba %>%
    select(7,9:78)

nba_svm <- nba %>% # 0.9646119 - 163.6251 - cost = .4, gamma = .005
    select(Margin, ORtg_away, DRtg_away, Pace_away, eFG_away, ORB_away,
           TOV_away, FTR_away, oeFG_away, DRB_away, oTOV_away, 
           oFTR_away, ORtg_home, DRtg_home, Pace_home, eFG_home, ORB_home,
           TOV_home, FTR_home, oeFG_home, DRB_home, oTOV_home, 
           oFTR_home)

nba_svm <- nba %>% # 0.9623288 - 163.3555 - cost = .4, gamma = .005
    select(Margin, ORtg_home, ORtg_away, DRtg_home, DRtg_away, ORB_home, TOV_home, oeFG_home, DRB_home, oTOV_home)

nba_svm_as <- nba %>% # 0.9623288 - 163.3555 - cost = .4, gamma = .005
    select(AS, ORtg_home, ORtg_away, DRtg_home, DRtg_away, ORB_home, TOV_home, oeFG_home, DRB_home, oTOV_home)

nba_svm_hs <- nba %>% # 0.9623288 - 163.3555 - cost = .4, gamma = .005
    select(HS, ORtg_home, ORtg_away, DRtg_home, DRtg_away, ORB_home, TOV_home, oeFG_home, DRB_home, oTOV_home)

### Margin

set.seed(214)

sample <- sample.split(nba_svm$Margin, SplitRatio = .70)
train <- nba_svm %>% filter(sample == TRUE)
test <- nba_svm %>% filter(sample == FALSE)

tune_results <- tune(svm, train.x = Margin ~ ., data = train, kernel='linear', 
                     ranges = list(cost = c(.25,.4,.5), gamma = c(.005,.1,.25)))
tune_results

svm_model <- svm(Margin ~ ., data = train, cost = .4, gamma = .005, kernal = 'linear')
summary(svm_model)

svm_pred <- predict(svm_model, test[,-1])
test$pred <- svm_pred
mean(test$Margin != round(svm_pred, 0))

rmsd <- mean((predict(svm_model, newdata = test) - test$Margin ) ^ 2)
rmsd 

ggplot(test,aes(x=Margin, y=pred)) + geom_point() + stat_smooth(method = "lm") + coord_obs_pred()


### AS

set.seed(214)

sample <- sample.split(nba_svm_as$AS, SplitRatio = .70)
train <- nba_svm_as %>% filter(sample == TRUE)
test <- nba_svm_as %>% filter(sample == FALSE)

tune_results <- tune(svm, train.x = AS ~ ., data = train, kernel='linear', 
                     ranges = list(cost = c(.6,.5,.4), gamma = c(.005,.004,.006)))
tune_results

svm_model <- svm(AS ~ ., data = train, cost = .5, gamma = .005, kernal = 'linear')
summary(svm_model)

svm_pred <- predict(svm_model, test[,-1])
test$pred <- svm_pred
mean(test$AS != round(svm_pred, 0))

rmsd <- mean((predict(svm_model, newdata = test) - test$AS ) ^ 2)
rmsd 

ggplot(test,aes(x=AS, y=pred)) + geom_point() + stat_smooth(method = "lm") + coord_obs_pred()




### HS

set.seed(214)

sample <- sample.split(nba_svm_hs$HS, SplitRatio = .70)
train <- nba_svm_hs %>% filter(sample == TRUE)
test <- nba_svm_hs %>% filter(sample == FALSE)

tune_results <- tune(svm, train.x = HS ~ ., data = train, kernel='linear', 
                     ranges = list(cost = c(.7,.5,.6), gamma = c(.005,.004,.006)))
tune_results

svm_model <- svm(HS ~ ., data = train, cost = .5, gamma = .005, kernal = 'linear')
summary(svm_model)

svm_pred <- predict(svm_model, test[,-1])
test$pred <- svm_pred
mean(test$HS != round(svm_pred, 0))

rmsd <- mean((predict(svm_model, newdata = test) - test$HS ) ^ 2)
rmsd 

ggplot(test,aes(x=HS, y=pred)) + geom_point() + stat_smooth(method = "lm") + coord_obs_pred()



### ML

nba_svm <- nba %>%
    select(8,9:78)

nba_svm <- nba %>% # 0.335049 - 0.3281875
    select(Win, ORtg_away, DRtg_away, Pace_away, eFG_away, ORB_away,
           TOV_away, FTR_away, oeFG_away, DRB_away, oTOV_away, 
           oFTR_away, ORtg_home, DRtg_home, Pace_home, eFG_home, ORB_home,
           TOV_home, FTR_home, oeFG_home, DRB_home, oTOV_home, 
           oFTR_home)
nba_svm$Win <- as_factor(nba_svm$Win)

nba_svm <- nba %>% # 0.3372549 - 0.3310463
    select(Win, ORtg_away, DRtg_away, eFG_away, oeFG_away, ORtg_home, DRtg_home, eFG_home, oeFG_home, DRB_home)
nba_svm$Win <- as_factor(nba_svm$Win)

nba_svm <- nba %>% # 0.3370098 - 0.3304746
    select(Win, ORtg_away, DRtg_away, eFG_away, oeFG_away, ORtg_home, DRtg_home, eFG_home, oeFG_home)
nba_svm$Win <- as_factor(nba_svm$Win)

nba_svm <- nba %>% # 0.3321078 - 0.3236135
    select(Win, ORtg_away, DRtg_away, ORtg_home, DRtg_home, Pace_home, ORB_home, oeFG_home, DRB_home, oTOV_home)
nba_svm$Win <- as_factor(nba_svm$Win)

set.seed(214)

sample <- sample.split(nba_svm$Win, SplitRatio = .70)
train <- nba_svm %>% filter(sample == TRUE)
test <- nba_svm %>% filter(sample == FALSE)

tune_results <- tune(svm, train.x = Win ~ ., data = train, kernel='radial', 
                     ranges = list(cost = c(.1, .4, .25), gamma = c(.005, .001)))
tune_results

svm_model <- svm(Win ~ ., data = train, cost = .4, gamma = .005, kernal = 'radial')
summary(svm_model)

svm_pred <- predict(svm_model, test[,-1])
#table(svm_pred, test$Win)
mean(test$Win != svm_pred)


########
# NN #
#######

library(neuralnet)
# Spread

nba_nn <- nba %>%
    select(7,9:78)

nba_nn <- nba %>% # 166.4394
    select(Margin, ORtg_away, DRtg_away, Pace_away, eFG_away, ORB_away,
           TOV_away, FTR_away, oeFG_away, DRB_away, oTOV_away,
           oFTR_away, ORtg_home, DRtg_home, Pace_home, eFG_home, ORB_home,
           TOV_home, FTR_home, oeFG_home, DRB_home, oTOV_home,
           oFTR_home)

nba_nn <- nba %>% # 165.3377 - 0.9714612
    select(Margin, ORtg_away, DRtg_away, eFG_away, oeFG_away, ORtg_home, DRtg_home, eFG_home, oeFG_home, DRB_home)

nba_nn <- nba %>% # 165.5106 - 0.9714612
    select(Margin, ORtg_away, DRtg_away, eFG_away, oeFG_away, ORtg_home, DRtg_home, eFG_home, oeFG_home)

nba_nn <- nba %>% # 162.6156 - 0.9680365
    select(Margin, ORtg_home, ORtg_away, DRtg_home, DRtg_away, ORB_home, TOV_home, oeFG_home, DRB_home, oTOV_home)

maxs <- apply(nba_nn, 2, max) 
mins <- apply(nba_nn, 2, min)
scaled <- as.data.frame(scale(nba_nn, center = mins, scale = maxs - mins))
# scaled <- bind_cols(nba_nn[,1], scaled)

set.seed(214)

sample <- sample.split(scaled$Margin, SplitRatio = .70)
train <- scaled%>% filter(sample == TRUE)
test <- scaled %>% filter(sample == FALSE)

n <- names(scaled)
f <- as.formula(paste("Margin ~", paste(n[!n %in% "Margin"], collapse = " + ")))

nn <- neuralnet(f, data = train, hidden = 5, linear.output = TRUE, threshold = .1)
plot(nn)

nn_pred <- compute(nn, test[-1])

true_pred <- nn_pred$net.result * (max(nba_nn$Margin) - min(nba_nn$Margin)) + min(nba_nn$Margin)
test_r <- (test$Margin) * (max(nba_nn$Margin) - min(nba_nn$Margin)) + min(nba_nn$Margin)

MSE.nn <- sum((test_r - true_pred)^2)/nrow(test)
MSE.nn

error_df <- data.frame(test_r,true_pred)

ggplot(error_df, aes(x=test_r,y=true_pred)) + geom_point() + stat_smooth(method = "lm") + coord_obs_pred()

mean(error_df$test_r != round(error_df$true_pred, 0))

### ML

nba_nn <- nba %>%
    select(8,9:78)

nba_nn <- nba %>% # 
    select(Win, ORtg_away, DRtg_away, Pace_away, eFG_away, ORB_away,
           TOV_away, FTR_away, oeFG_away, DRB_away, oTOV_away, 
           oFTR_away, ORtg_home, DRtg_home, Pace_home, eFG_home, ORB_home,
           TOV_home, FTR_home, oeFG_home, DRB_home, oTOV_home, 
           oFTR_home)

nba_nn <- nba %>% # 0.3339051
    select(Win, ORtg_away, DRtg_away, eFG_away, oeFG_away, ORtg_home, DRtg_home, eFG_home, oeFG_home, DRB_home)

nba_nn <- nba %>% # 0.3253288
    select(Win, ORtg_away, DRtg_away, eFG_away, oeFG_away, ORtg_home, DRtg_home, eFG_home, oeFG_home)

nba_nn <- nba %>% # 0.3430532
    select(Win, ORtg_away, DRtg_away, ORtg_home, DRtg_home, Pace_home, ORB_home, oeFG_home, DRB_home, oTOV_home)

maxs <- apply(nba_nn, 2, max) 
mins <- apply(nba_nn, 2, min)
scaled <- as.data.frame(scale(nba_nn, center = mins, scale = maxs - mins))

set.seed(214)

sample <- sample.split(scaled$Win, SplitRatio = .70)
train <- scaled %>% filter(sample == TRUE)
test <- scaled %>% filter(sample == FALSE)

n <- names(scaled)
f <- as.formula(paste("Win ~", paste(n[!n %in% "Win"], collapse = " + ")))

nn <- neuralnet(f, data = train, hidden = 2, linear.output = FALSE, threshold = .1)
plot(nn)

nn_pred <- compute(nn, test[-1])

true_pred <- round(nn_pred$net.result) * (max(nnr_fit$margin) - min(nnr_fit$margin)) + min(nnr_fit$margin)
test_r <- (test$Win) * (max(nba_nn$Win) - min(nba_nn$Win)) + min(nba_nn$Win)

MSE.nn <- sum((test_r - true_pred)^2)/nrow(test)
MSE.nn

error_df <- data.frame(test_r,true_pred)





# Spread

library(neuralnet)  # Neuralnet
library(plyr)       # Progress bar

#-------------------------------------------------------------------------------
# Cross validating function

crossvalidate <- function(data, hidden_l = c(5))
{
    # @params
    
    # data          Boston dataset (data.frame)
    # hidden_l      a numeric vector with number of neurons for each hidden layer
    #               default to 5.
    
    # Scaling the data (min-max scaling)
    maxs <- apply(data, 2, max) 
    mins <- apply(data, 2, min)
    scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
    
    # Initialize cv.error vector
    cv.error <- NULL
    
    # Number of train-test splits
    k <- 10
    
    # Cross validating
    for(j in 1:k)
    {
        # Train-test split
        index <- sample(1:nrow(data),round(0.90*nrow(data)))
        train.cv <- scaled[index,]
        test.cv <- scaled[-index,]
        
        # NN fitting
        nn <- neuralnet(f,data=train.cv,hidden=hidden_l,linear.output=T, threshold = .1)
        
        # Predicting
        pr.nn <- compute(nn,test.cv[-1])
        
        # Scaling back the predicted results
        pr.nn <- pr.nn$net.result*(max(data$Margin)-min(data$Margin))+min(data$Margin)
        
        # Real results
        test.cv.r <- (test.cv$Margin)*(max(data$Margin)-min(data$Margin))+min(data$Margin)
        
        # Calculating MSE test error
        cv.error[j] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
    }
    
    # Return average MSE
    return(mean(cv.error))
}


#-------------------------------------------------------------------------------
# Selecting the number of neurons in the hidden layer

# Data
# data_ <- nba %>% # 162.6156 - 0.9680365
#     select(Margin, ORtg_home, ORtg_away, DRtg_home, DRtg_away, ORB_home, TOV_home, oeFG_home, DRB_home, oTOV_home)

data_ <- nnr_train

# Initializing test and train error vectors
test.error <- NULL
train.error <- NULL

# Scaling for NN
maxs <- apply(data_, 2, max) 
mins <- apply(data_, 2, min)
scaled <- as.data.frame(scale(data_, center = mins, scale = maxs - mins))

n <- names(scaled)
f <- as.formula(paste("Margin ~", paste(n[!n %in% "Margin"], collapse = " + ")))

# Generate progress bar
pbar <- create_progress_bar('text')
pbar$init(10)

set.seed(214)
# Testing and Cross validating (may take a while to compute)
for(i in 1:10) {
    # Fit the net and calculate training error (point estimate)
    nn <- neuralnet(f, data = scaled, hidden = c(i), linear.output = T, threshold = .1)
    # train.error[i] <- sum(((as.data.frame(nn$net.result)*(50-5)+5) - (scaled$Margin*(50-5)+5))^2)/nrow(scaled)
    # (test.cv$Margin)*(max(data$Margin)-min(data$Margin))+min(data$Margin)
    
    # Calculate test error through cross validation
    test.error[i] <- crossvalidate(data_,hidden_l=c(i))
    
    # Step bar
    pbar$step()
}

# Print out test and train error vectors
test.error
train.error

# Plot train error
plot(train.error,main='MSE vs hidden neurons',xlab="Hidden neurons",ylab='Train error MSE',type='l',col='red',lwd=2)
# Plot test error
plot(test.error,main='MSE vs hidden neurons',xlab="Hidden neurons",ylab='Test error MSE',type='l',col='blue',lwd=2)

# Number of neurons (index) that minimizes test/train error
which(min(test.error) == test.error)
which(min(train.error) == train.error)

# ML

#-------------------------------------------------------------------------------
# Cross validating function

crossvalidate <- function(data, hidden_l = c(5))
{
    # @params
    
    # data          Boston dataset (data.frame)
    # hidden_l      a numeric vector with number of neurons for each hidden layer
    #               default to 5.
    
    # Scaling the data (min-max scaling)
    maxs <- apply(data, 2, max) 
    mins <- apply(data, 2, min)
    scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
    
    # Initialize cv.error vector
    cv.error <- NULL
    
    # Number of train-test splits
    k <- 10
    
    # Cross validating
    for(j in 1:k) {
        # Train-test split
        index <- sample(1:nrow(data),round(0.90*nrow(data)))
        train.cv <- scaled[index,]
        test.cv <- scaled[-index,]
        
        # NN fitting
        nn <- neuralnet(f,data=train.cv,hidden=hidden_l,linear.output=F,threshold = .1)
        
        # Predicting
        pr.nn <- compute(nn,test.cv[-1])
        
        # Scaling back the predicted results
        pr.nn <- round(pr.nn$net.result)*(max(data$Win)-min(data$Win))+min(data$Win)
        
        # Real results
        test.cv.r <- (test.cv$Win)*(max(data$Win)-min(data$Win))+min(data$Win)
        
        # Calculating MSE test error
        cv.error[j] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)
    }
    
    # Return average MSE
    return(mean(cv.error))
}


#-------------------------------------------------------------------------------
# Selecting the number of neurons in the hidden layer

# Data
# data_ <- nba %>% # 0.3253288
#     select(Win, ORtg_away, DRtg_away, eFG_away, oeFG_away, ORtg_home, DRtg_home, eFG_home, oeFG_home)

data_ <- nn_fit

# Initializing test and train error vectors
test.error <- NULL
train.error <- NULL

# Scaling for NN
maxs <- apply(data_, 2, max) 
mins <- apply(data_, 2, min)
scaled <- as.data.frame(scale(data_, center = mins, scale = maxs - mins))

n <- names(scaled)
f <- as.formula(paste("Win ~", paste(n[!n %in% "Win"], collapse = " + ")))

# Generate progress bar
pbar <- create_progress_bar('text')
pbar$init(9)

set.seed(214)
# Testing and Cross validating (may take a while to compute)
for(i in 1:9) {
    # Fit the net and calculate training error (point estimate)
    nn <- neuralnet(f, data = scaled, hidden = c(i), linear.output = F, threshold = .1)
    #train.error[i] <- sum(((as.data.frame(nn$net.result)*(50-5)+5) - (scaled$Win*(50-5)+5))^2)/nrow(scaled)
    #(test.cv$Win)*(max(data$Win)-min(data$Win))+min(data$Win)
    
    # Calculate test error through cross validation
    test.error[i] <- crossvalidate(data_,hidden_l=c(i))
    
    # Step bar
    pbar$step()
}

# Print out test and train error vectors
test.error
train.error

# Plot train error
plot(train.error,main='MSE vs hidden neurons',xlab="Hidden neurons",ylab='Train error MSE',type='l',col='red',lwd=2)
# Plot test error
plot(test.error,main='MSE vs hidden neurons',xlab="Hidden neurons",ylab='Test error MSE',type='l',col='blue',lwd=2)

# Number of neurons (index) that minimizes test/train error
which(min(test.error) == test.error)
which(min(train.error) == train.error)






