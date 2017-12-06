#STAT
#wdr5ft
#Final Project

library(calibrate)
library(tidyverse)
library(stats)
library(MASS)
library(leaps)
library(DAAG)
library(olsrr)
library(glmnet)

#########################################################################################################
#                                             Minor Cleaning
#########################################################################################################

remove_special = function(x) gsub('[$%,]', '', x)

#read-in
df = read_csv('fa_clean_D.csv')

#clean names
names(df)[names(df) == 'CAP HIT'] = 'cap_hit'
names(df)[names(df) == 'LY Salary'] = 'ly_salary'
names(df)[names(df) == 'LY Cap Hit'] = 'ly_cap_hit'
names(df)[names(df) == 'pk_TOI/GP'] = 'pk_TOIGP'
names(df)[names(df) == 'E+/-'] = 'Ediff'

#remove non-predictors
player_names = df$PLAYER
df$PLAYER = NULL
df$X1 = NULL
df$VALUE = NULL
df$DATE = NULL
df$POS = NULL
df$`ev_FO%` = NULL
df$Nat = NULL
df$LENGTH = NULL

#convert percentages
percent_cols = names(df)[grepl('%', names(df)) == T]

df[, percent_cols] = apply(df[, percent_cols], 2, remove_special)
df[, percent_cols] = apply(df[, percent_cols], 2, as.numeric)

#convert dollars
df$cap_hit = lapply(df$cap_hit, remove_special)
df$ly_cap_hit = lapply(df$ly_cap_hit, remove_special)
df$ly_salary = lapply(df$ly_salary, remove_special)

df$cap_hit = lapply(df$cap_hit, as.numeric)
df$ly_cap_hit = lapply(df$ly_cap_hit, as.numeric)
df$ly_salary = lapply(df$ly_salary, as.numeric)

df$cap_hit = unlist(df$cap_hit)
df$ly_cap_hit = unlist(df$ly_cap_hit)
df$ly_salary = unlist(df$ly_salary)

#re-type
df = as.data.frame(apply(df, 2, as.numeric))

# df$OTG = as.factor(df$OTG)
# df$pk_G = as.factor(df$pk_G)
# df$ev_iFOW = as.factor(df$ev_iFOW)

#########################################################################################################
#                                         Preliminary Analysis
#########################################################################################################

#plot(cap_hit ~ ., data = df)

#create full model to perform analysis
fit = lm(cap_hit ~ ., data = df)

summary(fit)
anova(fit)

#the r-squared value is very high, likely due to the large number of predictors and multicolinearity

#########################################################################################################
#                                           Multicollinearity
#########################################################################################################

#predictor correlation
cor(df[, -which(names(df) == 'cap_hit')])

#VIF scores
max(vif(fit))
vif(fit)[which(vif(fit) > 1000)]

#------------------------------------------------lasso---------------------------------------------------

#we will perform lasso regression to reduce the multicollinearity as well as the number of predictors

#convert to matrix

df_mat = as.matrix(df)

#fit lasso
fit_lasso = glmnet(df_mat[, -which(names(df) == 'cap_hit')], df_mat[, 2], alpha = 1)

#plot trace
plot(fit_lasso, xvar = "lambda", label = TRUE)

#fit lasso with conservative lambda estimate to see which variables it keeps
fit_lasso = glmnet(df_mat[, -which(names(df) == 'cap_hit')], df_mat[, 2], alpha = 1, lambda = 30000)

#get variables selected by lasso so it can actually run
good_vars = names(df)[which(coef(fit_lasso) != 0)]
good_vars = good_vars[-2]

#cross validate
lasso_cvm = cv.glmnet(df_mat[, which(names(df) %in% good_vars)], df_mat[, 2], alpha = 1)
min_lambda = lasso_cvm$lambda.min

#final lasso model
lasso_cvm = glmnet(df_mat[, which(names(df) %in% good_vars)], df_mat[, 2], alpha = 1, lambda = min_lambda)

lasso_cvm$dev.ratio
#.842

#########################################################################################################
#                                           Residual Analysis 
#########################################################################################################

#having used lasso for variable selection, we will focus on those variables
#because our small sample size doesn't lend itself to large numbers of predictors

df_reduced = df[, which(names(df) %in% good_vars)]
df_reduced$cap_hit = df$cap_hit

fit = lm(cap_hit ~ ., data = df_reduced)
summary(fit)
 
#find the residuals
ei = resid(fit)

#find the studentized residuals
ri = rstandard(fit)

#find the R-student residuals
ti = rstudent(fit)

#PRESS statistic
pr = fit$residuals/(1 - lm.influence(fit)$hat)
press = sum(pr^2)

#Q-Q Plot
qqnorm(fit$residuals)
qqline(fit$residuals)
#the Q-Q plot looks very normal other than some points at the tails

#residuals vs fitted values
preds = predict(fit, df)
plot(preds, fit$residuals)
#odd diagonal line pattern
#this pattern is a result of having a large number of players at the league minimum salary ($650,000)

#residuals vs predictors
for(i in 1:length(good_vars)){
  plot(df[, good_vars[i]], fit$residuals)
}

#18 - slight fan outwards
#16 & 17 - slight fan inwards

#########################################################################################################
#                                               Transform
#########################################################################################################

#-------------------------------------------------pp_A1--------------------------------------------------

#transform pp_A1 because of its inward fan
df_trans = df
df_trans$pp_A1 = sqrt(df$pp_A1)

fit_trans = lm(cap_hit ~ ., data = df_trans)
summary(fit_trans)

#compare residuals
plot(df_trans$pp_A1, fit_trans$residuals)
plot(df$pp_A1, fit$residuals)

#the new residual plot looks much better and the adjusted r-squared is higher
#so we will keep this transformation
df_reduced$pp_A1 = sqrt(df$pp_A1)
fit = lm(cap_hit ~ ., data = df_reduced)
summary(fit)

#--------------------------------------------------pk_A--------------------------------------------------

#transform pk_A because of its inward fan
df_trans$pk_A = sqrt(df$pk_A)

fit_trans = lm(cap_hit ~ ., data = df_trans)
summary(fit_trans)

#compare residuals
plot(df_trans$pk_A, fit_trans$residuals)
plot(df$pk_A, fit$residuals)

#the new residual plot doesn't look much better and the adjusted r-squared has not changed
#so we will discard this transformation
df_trans$pk_A = df$pk_A

#------------------------------------------------pk_TOI/GP-----------------------------------------------

#transform pk_TOI/GP because of its outward fan
df_trans$`pk_TOI/GP` = df$`pk_TOI/GP`^2

fit_trans = lm(cap_hit ~ ., data = df_trans)
summary(fit_trans)

plot(df_trans$`pk_TOI/GP`, fit_trans$residuals)
plot(df$`pk_TOI/GP`, fit$residuals)

#the adjusted r-squared has improved, but the residual plot has not
#changing it is not worth the reduced interpretability
df_trans$`pk_TOI/GP` = df$`pk_TOI/GP`

#########################################################################################################
#                                               Influence
#########################################################################################################

summary(influence.measures(fit))

ols_dfbetas_panel(fit)
ols_cooksd_chart(fit)
ols_rsdlev_plot(fit)

#point 45 is both a leverage point and an outlier

fit_infl1 = lm(cap_hit ~ ., data = df_reduced)
summary(fit_infl1)

#point 29 is both a leverage point and an outlier
fit_infl2 = lm(cap_hit ~ ., data = df_reduced)
summary(fit_infl2)

#test models without the points
cv.lm(data = df[good_vars], form.lm = fit, m = 5, plotit = F)
cv.lm(data = df[good_vars], form.lm = fit_infl1, m = 5, plotit = F)
cv.lm(data = df[good_vars], form.lm = fit_infl2, m = 5, plotit = F)


#while the cross-validated MSE stayed the same after removing leverage points, the r-squared 
#went down, so there isn't enough support to justify removing them

#########################################################################################################
#                                           Variable Selection
#########################################################################################################

#-------------------------------------------------step---------------------------------------------------

fit_null = lm(cap_hit ~ 1, data = df_reduced)
fit_full = lm(cap_hit ~ ., data = df_reduced)

forward = step(fit_null, scope = list(lower = fit_null, upper = fit_full), direction = "forward")
summary(forward)
labels(terms(forward))

#  [1] "ev_ixG"      "pp_G"        "pk_PTS"      "Ovrl"        "ev_iTKA"     "ly_salary"   "AGE"        
#  [8] "ev_Prev3PTS" "pp_Prev3PPG" "pp_PTS"      "ev_G"        "OTG"         "pk_iTKA"     "Wt"         
# [15] "`pp_Pct%`"   "ev_Prev3G"   "ev_iHA" 

backward = step(fit_full, scope = list(lower = fit_null, upper = fit_full), direction = "backward")
summary(backward)
labels(terms(backward))

#  [1] "AGE"         "Wt"          "Ediff"       "OTG"         "`3rd`"       "ly_salary"   "ev_G"       
#  [8] "`ev_F/60`"   "ev_ixG"      "ev_iTKA"     "pp_PTS"      "pk_A"        "pk_TOIGP"    "ev_Prev3G"  
# [15] "pp_Prev3PPG" "pk_Prev3TOI"

stepwise = step(fit_null, scope = list(lower = fit_null, upper = fit_full), direction = "both")
summary(stepwise)

# [1] "ev_ixG"       "pp_G"         "pk_PTS"       "ev_iTKA"      "ly_salary"    "AGE"          "pp_Prev3PPG" 
# [8] "pp_A1"        "ev_G"         "Wt"           "`pp_Pct%`"    "OTG"          "pk_Prev3TOI"  "pk_Prev3Hits"
# [15] "ev_iHA"       "ev_Prev3G"    "`3rd`" 

best1_vars = labels(terms(backward))

best1 = backward

#--------------------------------------------all regressors----------------------------------------------

bestmod <- regsubsets(cap_hit ~ ., data = df_reduced, nbest = 10)
summary(bestmod)

best.sum <- as.data.frame(summary(bestmod)$outmat)
best.sum$p <- as.numeric(substr(rownames(best.sum),1,1))+1

## The criterion values corresponding to each model
best.sum$rss <- summary(bestmod)$rss
best.sum$adjr2 <- summary(bestmod)$adjr2
best.sum$cp <- summary(bestmod)$cp
best.sum$bic <- summary(bestmod)$bic

## get variables of 'best' models by cp
best.sum = best.sum[order(best.sum$cp),]

best_vars2 = names(best.sum)[which(best.sum[1, ] == '*')]
best_vars2 = c(best_vars2, 'cap_hit')

best_vars3 = names(best.sum)[which(best.sum[2, ] == '*')]
best_vars3 = c(best_vars3, 'cap_hit')

#create models
best2 = lm(cap_hit ~ ., data = df[, best_vars2])
summary(best2)

best3 = lm(cap_hit ~ ., data = df[, best_vars3])
summary(best3)

## get variables of 'best' models by r squared
best.sum = best.sum[order(-best.sum$adjr2),]

#best model is the same

#--------------------------------------------cross validation----------------------------------------------

cv.lm(data = df, form.lm = best1, m = 5, plotit = F)
#691315558274 
cv.lm(data = df, form.lm = best2, m = 5, plotit = F)
#571815793540
cv.lm(data = df, form.lm = best3, m = 5, plotit = F)
#601950123709  

#the model with 17 variables attained from the stepwise selection has the highest r squared
#but the lowest cross validated MSE

#-----------------------------------------manual cross validation----------------------------------------------

#manually cross validate for lasso regression

sub = sample(1:nrow(df), size = 0.8*nrow(df))
train = df[sub, ]
valid = df[-sub, ]

train_mat = as.matrix(train)
test_mat = as.matrix(valid)

#create model
lasso_cvm_cv = glmnet(train_mat[, which(names(df) %in% good_vars)], train_mat[, 2], alpha = 1, lambda = min_lambda)
lasso_preds = predict(lasso_cvm, test_mat[, which(names(df) %in% good_vars)])

#MSE
mean((lasso_preds - valid$cap_hit)^2)
#326943770438

#########################################################################################################
#                                              WINNER
#########################################################################################################

#the lasso performs best, but is not very interpretable and has 33 variables

lasso_cvm$beta@Dimnames[[1]]

#  [1] "AGE"           "Wt"            "Ovrl"          "Ediff"         "OTG"           "GWG"          
#  [7] "NPD"           "3rd"           "ly_salary"     "ev_G"          "ev_A1"         "ev_IPP%"      
# [13] "ev_F/60"       "ev_ixG"        "ev_iSCF"       "ev_iHA"        "ev_iTKA"       "pp_G"         
# [19] "pp_A1"         "pp_PTS"        "pp_Pct%"       "pk_A"          "pk_PTS"        "pk_TOIGP"     
# [25] "pk_iTKA"       "ev_Prev3G"     "ev_Prev3PTS"   "ev_Prev3Corsi" "pp_Prev3PPG"   "pk_Prev3Hits" 
# [31] "pk_Prev3TOI"  


#########################################################################################################
#                                              graphs
#########################################################################################################

#histogram of predictions
best_preds = predict(lasso_cvm, df_mat[, good_vars])
min(best_preds)

bins = seq(min(best_preds), 8000000, 100000)

hist(best_preds, breaks = bins)


df_mat = as.matrix(df)
lasso_preds = predict(lasso_cvm, df_mat[, which(names(df) %in% good_vars)])
pred_df = data.frame(name = player_names, preds = lasso_preds, actual = df$cap_hit)
names(pred_df) = c('name', 'preds', 'actual')
#write_csv(pred_df, 'def_pred_df.csv')

forwards = read_csv('Prediction_Forwards.csv')
forwards$X1 = NULL
names(forwards) = names(pred_df)

full_pred_df = rbind(pred_df, forwards)
full_pred_df = full_pred_df[order(full_pred_df$preds), ]
full_pred_df$x = seq(1, nrow(full_pred_df))

ggplot() + geom_point(data = full_pred_df, aes(x = x, y = actual, colour = 'observed')) + 
           geom_point(data = full_pred_df, aes(x = x, y = preds, colour = 'predicted')) + 
           scale_color_manual('', values = c('observed' = 'black', 'predicted' = 'seagreen')) +
           labs(x = 'player', y = 'cap hit')
lasso_cvm$dev.ratio

ggplot(pred_df, aes(x = preds)) + geom_histogram(color="black", fill="white", binwidth = 100000)



