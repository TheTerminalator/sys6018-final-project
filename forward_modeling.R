# Data Cleaning
library(tidyverse)
library(DAAG)
library(glmnet)
library(MASS)
library(leaps)
# setwd("~/Desktop/UVA DSI/STAT 6021/NHL_WAR")
setwd("/home/yingjie/Desktop/NHL_WAR")

# Read in files

main14 <- read_csv('NHL 2013-14_main.csv')
ev14 <- read_csv('NHL 2013-14_ev.csv')
pp14 <- read_csv('NHL 2013-14_pp.csv')
pk14 <- read_csv('NHL 2013-14_pk.csv')

#ev14 <- ev14[!is.na(ev14$`First Name`),]

main15 <- read_csv('NHL 2014-15_main.csv')
ev15 <- read_csv('NHL 2014-15_ev.csv')
pp15 <- read_csv('NHL 2014-15_pp.csv')
pk15 <- read_csv('NHL 2014-15_pk.csv')

main16 <- read_csv('NHL 2015-16_main.csv')
ev16 <- read_csv('NHL 2015-16_ev.csv')
pp16 <- read_csv('NHL 2015-16_pp.csv')
pk16 <- read_csv('NHL 2015-16_pk.csv')

main17 <- read_csv('NHL 2016-17_main.csv')
ev17 <- read_csv('NHL 2016-17_ev.csv')
pp17 <- read_csv('NHL 2016-17_pp.csv')
pk17 <- read_csv('NHL 2016-17_pk.csv')

fa <- read_csv('fa_signings.csv')


# Drop necessary columns/rows
fa$TEAM <- NULL
fa <- fa[fa$TYPE!='Entry-Level',]
fa <- fa[fa$POS!='G',]
fa$TYPE <- NULL

# Clean player names

# fa[7,'PLAYER'] <- 'Roman Polak'
# fa[18,'PLAYER'] <- 'Jaromir Jagr'
# fa[45,'PLAYER'] <- 'Jean-Sebastien Dea'
# fa[92,'PLAYER'] <- 'Marcus Sorensen'
# fa[94,'PLAYER'] <- 'Felix Girard'
# fa[98,'PLAYER'] <- 'Frederick Gaudreau'
# fa[117,'PLAYER'] <- 'Michael Bournival'
# fa[160,'PLAYER'] <- 'Andre Burakovsky'
# fa[215,'PLAYER'] <- 'Andre Benoit'
# fa[252,'PLAYER'] <- 'Marek Hrivik'
# fa[281,'PLAYER'] <- 'Marc-Edouard Vlasic'
# fa[286,'PLAYER'] <- 'Pierre-Cedric Labrie'
# fa[298,'PLAYER'] <- 'Anton Rodin'
# fa[308,'PLAYER'] <- 'Magnus Paajarvi'
# fa[357,'PLAYER'] <- 'Ludwig Bystrom'
# fa[362,'PLAYER'] <- 'Teuvo Teravainen'
fa[39,'PLAYER'] <- 'Alex Wennberg'



# Select relevant columns 
main17_names <- c('Nat','Ht','Wt','DftYr','DftRd','Ovrl','Last Name','First Name',
                  'GP','Grit','1st','2nd','3rd','Salary','Cap Hit','PIM','+/-','E+/-',
                  'OTG','GWG','iPEND','iPENT','NPD','xGF','xGA')

ev17_names <- c('First Name','Last Name','G','A','A1','PTS','IPP%','SH%','PDO','F/60','A/60','Pct%','iCF','iFF','iSF','ixG',
                'iSCF','iRB','iRS','iDS','Pass','iHF','iHA','iGVA','iTKA','iBLK','iFOW',
                'FO%','CF','CA','FF','FA','SCF','SCA','RelPct%')

pp17_names <- c('First Name','Last Name','G','A','A1','PTS','TOI','TOI/GP','F/60','A/60','Pct%','RelPct%')

pk17_names <- c('First Name','Last Name','G','A','PTS','TOI','TOI/GP','iBLK','iTKA','F/60','A/60','Pct%','RelPct%')

# subset data frames with correct columns
main17_cl <- main17[ , (names(main17) %in% main17_names)]
pp17_cl <- pp17[ , (names(pp17) %in% pp17_names)]
pk17_cl <- pk17[ , (names(pk17) %in% pk17_names)]
ev17_cl <- ev17[ , (names(ev17) %in% ev17_names)]

main17_cl$FullName <- paste(main17_cl$`First Name`,main17_cl$`Last Name`, sep=' ')
ev17_cl$FullName <- paste(main17_cl$`First Name`,main17_cl$`Last Name`, sep=' ')
pp17_cl$FullName <- paste(main17_cl$`First Name`,main17_cl$`Last Name`, sep=' ')
pk17_cl$FullName <- paste(main17_cl$`First Name`,main17_cl$`Last Name`, sep=' ')

drops <- c('First Name','Last Name')
main17_cl <- main17_cl[ , !(names(main17_cl) %in% drops)]
pp17_cl <- pp17_cl[ , !(names(pp17_cl) %in% drops)]
pk17_cl <- pk17_cl[ , !(names(pk17_cl) %in% drops)]
ev17_cl <- ev17_cl[ , !(names(ev17_cl) %in% drops)]

names(pk17_cl) <- paste('pk', names(pk17_cl), sep='_')
names(pp17_cl) <- paste('pp', names(pp17_cl), sep='_')
names(ev17_cl) <- paste('ev', names(ev17_cl), sep='_')

names(pp17_cl)[names(pp17_cl) == 'pp_FullName'] <- 'FullName'
names(pk17_cl)[names(pk17_cl) == 'pk_FullName'] <- 'FullName'
names(ev17_cl)[names(ev17_cl) == 'ev_FullName'] <- 'FullName'


# Combine Data Frames
merged17 <- Reduce(function(x, y) merge(x, y, by=c('FullName')), 
                   list(main17_cl, ev17_cl, pp17_cl, pk17_cl))

fa_clean <- left_join(fa, merged17, by= c('PLAYER'='FullName'))

names(fa_clean)[names(fa_clean) == 'Cap Hit'] <- 'LY Cap Hit'
names(fa_clean)[names(fa_clean) == 'Salary'] <- 'LY Salary'


fa_clean_D <- fa_clean[fa_clean$POS == 'D',]
fa_clean_F <- fa_clean[!fa_clean$POS == 'D',]


############### Further clean the data for modeling ###################################
clean_F = na.omit(fa_clean_F)

# Remove Name and Date (non-relevant variables)
clean_F = clean_F[,-c(1,4)]

# Check data types of each column
sapply(clean_F,class)

# Convert VALUE, CAP HIT, LY Salary, LY Cap Hit from $ to numeric value
clean_F$VALUE = as.numeric(gsub('[$,]', '', clean_F$VALUE))
clean_F$`CAP HIT` = as.numeric(gsub('[$,]', '', clean_F$`CAP HIT`))
clean_F$`LY Salary` = as.numeric(gsub('[$,]', '', clean_F$`LY Salary`))
clean_F$`LY Cap Hit` = as.numeric(gsub('[$,]', '', clean_F$`LY Cap Hit`))

# Convert % value from character type to numeric type
name_perc = names(clean_F[,grepl( "%" , names(clean_F))])
clean_F[,name_perc] = sapply(clean_F[,name_perc], function(x) gsub("%","",as.character(x)))
clean_F[,name_perc] = sapply(clean_F[,name_perc], as.numeric)

# Convert interger value to numeric
name_non_int = c(name_perc,'Nat','POS')
name_int = !names(clean_F) %in% name_non_int
clean_F[,name_int] = sapply(clean_F[,name_int], as.numeric)


# Double check data types of each column
sapply(clean_F,class)

# Check if Cap Hit = Value/Length
sum(clean_F$VALUE/clean_F$LENGTH == clean_F$`CAP HIT`) # Most of them is true
## Investigate False
clean_F[clean_F$VALUE/clean_F$LENGTH != clean_F$`CAP HIT`,] # one difference due to rounding; not sure of others. Since there are only 6 observations where value/length not equal to Cap Hit, ignore the difference. 

# Remove Value and Length
clean_F = clean_F[,!names(clean_F) %in% c("LENGTH","VALUE")]

# Separte POS into individual columns
clean_F$POS_C <- ifelse(grepl("C", clean_F$POS), 1, 0)
clean_F$POS_LW <- ifelse(grepl("LW", clean_F$POS), 1, 0)
clean_F$POS_RW <- ifelse(grepl("RW", clean_F$POS), 1, 0)

# Remove POS col
clean_F = clean_F[,!names(clean_F) %in% c("POS")]

# Factor Nat
clean_F$Nat = as.factor(clean_F$Nat)
sapply(clean_F,class)

# Change variables name
names(clean_F)[names(clean_F) == 'CAP HIT'] = 'cap_hit'
names(clean_F)[names(clean_F) == 'LY Salary'] = 'ly_salary'
names(clean_F)[names(clean_F) == 'LY Cap Hit'] = 'ly_cap_hit'


#### Create another column
clean_F$cap_hit_c = clean_F$cap_hit/10^6 
clean_F = clean_F[!(clean_F$cap_hit==0),]
clean_F = clean_F[,!names(clean_F)%in% 'cap_hit']

### Linear regression model
# Data Exploration
hist(clean_F$AGE)
hist(clean_F$cap_hit_c) # Might be an outlier at 1.4*10^7
plot(clean_F$Nat) # Might separate into Can/US and non-Can/US
hist(clean_F$Ht)
hist(clean_F$Wt)
hist(clean_F$DftYr)
hist(clean_F$DftRd)

###### separte Nat into CA/US or not CA/US
clean_F$CA_US = ifelse(((clean_F$Nat == 'CAN')|(clean_F$Nat == 'USA')),1,0)
clean_F = clean_F[,-2]

#########################################################################################################
#                                         Preliminary Analysis
#########################################################################################################

lm0=lm(cap_hit_c~., data = clean_F)
summary(lm0) # HIGH r square but might largely due to multicollinearity
vif(lm0) # severe multicollineary

#########################################################################################################
#                                           Multicollinearity
#########################################################################################################
# Check correlation among variables with simliar name
class(clean_F)
cor = as.data.frame(cor(clean_F[, -which(names(clean_F) == 'cap_hit_c')]))

#------------------------------------------------lasso---------------------------------------------------

#we will perform lasso regression to reduce the multicollinearity as well as the number of predictors
#convert to matrix
clean_F.m = as.matrix(clean_F)
#fit lasso
fit_lasso = glmnet(clean_F.m[, -which(names(clean_F) == 'cap_hit_c')], clean_F.m[, 79], alpha = 1)
#plot trace
plot(fit_lasso, xvar = "lambda", label = TRUE)

#fit lasso with conservative lambda estimate to see which variables it keeps
fit_lasso = glmnet(clean_F.m[, -which(names(clean_F) == 'cap_hit_c')], clean_F.m[, 79], alpha = 1, lambda = exp(-3))

#get variables selected by lasso so it can actually run
good_vars = names(clean_F)[which(coef(fit_lasso) != 0)]

#cross validate
lasso_cvm = cv.glmnet(clean_F.m[, which(names(clean_F) %in% good_vars)], clean_F.m[, 79], alpha = 1)
min_lambda = lasso_cvm$lambda.min

#final lasso model
lasso_cvm = glmnet(clean_F.m[, which(names(clean_F) %in% good_vars)], clean_F.m[, 3], alpha = 1, lambda = min_lambda)

lasso_cvm$dev.ratio
#0.9999663


#########################################################################################################
#                                           Residual Analysis 
#########################################################################################################

#having used lasso for variable selection, we will focus on those variables
#because our small sample size doesn't lend itself to large numbers of predictors

clean_F.2 = clean_F[, which(names(clean_F) %in% good_vars)]
clean_F.2$cap_hit_c = clean_F$cap_hit_c

fit = lm(cap_hit_c ~ ., data = clean_F.2)
summary(fit)

#find the residuals
ei = resid(fit)

#find the studentized residuals
ri = rstandard(fit)

#find the R-student residuals
ti = rstudent(fit)

#PRESS statistic
pr = fit$residuals/(1 - lm.influence(fit)$hat)
press = sum(pr^2) #159.413

#Q-Q Plot
qqnorm(fit$residuals)
qqline(fit$residuals)
#the Q-Q plot looks very normal other than some points at the tails - might be one outlier

#residuals vs fitted values
plot(fit$fitted.values,fit$residuals,xlab='Predicted Values',ylab='Residuals',abline(h=0,lty=2),main = 'Residual plot')
# A result of having a large number of players at the league minimum salary ($650,000)

#residuals vs predictors
good_vars
plot(fit$residuals~., data = clean_F.2)

# ev_IPP% fanning out
# ev_Pct% looks tunnel shaped
# ev_iBLK, pk_G, pk_PTS, pk_iTKA fanning -in 
# pk_A/60 - outlier
#########################################################################################################
#                                               Transform
#########################################################################################################

#-------------------------------------------------fanning out --------------------------------------------------
df_trans = clean_F

#transform ev_IPP% because of its outward fan
df_trans$`ev_IPP%` = clean_F$`ev_IPP%`^2

fit_trans = lm(cap_hit_c ~ ., data = df_trans)
summary(fit_trans)

plot(df_trans$`ev_IPP%`, fit_trans$residuals)
plot(clean_F$`ev_IPP%`, fit$residuals)
# The residual plot looks much better now
# We will use this transformation

clean_F$`ev_IPP%` = (clean_F$`ev_IPP%`^2)
fit = lm(cap_hit_c ~ ., data = clean_F[, good_vars])
summary(fit)

#-------------------------------------------------fanning in --------------------------------------------------
#transform ev_iBLK because of its inward fan
df_trans$ev_iBLK = sqrt(clean_F$ev_iBLK)

fit_trans = lm(cap_hit_c ~ ., data = df_trans)
summary(fit_trans)

#compare residuals
plot(df_trans$ev_iBLK, fit_trans$residuals)
plot(clean_F$ev_iBLK, fit$residuals)

#the new residual plot doesn't that look much better and the adjusted r-squared has not changed
#so we will discard this transformation

###

#transform pk_G because of its inward fan
df_trans$pk_G = sqrt(clean_F$pk_G)

fit_trans = lm(cap_hit_c ~ ., data = df_trans)
summary(fit_trans)

#compare residuals
plot(df_trans$pk_G, fit_trans$residuals)
plot(clean_F$pk_G, fit$residuals)

#the new residual plot doesn't that look much better and the adjusted r-squared decreased
#so we will discard this transformation

##
#transform pk_PTS because of its inward fan
df_trans$pk_PTS = sqrt(clean_F$pk_PTS)

fit_trans = lm(cap_hit_c ~ ., data = df_trans)
summary(fit_trans)

#compare residuals
plot(df_trans$pk_PTS, fit_trans$residuals)
plot(clean_F$pk_PTS, fit$residuals)

#the new residual plot look much better and the adjusted r-squared increased
#so we will use this transformation

clean_F$pk_PTS = sqrt(clean_F$pk_PTS)
fit = lm(cap_hit_c ~ ., data = clean_F[, good_vars])
summary(fit)

##
#transform pk_iTKA because of its inward fan
df_trans$pk_iTKA = sqrt(clean_F$pk_iTKA)

fit_trans = lm(cap_hit_c ~ ., data = df_trans)
summary(fit_trans)

#compare residuals
plot(df_trans$pk_iTKA, fit_trans$residuals)
plot(clean_F$pk_iTKA, fit$residuals)

#the new residual plot look much better and the adjusted r-squared increased
#so we will use this transformation

clean_F$pk_iTKA = sqrt(clean_F$pk_iTKA)
fit = lm(cap_hit_c ~ ., data = clean_F[, good_vars])
summary(fit)

#########################################################################################################
#                                               Influence
#########################################################################################################

df = as.data.frame(summary(influence.measures(lm0)))
# obs 10 and 65 might be an outlier

fit_infl1 = lm(cap_hit_c ~ ., data = clean_F[-10, good_vars])
summary(fit_infl1)

#test models without the points
cv.lm(data = clean_F[good_vars], form.lm = fit, m = 5, plotit = F)
cv.lm(data = clean_F[good_vars], form.lm = fit_infl1, m = 5, plotit = F)

#while the cross-validated MSE stayed the same after removing point 10, the r-squared 
#went down, so there isn't enough support to justify removing it

fit_infl1 = lm(cap_hit_c ~ ., data = clean_F[-65, good_vars])
summary(fit_infl1)

#test models without the points
cv.lm(data = clean_F[good_vars], form.lm = fit, m = 5, plotit = F)
cv.lm(data = clean_F[good_vars], form.lm = fit_infl1, m = 5, plotit = F)

#while the cross-validated MSE stayed the same after removing point 10, the r-squared 
#went down, so there isn't enough support to justify removing it

#########################################################################################################
#                                           Variable Selection
#########################################################################################################

#--------------------------------------------all regressors----------------------------------------------
bestmod <- regsubsets(cap_hit_c ~ ., data = clean_F[, good_vars], nbest = 10)
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
best_vars2 = c(best_vars2, 'cap_hit_c')

best_vars3 = names(best.sum)[which(best.sum[2, ] == '*')]
best_vars3 = c(best_vars3, 'cap_hit_c')

#create models
best2 = lm(cap_hit_c ~ ., data = clean_F[which(names(clean_F)%in%best_vars2)])
summary(best2)

best3 = lm(cap_hit_c ~ ., data = clean_F[which(names(clean_F)%in%best_vars3)])
summary(best3)

## get variables of 'best' models by r squared
best.sum = best.sum[order(-best.sum$adjr2),] # max(adj r-squre) = 0.831
best.sum[1,]
# AGE, `+/-`, `3rd` , ly_salary, ev_iDS, ev_iTKA, pp_A1, pk_PTS

lm2 = lm(cap_hit_c~ AGE+`+/-`+`3rd`+ly_salary+ev_iDS+ev_iTKA+pp_A1+pk_PTS, data = clean_F[, good_vars])
summary(lm2)
vif(lm2)
kappa(lm2) #14236459 indicating extreme multicollinearity
#-------------------------------------------------step---------------------------------------------------
fit_null = lm(cap_hit_c ~ 1, data = clean_F[, good_vars])
fit_full = lm(cap_hit_c ~ ., data = clean_F[, good_vars])

lm3 = step(fit_null, scope = list(lower = fit_null, upper = fit_full), direction = "both")
summary(lm3)
best1_vars = labels(terms(lm3))
best1_vars
# Similar to all possible
# Both models don't differ that much

# Check
vif(stepwise)
kappa(stepwise) # 16482182 indicating extreme multicollineary!

PRESS <- function(linear.model) {
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  PRESS <- sum(pr^2)
  return(PRESS)
}
PRESS(lm2) #119
PRESS(lm3) #119
# These two models are mostly same

pred = predict(lm2,data=clean_F[,-79])
pred
plot(lm2$fitted.values,clean_F$cap_hit_c)
abline(0,1,col="red")

pred = predict(lm3,data=clean_F[,-79])
pred
plot(lm2$fitted.values,clean_F$cap_hit_c)
abline(0,1,col="red")

# Linear model fit the data well. However, there might be serious multicollinearity issues

#########################################################################################################
#                                           Multicollinearity - Use lasso
#########################################################################################################
# Use lasso regression
# Put data into a matrix for ridge regression
clean_F.m <- as.matrix(clean_F[, good_vars])
f.lasso <- glmnet(clean_F.m[,c(-27)], clean_F.m[,27], alpha=1)
plot(f.lasso,xvar="lambda",label=TRUE)

# Coefficients seem to have stabilized when log lambda ~ -4.
f.lasso2 <- glmnet(clean_F.m[,c(-27)], clean_F.m[,27], alpha=0, lambda=exp(-4))
f.lasso2$dev.ratio
# R-squared is 0.876

# Generate residuals plot
f.fitted <- predict(f.lasso2, clean_F.m[,c(-27)])
f.resid <- clean_F.m[,27] - f.fitted

plot(f.fitted, f.resid)
# weird pattern

SSR.ridge <- sum(f.resid^2)
# 72.2

# Transform y -> y^(1/2)
f.lasso.2 <- glmnet(clean_F.m[,c(-27)], (clean_F.m[,27]^(1/2)), alpha=1)
plot(f.lasso.2,xvar="lambda",label=TRUE)

# Coefficients seem to have stabilized when log lambda ~ -4.
f.lasso2.2 <- glmnet(clean_F.m[,c(-27)], (clean_F.m[,27]^(1/2)), alpha=0, lambda=exp(-4))
f.lasso2.2$dev.ratio
# R-squared is 0.892.

# Generate residuals plot
f.fitted <- predict(f.lasso2.2, clean_F.m[,c(-27)])
f.resid <- (clean_F.m[,27]^(1/2)) - f.fitted

plot(f.fitted, f.resid)
# Better but there are still some pattern.

# Transform y -> log(y)
f.lasso.3 <- glmnet(clean_F.m[,c(-27)], log(clean_F.m[,27]), alpha=1)
plot(f.lasso.3,xvar="lambda",label=TRUE)

# Coefficients seem to have stabilized when log lambda ~ -4.
f.lasso3.2 <- glmnet(clean_F.m[,c(-27)], log(clean_F.m[,27]), alpha=0, lambda=exp(-4))
f.lasso3.2$dev.ratio
# R-squared is 0.885.

# Generate residuals plot
f.fitted <- predict(f.lasso3.2, clean_F.m[,c(-27)])
f.resid <- log(clean_F.m[,27]) - f.fitted

qqnorm(f.resid) # might have outlier
qqline(f.resid)

plot(f.fitted,f.resid,xlab='Predicted Values',ylab='Residuals',abline(h=0,lty=2),main = 'Residual plot')
# Looks good - might have some outliers

SSR.ridge <- sum(f.resid^2)
# 8.24

#--------------------------------------------cross validation----------------------------------------------
cv.lm(data = clean_F, form.lm = lm2, m = 5, plotit = F)
#MSE = 1.1 
cv.lm(data = clean_F, form.lm = lm3, m = 5, plotit = F)
#MSE = 1.14

#lm2 is slightly better


#-----------------------------------------manual cross validation----------------------------------------------

#manually cross validate for lasso regression
set.seed(1)
sub = sample(1:nrow(clean_F), size = 0.8*nrow(clean_F))
train = clean_F[sub, ]
valid = clean_F[-sub, ]

train_mat = as.matrix(train)
test_mat = as.matrix(valid)

#create model
lasso_cvm_cv = glmnet(train_mat[, which(names(clean_F) %in% good_vars)], log(train_mat[, 79]), alpha = 1, lambda = exp(-4))
lasso_preds = predict(lasso_cvm_cv, test_mat[, which(names(clean_F) %in% good_vars)])

#MSE
mean((exp(lasso_preds) - valid$cap_hit_c)^2)
#43.9
# Seems that lasso regression is much better


#######
# Histogram
bins = seq(0,17000000, 100000) # above 650,000
hist((exp(f.fitted)*10^6), breaks = bins) # Looks pretty good

plot((exp(f.fitted)),clean_F$cap_hit_c)
abline(0,1,col="red")

