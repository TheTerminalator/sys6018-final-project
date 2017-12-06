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
clean_F$cap_hit_c = clean_F$cap_hit
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

#####################################################
plot(clean_F$cap_hit_c~.,data = clean_F)
#xGF,xGA,ev_G,ev_PTS,ev_IPP%,ev_SH%,ev_iCF,ev_iFF,ev_iSF,ev_CF,ev_CA,ev_FF,ev_FA,ev_SCA,pp_TOI/GP,pp_F/60
clean_F$XGF_2 = poly(clean_F$xGF,2)
clean_F$XGA_2 = poly(clean_F$XGA,2)
clean_F$ev_G_2 = poly(clean_F$ev_G,2)
clean_F$ev_PTS_2 = poly(clean_F$ev_PTS,2)
clean_F$ev_IPP_2 = poly(clean_F$`ev_IPP%`,2)
clean_F$ev_SH_2 = poly(clean_F$`ev_SH%`,2)
clean_F$ev_iCF_2 = poly(clean_F$ev_iCF,2)
clean_F$ev_iFF_2 = poly(clean_F$ev_iFF,2)
clean_F$ev_iSF_2 = poly(clean_F$ev_iSF,2)
clean_F$ev_CF_2 = poly(clean_F$ev_CF,2)
clean_F$ev_CA_2 = poly(clean_F$ev_CA,2)
clean_F$ev_FF_2 = poly(clean_F$ev_FF,2)
clean_F$ev_FA_2 = poly(clean_F$ev_FA,2)
clean_F$ev_SCA_2 = poly(clean_F$ev_SCA,2)
clean_F$pp_TOI_GP_2 = poly(clean_F$`pp_TOI/GP`,2)
clean_F$pp_F_2 = poly(clean_F$`pp_F/60`,2)

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
fit_lasso = glmnet(clean_F.m[, -which(names(clean_F) == 'cap_hit_c')], clean_F.m[, 79], alpha = 1, lambda = exp(10))

#get variables selected by lasso so it can actually run
good_vars = names(clean_F)[which(coef(fit_lasso) != 0)]
good_vars=good_vars[1:38]

#cross validate
lasso_cvm = cv.glmnet(clean_F.m[, which(names(clean_F) %in% good_vars)], clean_F.m[, 79], alpha = 1)
min_lambda = lasso_cvm$lambda.min

#final lasso model
lasso_cvm = glmnet(clean_F.m[, which(names(clean_F) %in% good_vars)], clean_F.m[, 3], alpha = 1, lambda = min_lambda)

lasso_cvm$dev.ratio
#1


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
press = sum(pr^2) #3.48e+14

#Q-Q Plot
qqnorm(fit$residuals)
qqline(fit$residuals)
#the Q-Q plot looks normal other than some points at the tails - might be one outlier

#residuals vs fitted values
plot(fit$fitted.values,fit$residuals,xlab='Predicted Values',ylab='Residuals',abline(h=0,lty=2),main = 'Residual plot')
# A result of having a large number of players at the league minimum salary ($650,000)

#residuals vs predictors
good_vars
plot(fit$residuals~., data = clean_F.2)

# ev_SH%, EV_PDO, ev_F.60, pp_Pct% fanning out
# Ovrl, OTG, GRIT, PP_G, pk_G,pk_A,pk_PTS fanning in
#########################################################################################################
#                                               Transform
#########################################################################################################

#-------------------------------------------------fanning out --------------------------------------------------
df_trans = clean_F

#transform ev_SH% because of its outward fan
df_trans$`ev_SH%` = clean_F$`ev_SH%`^2

fit_trans = lm(cap_hit_c ~ ., data = df_trans)
summary(fit_trans)

plot(df_trans$`ev_SH%`, fit_trans$residuals)
plot(clean_F$`ev_SH%`, fit$residuals)
# The residual plot looks much better now
# We will use this transformation

clean_F$`ev_SH%` = (clean_F$`ev_SH%`^2)
fit = lm(cap_hit_c ~ ., data = clean_F[, good_vars])
summary(fit)

#transform ev_PDO% because of its outward fan
df_trans$`ev_PDO` = clean_F$`ev_PDO`^2

fit_trans = lm(cap_hit_c ~ ., data = df_trans)
summary(fit_trans)

plot(df_trans$`ev_PDO`, fit_trans$residuals)
plot(clean_F$`ev_PDO`, fit$residuals)
# The residual plot does not looks better
# We will discard this transformation

#transform ev_F/60 because of its outward fan
df_trans$`ev_F/60` = clean_F$`ev_F/60`^2

fit_trans = lm(cap_hit_c ~ ., data = df_trans)
summary(fit_trans)

plot(df_trans$`ev_F/60`, fit_trans$residuals)
plot(clean_F$`ev_F/60`, fit$residuals)
# The residual plot does not looks better
# We will use this transformation

clean_F$`ev_F/60` = (clean_F$`ev_F/60`^2)
fit = lm(cap_hit_c ~ ., data = clean_F[, good_vars])
summary(fit)

#transform pp_Pct% because of its outward fan
df_trans$`pp_Pct%` = clean_F$`pp_Pct%`^3

fit_trans = lm(cap_hit_c ~ ., data = df_trans)
summary(fit_trans)

plot(df_trans$`pp_Pct%`, fit_trans$residuals)
plot(clean_F$`pp_Pct%`, fit$residuals)
# The residual plot does looks better
# We will use this transformation

clean_F$`pp_Pct%` = (clean_F$`pp_Pct%`^3)
fit = lm(cap_hit_c ~ ., data = clean_F[, good_vars])
summary(fit)



#-------------------------------------------------fanning in --------------------------------------------------
#transform OTG because of its inward fan
df_trans$OTG = sqrt(clean_F$OTG)

fit_trans = lm(cap_hit_c ~ ., data = df_trans)
summary(fit_trans)

#compare residuals
plot(df_trans$OTG, fit_trans$residuals)
plot(clean_F$OTG, fit$residuals)

#the new residual plot doesn't that look much better and the adjusted r-squared has not changed
#so we will discard this transformation

###

#transform Grit because of its inward fan
df_trans$Grit = sqrt(clean_F$Grit)

fit_trans = lm(cap_hit_c ~ ., data = df_trans)
summary(fit_trans)

#compare residuals
plot(df_trans$Grit, fit_trans$residuals)
plot(clean_F$Grit, fit$residuals)

#the new residual plot looks better
#so we will use this transformation

clean_F$Grit = sqrt(Grit)
fit = lm(cap_hit_c ~ ., data = clean_F[, good_vars])
summary(fit)

##
#transform Ovrl because of its inward fan
df_trans$Ovrl = sqrt(clean_F$Ovrl)

fit_trans = lm(cap_hit_c ~ ., data = df_trans)
summary(fit_trans)

#compare residuals
plot(df_trans$Ovrl, fit_trans$residuals)
plot(clean_F$Ovrl, fit$residuals)

#the new residual plot look much better and the adjusted r-squared increased
#so we will use this transformation

clean_F$Ovrl = sqrt(clean_F$Ovrl)
fit = lm(cap_hit_c ~ ., data = clean_F[, good_vars])
summary(fit)

##
#transform pp_G because of its inward fan
df_trans$pp_G = sqrt(clean_F$pp_G)

fit_trans = lm(cap_hit_c ~ ., data = df_trans)
summary(fit_trans)

#compare residuals
plot(df_trans$pp_G, fit_trans$residuals)
plot(clean_F$pp_G, fit$residuals)

#the new residual plot look much better
#so we will use this transformation

clean_F$pp_G = sqrt(clean_F$pp_G)
fit = lm(cap_hit_c ~ ., data = clean_F[, good_vars])
summary(fit)

##
#transform pk_G because of its inward fan
df_trans$pk_G = sqrt(clean_F$pk_G)

fit_trans = lm(cap_hit_c ~ ., data = df_trans)
summary(fit_trans)

#compare residuals
plot(df_trans$pk_G, fit_trans$residuals)
plot(clean_F$pk_G, fit$residuals)

#the new residual plot doesn't look much better
#so we will discard this transformation

##
#transform pk_G because of its inward fan
df_trans$pk_A = sqrt(clean_F$pk_A)

fit_trans = lm(cap_hit_c ~ ., data = df_trans)
summary(fit_trans)

#compare residuals
plot(df_trans$pk_A, fit_trans$residuals)
plot(clean_F$pk_A, fit$residuals)

#the new residual plot look much better
#so we will use this transformation

clean_F$pk_A = sqrt(clean_F$pk_A)
fit = lm(cap_hit_c ~ ., data = clean_F[, good_vars])
summary(fit)

##
#transform pk_G because of its inward fan
df_trans$pk_PTS = sqrt(clean_F$pk_PTS)

fit_trans = lm(cap_hit_c ~ ., data = df_trans)
summary(fit_trans)

#compare residuals
plot(df_trans$pk_PTS, fit_trans$residuals)
plot(clean_F$pk_PTS, fit$residuals)

#the new residual plot look much better
#so we will use this transformation

clean_F$pk_PTS = sqrt(clean_F$pk_PTS)
fit = lm(cap_hit_c ~ ., data = clean_F[, good_vars])
summary(fit)


#########################################################################################################
#                                               Influence
#########################################################################################################
library(olsrr)
df = as.data.frame(summary(influence.measures(lm0)))

# ols_dfbetas_panel(lm0)
ols_cooksd_chart(lm0)
ols_rsdlev_plot(lm0)
# obs 10, 12,42,63,32,70,28,76,68 are an outliers
# No need to remove

#########################################################################################################
#                                           Variable Selection
#########################################################################################################

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
vif(lm3) # no significant multicollinearity issues

# Remove insignicant variables
lm4 = lm(formula = cap_hit_c ~ ev_PTS_2 + ev_iFOW + pp_TOI + AGE + 
           ly_salary, data = clean_F[, good_vars])

summary(lm4)
vif(lm4) # NO MULTICOLLIEARITY ISSUES

# Residual plot
plot(lm4$fitted.values,lm4$residuals)
qqnorm(lm4$residuals)
qqline(lm4$residuals)
# Looks good excpet some heavy tail

PRESS <- function(linear.model) {
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  PRESS <- sum(pr^2)
  return(PRESS)
}
PRESS(lm4) #1.03e+14
SSR = sum(lm4$residuals^2) #8.64e+13

pred = predict(lm4,data=clean_F[,-79])
pred
plot(lm4$fitted.values,clean_F$cap_hit_c)
abline(0,1,col="red")

#--------------------------------------------all regressors----------------------------------------------
bestmod <- regsubsets(cap_hit_c ~ ev_PTS_2 + ev_iFOW + pp_TOI + AGE + ly_salary + ev_iSF_2 + 
                        ev_iTKA + CA_US + pk_PTS + ev_SCA_2 + `ev_Pct%` + `pk_Pct%` + 
                        `pk_A/60`, data = clean_F[, good_vars], nbest = 10)
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

## get variables of 'best' models by cp
best.sum = best.sum[order(-best.sum$adjr2),] # max(adj r-squre) = 0.831
best.sum[1,]
# AGE, ly_salary, ev_G, GP, ev_A, pp_A1, ev_iFOW, ev_iGA
# It doesn't make sense that ev_iGA has positive coefficient - try delete them

lm5 = lm(cap_hit_c~ev_PTS+ev_iFOW+AGE+ly_salary+ev_iSF_2+ev_iTKA+CA_US, data = clean_F)
summary(lm5)

# Similar to step but slightly higher adj.R square
# Check other criteria
# Residual plot
plot(lm5$fitted.values,lm5$residuals)
qqnorm(lm5$residuals)
qqline(lm5$residuals)
# Not as good

PRESS <- function(linear.model) {
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  PRESS <- sum(pr^2)
  return(PRESS)
}
PRESS(lm5) #1.03e+14 - same
SSR = sum(lm5$residuals^2) #8.64e+13 slightly lower


pred = predict(lm5,data=clean_F[,-79])
pred
plot(lm5$fitted.values,clean_F$cap_hit_c)
abline(0,1,col="red")
# Good

# Transform
boxcox(lm5)
lm6 = lm(log(cap_hit_c)~ev_PTS+ev_iFOW+AGE+ly_salary+ev_iSF_2+ev_iTKA+CA_US, data = clean_F)
summary(lm6)
# Lower R-square

# Overall lm4 seems to be a better model
#########################################################################################################
#                                        Use lasso
#########################################################################################################
# Use lasso regression
# Put data into a matrix for ridge regression
clean_F.m <- as.matrix(clean_F[, good_vars])
f.lasso <- glmnet(clean_F.m[,c(-27)], clean_F.m[,27], alpha=1)
plot(f.lasso,xvar="lambda",label=TRUE)

# Coefficients seem to have stabilized when log lambda ~ -2.
f.lasso2 <- glmnet(clean_F.m[,c(-27)], clean_F.m[,27], alpha=0, lambda=exp(-2))
f.lasso2$dev.ratio
# R-squared is 0.462, not good

# Generate residuals plot
f.fitted <- predict(f.lasso2, clean_F.m[,c(-27)])
f.resid <- clean_F.m[,27] - f.fitted
plot(f.fitted, f.resid)
qqnorm(f.resid)
qqline(f.resid)
SSR.ridge <- sum(f.resid^2)
# 11091 - less than linear models

vars = names(clean_F)[which(coef(f.lasso2) != 0)]
# [1] "AGE"        "Ht"         "Wt"         "DftYr"      "DftRd"      "Ovrl"       "GP"         "+/-"        "E+/-"      
# [10] "PIM"        "OTG"        "GWG"        "NPD"        "xGF"        "xGA"        "Grit"       "1st"        "2nd"       
# [19] "3rd"        "ly_salary"  "ly_cap_hit" "ev_G"       "ev_A"       "ev_A1"      "ev_PTS"     "ev_IPP%"    "ev_SH%"    
# [28] "ev_PDO"     "ev_F/60"    "ev_A/60"    "ev_Pct%"    "ev_RelPct%" "ev_ixG"     "ev_iSCF"    "ev_iCF"     "ev_iFF"    
# [37] "ev_iSF"     "ev_iRB"     "ev_iRS"     "ev_iDS"     "ev_Pass"    "ev_iHF"     "ev_iHA"     "ev_iGVA"    "ev_iTKA"

# Not worth it with so many variables but far less R-square

#--------------------------------------------cross validation----------------------------------------------
clean_F = as.data.frame(clean_F)
cv.lm(data = clean_F, form.lm = lm4, m=5,plotit = F)
#MSE = 1.05e+12

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
# 2.29e+12
# Seems that lm4 is still much better


#######
# Histogram
values = lm4$fitted.values[lm4$fitted.values>0]
hist(values)
