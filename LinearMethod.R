#Tyler Lewris
#tal3fj
#Final Project
#Linear Method

setwd("~/Documents/UVA/UVA DS /SYS 6018 Data Mining/NHL_Salary")

# Grading Criteria
# Data exploration
# Data cleaning (missing values, outliers, etc.)
# Rationale for the selected statistical modeling methods
# Correct implementation and use of statistical modeling methods
# Appropriate model selection approach (train/test, cross-validation, etc.)
# Thoroughly documented code (R comments)
# Appropriate use of functions to reduce code complexity and redundancy
# Writing quality for final report, evaluated in terms of conformance to process outline, 
# level of detail, and correctness.

#NOTE: Data cleaning and exploration have been thoroughly done in a separate R file.
#The data in the csv's below have already been cleaned and explored.


library(car)
library(MASS)

#-------------------------------------------READ IN DATA------------------------------------------------

#------------------Full Data Set
#Defensive Players
full_D <- read.csv('full_D.csv', header=T) #reading in the data
#Offensive Players
full_F <- read.csv('full_F.csv', header=T)

#Check data has been read in properly
head(full_D)
head(full_F)

sum(is.na(full_D)) #no NA's
sum(is.na(full_F)) #no NA's

#Drop X and Name
full_D <- full_D[, -c(1:2)] 
full_F <- full_F[, -c(1:2)] 

#------------------Performance Only Data Set
#Defensive Players
perfonly_D <- read.csv('perfonly_D.csv', header=T) #reading in the data
#Offensive Players
perfonly_F <- read.csv('perfonly_F.csv', header=T)

#Check data has been read in properly
head(perfonly_D)
head(perfonly_F)

sum(is.na(perfonly_D)) #no NA's
sum(is.na(perfonly_F)) #no NA's

#Drop X, Name, and Age (We want performance metrics only)
perfonly_D <- perfonly_D[, -c(1:3)] 
perfonly_F <- perfonly_F[, -c(1:3)] 

#-------------------------------------------DATA EXPLORATION------------------------------------------------

#------------------Full Data Set
#Need to idetify which are categorical variables and which are numeric
lapply(full_D, class)
lapply(full_F, class)

#Nat converted to factor
full_D$Nat <- as.factor(full_D$Nat)
#Rest convert to numeric
full_D[2:88] <- lapply(full_D[2:88], as.numeric)

#Nat converted to factor
full_F$Nat <- as.factor(full_F$Nat)
#Rest convert to numeric
full_F[2:88] <- lapply(full_F[2:88], as.numeric)

#Putting response variable in first column
col_idx <- grep("cap_hit", names(full_D))
full_D <- full_D[, c(col_idx, (1:ncol(full_D))[-col_idx])]
names(full_D)

#Putting response variable in first column
col_idx <- grep("cap_hit", names(full_F))
full_F <- full_F[, c(col_idx, (1:ncol(full_F))[-col_idx])]
names(full_F)

#------------------Performance Only Data Set
#Need to idetify which are categorical variables and which are numeric
lapply(perfonly_D, class)
lapply(perfonly_F, class)

#Convert to numeric
perfonly_D[1:79] <- lapply(perfonly_D[1:79], as.numeric)

#Convert to numeric
perfonly_F[1:79] <- lapply(perfonly_F[1:79], as.numeric)

#Putting response variable in first column
col_idx <- grep("cap_hit", names(perfonly_D))
perfonly_D <- perfonly_D[, c(col_idx, (1:ncol(perfonly_D))[-col_idx])]
names(perfonly_D)

#Putting response variable in first column
col_idx <- grep("cap_hit", names(perfonly_F))
perfonly_F <- perfonly_F[, c(col_idx, (1:ncol(perfonly_F))[-col_idx])]
names(perfonly_F)


#-------------------------------------------LINEAR MODELING------------------------------------------------

#-------------------------------------------ITERATIVE MODEL SELECTION------------------------------------------------

#-------------------------------------------this section is bullshit

library(glmnet)
## Put data into a matrix for ridge regression
sdata.m <- as.matrix(full_D)

## Lasso regression
###################

## Lasso regression uses the same function as ridge regression with alpha=1
s.lasso <- glmnet(sdata.m[,2:88], sdata.m[,1], alpha=0)
s.lasso$lambda[20]
coef(s.lasso)[,20]
#-------------------------------------------this section is bullshit

#----------------------------Full Defense Data Set
## Iterative model selection
## Begin by defining the models with no variables (null) and all variables (full)
model.null <- lm(cap_hit~1, data=full_D)
model.full <- lm(cap_hit~., data=full_D)

## Forward selection
step(model.null, scope=list(lower=model.null, upper=model.full), direction="forward")
# Step:  AIC=2344.64
# cap_hit ~ xGF + pp_G + ev_iTKA + Ovrl + ly_salary + AGE + pp_Prev3PPG + 
#   ev_Prev3G + ev_CF + ev_G + ev_ixG + ly_cap_hit + DftRd + 
#   ev_iHA + pp_Pct. + OTG + ev_FF + Nat + ev_Prev3A + ev_Prev3Corsi + 
#   pk_iBLK + ev_CA + Wt + ev_SCA + pp_A.60 + pp_Prev3PPA + pp_Prev3TOI + 
#   ev_A.60 + ev_RelPct. + pp_TOI.GP + pk_Prev3TOI + ev_F.60 + 
#   GP + ev_SH. + pk_A.60 + ev_SCF

## Backward selection
step(model.full, scope=list(lower=model.null, upper=model.full), direction="backward")
# Step:  AIC=2087.36
# cap_hit ~ Nat + AGE + Ht + Wt + DftRd + Ovrl + GP + plusminus + 
#   Eplusminus + PIM + OTG + GWG + NPD + xGF + xGA + ly_salary + 
#   ly_cap_hit + ev_G + ev_A + ev_A1 + ev_IPP. + ev_SH. + ev_PDO + 
#   ev_F.60 + ev_A.60 + ev_Pct. + ev_RelPct. + ev_iSCF + ev_iCF + 
#   ev_iFF + ev_iSF + ev_iRB + ev_iRS + ev_Pass + ev_iHF + ev_iHA + 
#   ev_iGVA + ev_iTKA + ev_iBLK + ev_iFOW + ev_FO. + ev_CF + 
#   ev_CA + ev_FF + ev_FA + ev_SCA + pp_G + pp_A1 + pp_TOI + 
#   pp_TOI.GP + pp_F.60 + pp_A.60 + pp_Pct. + pp_RelPct. + pk_G + 
#   pk_A + pk_TOI + pk_TOIGP + pk_F.60 + pk_A.60 + pk_Pct. + 
#   pk_RelPct. + pk_iTKA + pk_iBLK + Prev3GP + ev_Prev3G + ev_Prev3A + 
#   ev_Prev3TOI + ev_Prev3Corsi + pp_Prev3PPG + pp_Prev3PPA + 
#   pp_Prev3TOI + pk_Prev3Blk + pk_Prev3Hits + pk_Prev3TOI + 
#   stars

## Stepwise selection
step(model.null, scope=list(lower=model.null, upper=model.full), direction="both")
# Step:  AIC=2339.99
# cap_hit ~ xGF + ev_iTKA + Ovrl + ly_salary + AGE + pp_Prev3PPG + 
#   ev_Prev3G + ev_CF + ev_G + ev_ixG + ly_cap_hit + DftRd + 
#   ev_iHA + OTG + ev_FF + Nat + ev_Prev3A + ev_Prev3Corsi + 
#   pk_iBLK + ev_CA + Wt + ev_SCA + pp_Prev3PPA + pp_Prev3TOI + 
#   ev_A.60 + ev_RelPct. + ev_A + ev_F.60 + PIM + pk_A.60 + pk_RelPct. + 
#   ev_A1 + pp_RelPct.

#Stepmodel with stepwise selection
step_fulld <- lm(cap_hit ~ xGF + ev_iTKA + Ovrl + ly_salary + AGE + pp_Prev3PPG + 
                           ev_Prev3G + ev_CF + ev_G + ev_ixG + ly_cap_hit + DftRd + 
                           ev_iHA + OTG + ev_FF + Nat + ev_Prev3A + ev_Prev3Corsi + 
                           pk_iBLK + ev_CA + Wt + ev_SCA + pp_Prev3PPA + pp_Prev3TOI + 
                           ev_A.60 + ev_RelPct. + ev_A + ev_F.60 + PIM + pk_A.60 + pk_RelPct. + 
                           ev_A1 + pp_RelPct., data=full_D)

# Take a look at the model statistics and if there is any multicollinearity present that we need to be worried about.
summary(step_fulld)
vif(step_fulld)


#Remove multicollinear variables
step_fulld1 <- lm(cap_hit ~ ev_iTKA + ly_salary + AGE + pp_Prev3PPG + 
                   ev_Prev3G + ev_G + ev_iHA + OTG + Nat + ev_Prev3A + ev_Prev3Corsi + 
                   pk_iBLK + Wt + ev_A.60 + ev_RelPct. + ev_A + ev_F.60 + PIM + pk_A.60 + 
                   pk_RelPct. + ev_A1 + pp_RelPct., data=full_D)
summary(step_fulld1)
vif(step_fulld1)
#no longer multicollinearity in the model

#----------------------------Full Offense Data Set
## Iterative model selection
## Begin by defining the models with no variables (null) and all variables (full)
model.null <- lm(cap_hit~1, data=full_F)
model.full <- lm(cap_hit~., data=full_F)

## Forward selection
step(model.null, scope=list(lower=model.null, upper=model.full), direction="forward")
# Step:  AIC=5039.34
# cap_hit ~ stars + pp_A + ev_iFOW + AGE + ly_salary + plusminus + 
#   ly_cap_hit + pp_G + pp_TOI.GP + ev_iTKA + GP + ev_iFF + Grit + 
#   Nat + ev_Prev3A + ev_G + pk_A + pk_iBLK + pp_Prev3PPA + pp_Prev3TOI + 
#   ev_Pct. + pk_F.60 + Eplusminus + pp_A1 + pp_TOI + ev_CA + 
#   ev_FF + pk_Prev3TOI + Ht + ev_iCF + xGA

## Backward selection
step(model.full, scope=list(lower=model.null, upper=model.full), direction="backward")
# Step:  AIC=5029.68
# cap_hit ~ ly_cap_hit + Nat + AGE + GP + Eplusminus + xGF + ly_salary + 
#   ev_A1 + ev_PDO + ev_F.60 + ev_A.60 + ev_RelPct. + ev_ixG + 
#   ev_iCF + ev_Pass + ev_iHF + ev_iTKA + ev_iFOW + ev_CF + ev_CA + 
#   ev_SCA + pp_G + pp_A + pp_A1 + pp_A.60 + pk_F.60 + Prev3GP + 
#   ev_Prev3G + ev_Prev3A + ev_Prev3TOI + pp_Prev3PPA + pp_Prev3TOI + 
#   pk_Prev3TOI + stars

## Stepwise selection
step(model.null, scope=list(lower=model.null, upper=model.full), direction="both")
# Step:  AIC=5034.65
# cap_hit ~ stars + pp_A + ev_iFOW + AGE + ly_salary + ly_cap_hit + 
#   pp_G + ev_iTKA + GP + ev_iFF + Grit + Nat + ev_Prev3A + ev_G + 
#   pk_A + pp_Prev3PPA + pp_Prev3TOI + pk_F.60 + Eplusminus + 
#   pp_A1 + pp_TOI + ev_CA + ev_FF + pk_Prev3TOI + Ht

#Stepmodel with stepwise selection
step_fullf <- lm(cap_hit ~ stars + pp_A + ev_iFOW + AGE + ly_salary + ly_cap_hit + 
                   pp_G + ev_iTKA + GP + ev_iFF + Grit + Nat + ev_Prev3A + ev_G + 
                   pk_A + pp_Prev3PPA + pp_Prev3TOI + pk_F.60 + Eplusminus + 
                   pp_A1 + pp_TOI + ev_CA + ev_FF + pk_Prev3TOI + Ht, data=full_F)

# Take a look at the model statistics and if there is any multicollinearity present that we need to be worried about.
summary(step_fullf)
vif(step_fullf)

#Remove multicollinear variables
step_fullf1 <- lm(cap_hit ~ stars + ev_iFOW + AGE + ly_cap_hit + 
                   pp_G + ev_iTKA + GP + Grit + Nat + ev_Prev3A + ev_G + 
                   pk_A + pk_F.60 + Eplusminus + 
                   pp_A1 + pk_Prev3TOI + Ht, data=full_F)
summary(step_fullf1)
vif(step_fullf1)
#no longer multicollinearity in the model

#BEST DEFENSE MODEL = step_fulld1
#BEST OFFENSE MODEL = step_fullf1

#----------------------------Performance Only Defense Data Set
## Iterative model selection
## Begin by defining the models with no variables (null) and all variables (full)
model.null <- lm(cap_hit~1, data=perfonly_D)
model.full <- lm(cap_hit~., data=perfonly_D)

## Forward selection
step(model.null, scope=list(lower=model.null, upper=model.full), direction="forward")
# Step:  AIC=2437.85
# cap_hit ~ xGF + pp_G + ev_iTKA + pk_Prev3TOI + pp_Prev3PPG + 
#   NPD + ev_iSCF + PIM + ev_iDS + ev_G + ev_Prev3Corsi + ev_ixG + 
#   pp_Prev3PPA + pp_A1 + ev_iCF + Grit + OTG + ev_F.60 + ev_IPP.

## Backward selection
step(model.full, scope=list(lower=model.null, upper=model.full), direction="backward")
# Step:  AIC=2403.72
# cap_hit ~ GP + plusminus + Eplusminus + PIM + OTG + NPD + xGF + 
#   Grit + ev_G + ev_A + ev_A1 + ev_F.60 + ev_A.60 + ev_Pct. + 
#   ev_iSCF + ev_iCF + ev_iFF + ev_iSF + ev_iRS + ev_Pass + ev_iHF + 
#   ev_iHA + ev_iGVA + ev_iTKA + ev_iBLK + ev_CF + ev_CA + ev_FF + 
#   ev_FA + ev_SCF + ev_SCA + pp_A + pp_A1 + pp_TOI + pp_A.60 + 
#   pp_RelPct. + pk_G + pk_A + pk_TOI + pk_TOIGP + pk_F.60 + 
#   pk_A.60 + pk_Pct. + pk_RelPct. + pk_iBLK + ev_Prev3Corsi + 
#   pp_Prev3PPG + pp_Prev3PPA + pp_Prev3TOI + pk_Prev3Blk + pk_Prev3Hits + 
#   pk_Prev3TOI

## Stepwise selection
step(model.null, scope=list(lower=model.null, upper=model.full), direction="both")
# Step:  AIC=2433.94
# cap_hit ~ pp_G + ev_iTKA + pk_Prev3TOI + pp_Prev3PPG + NPD + 
#   ev_iDS + ev_G + ev_Prev3Corsi + ev_ixG + pp_Prev3PPA + pp_A1 + 
#   ev_iCF + Grit + OTG + ev_SCA

#Stepmodel with stepwise selection
step_perfonlyd <- lm(cap_hit ~ pp_G + ev_iTKA + pk_Prev3TOI + pp_Prev3PPG + NPD + 
                   ev_iDS + ev_G + ev_Prev3Corsi + ev_ixG + pp_Prev3PPA + pp_A1 + 
                   ev_iCF + Grit + OTG + ev_SCA, data=perfonly_D)

# Take a look at the model statistics and if there is any multicollinearity present that we need to be worried about.
summary(step_perfonlyd)
vif(step_perfonlyd)

#Remove multicollinear variables
step_perfonlyd1 <- lm(cap_hit ~ pp_G + ev_iTKA + pk_Prev3TOI + pp_Prev3PPG + NPD + 
                       ev_iDS + ev_G + ev_Prev3Corsi + pp_Prev3PPA + pp_A1 + 
                       Grit + OTG + ev_SCA, data=perfonly_D)
summary(step_perfonlyd1)
vif(step_perfonlyd1)
#no longer multicollinearity in the model

#----------------------------Performance Only Offense Data Set
## Iterative model selection
## Begin by defining the models with no variables (null) and all variables (full)
model.null <- lm(cap_hit~1, data=perfonly_F)
model.full <- lm(cap_hit~., data=perfonly_F)

## Forward selection
step(model.null, scope=list(lower=model.null, upper=model.full), direction="forward")
# Step:  AIC=5081.57
# cap_hit ~ stars + pp_A + ev_iFOW + Eplusminus + pk_RelPct. + 
#   pp_G + ev_iHF + GP + ev_PTS + ev_iTKA + pp_A1 + pk_Prev3TOI + 
#   ev_CA + xGF + ev_iSCF + pp_Prev3PPA + ev_iRB + ev_iCF + ev_FA + 
#   pk_A + pk_TOI + ev_iSF

## Backward selection
step(model.full, scope=list(lower=model.null, upper=model.full), direction="backward")
# Step:  AIC=5067.67
# cap_hit ~ GP + Eplusminus + OTG + xGF + ev_A1 + ev_ixG + ev_iSCF + 
#   ev_iFF + ev_iSF + ev_Pass + ev_iHF + ev_iTKA + ev_iFOW + 
#   ev_CA + ev_SCA + pp_G + pp_A + pp_A1 + pp_TOI.GP + pp_A.60 + 
#   pk_RelPct. + Prev3GP + ev_Prev3G + ev_Prev3A + ev_Prev3TOI + 
#   pp_Prev3PPA + pk_Prev3TOI + stars

## Stepwise selection
step(model.null, scope=list(lower=model.null, upper=model.full), direction="both")
# Step:  AIC=5080.62
# cap_hit ~ stars + pp_A + ev_iFOW + Eplusminus + pk_RelPct. + 
#   pp_G + ev_iHF + GP + ev_PTS + ev_iTKA + pp_A1 + ev_CA + xGF + 
#   ev_iSCF + pp_Prev3PPA + ev_iRB + ev_iCF + ev_FA + pk_A + 
#   pk_TOI + ev_iSF

#Stepmodel with stepwise selection
step_perfonlyf <- lm(cap_hit ~ stars + pp_A + ev_iFOW + Eplusminus + pk_RelPct. + 
                       pp_G + ev_iHF + GP + ev_PTS + ev_iTKA + pp_A1 + ev_CA + xGF + 
                       ev_iSCF + pp_Prev3PPA + ev_iRB + ev_iCF + ev_FA + pk_A + 
                       pk_TOI + ev_iSF, data=perfonly_F)

# Take a look at the model statistics and if there is any multicollinearity present that we need to be worried about.
summary(step_perfonlyf)
vif(step_perfonlyf)

#Remove multicollinear variables
step_perfonlyf1 <- lm(cap_hit ~ stars + pp_A + ev_iFOW + Eplusminus + pk_RelPct. + 
                       pp_G + ev_iHF + ev_iTKA + pp_A1 + 
                       ev_iSCF + pp_Prev3PPA + ev_iRB + pk_A + 
                       pk_TOI, data=perfonly_F)
summary(step_perfonlyf1)
vif(step_perfonlyf1)
#no longer multicollinearity in the model

#BEST DEFENSE MODEL = step_perfonlyd1
#BEST OFFENSE MODEL = step_perfonlyf1

#Now lets test our hypothesis and see which model performs better

summary(step_fulld1)
# Adjusted R^2 = 0.8104
# p-value: < 2.2e-16

summary(step_fullf1)
# Adjusted R-squared:  0.9016
# p-value: < 2.2e-16

summary(step_perfonlyd1)
# Adjusted R-squared:  0.7832 
# p-value: < 2.2e-16

summary(step_perfonlyf1)
# Adjusted R-squared:  0.8595
# p-value: < 2.2e-16

anova(step_fulld1, step_perfonlyd1)
anova(step_fullf1, step_perfonlyf1)

#RESIDUAL ANALYSIS
#Plot residuals vs. fitted
ti <- rstudent(step_fulld1)
yhat <- fitted(step_fulld1)
plot(yhat, ti) #small pattern but still distributed around 0
abline(0,0)
summary(influence.measures(step_fulld1)) #no significant influential measures
qqnorm(ti) #looks good
qqline(ti)

#Plot residuals vs. fitted
ti <- rstudent(step_fullf1)
yhat <- fitted(step_fullf1)
plot(yhat, ti) #small pattern but still distributed around 0
abline(0,0)
summary(influence.measures(step_fullf1)) #no significant influential measures
qqnorm(ti) #significant fanning towards the tails
qqline(ti)

#Plot residuals vs. fitted
ti <- rstudent(step_perfonlyd1)
yhat <- fitted(step_perfonlyd1)
plot(yhat, ti) #small pattern but still distributed around 0
abline(0,0)
summary(influence.measures(step_perfonlyd1)) #no significant influential measures
qqnorm(ti) #significant fanning towards the tails
qqline(ti)

#Plot residuals vs. fitted
ti <- rstudent(step_perfonlyf1)
yhat <- fitted(step_perfonlyf1)
plot(yhat, ti)
abline(0,0) #small pattern but still distributed around 0
summary(influence.measures(step_perfonlyf1)) #no significant influential measures
qqnorm(ti) #significant fanning towards the tails
qqline(ti)


#---------------------------------------------Comparative Model Selection--------------------------------------------
#The data frame full_Dnm I deleted because it didnt make sense
#We can easily remove this seciton if we dont want to use it


#Using comparative model selection on the dataset with no multicollinearity
bestmod <- regsubsets(cap_hit~., data=full_Dnm, nbest=10)

## The 10 best models for each number of explanatory variables in the model
summary(bestmod)
best.sum <- as.data.frame(summary(bestmod)$outmat)
best.sum$p <- as.numeric(substr(rownames(best.sum),1,1))+1

## The criterion values corresponding to each model
best.sum$rss <- summary(bestmod)$rss
best.sum$adjr2 <- summary(bestmod)$adjr2
best.sum$cp <- summary(bestmod)$cp
best.sum$bic <- summary(bestmod)$bic

## Determine "best" models
best.sum[order(best.sum$rss),]
best.sum[order(best.sum$adjr2,decreasing=T),]
best.sum[order(best.sum$cp),]
best.sum[order(best.sum$bic),]

#---------------------------------------------K-Fold Cross Validation--------------------------------------------
#I am selecting K-fold Cross-Validation for its ease of use and will be 
#using 5 as my k-fold value. To derive this value, I performed three different K-fold CV's using k = 3,
#k=5, and k=10. For the purpose of this assignment, K=5 made the most sense both computationally and
#logically. 

# K-fold cross-validation
library(boot) #necessary library for logistic regression models

#K = 5 in K-fold cross-validation 
cv.lm(data = full_D, step_fulld1, m=5)

#K = 5 in K-fold cross-validation 
cv.lm(data = full_F, step_fullf1, m=5)

#K = 5 in K-fold cross-validation 
cv.lm(data = perfonly_D, step_perfonlyd1, m=5)

#K = 5 in K-fold cross-validation 
cv.lm(data = perfonly_F, step_perfonlyf1, m=5)

