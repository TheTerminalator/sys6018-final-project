
# setwd("~/Desktop/UVA DSI/SYS6018/NHL_Salary")

library(tidyverse)
# library(FNN)
library(rknn)


df <- read_csv('fa_clean_F.csv')
sum(is.na(df))

full_D <- read_csv('full_D.csv')
full_F <- read_csv('full_F.csv')

perf_D <- read_csv('perfonly_D.csv')
perf_F <- read_csv('perfonly_F.csv')

full_D$X1 <- NULL
full_F$X1 <- NULL
perf_D$X1 <- NULL
perf_F$X1 <- NULL

varUsed ###### !!!!!!!!!

## Do k-fold cross validation to get fitted for all players? even manually
## Need to extract vars for best model

######## FULL D ######

sub <- sample(1:nrow(full_D), size = .8*nrow(full_D))
tr.full_D <- full_D[sub,]
te.full_D <- full_D[-sub,]

tr.full_D_hit <- full_D$cap_hit[sub]
te.full_D_hit <- full_D$cap_hit[-sub]
tr.full_D_name <- tr.full_D$name
te.full_D_name <- te.full_D$name

tr.full_D <- tr.full_D[ , !names(tr.full_D) %in% c('cap_hit','name','Nat')]
te.full_D <- te.full_D[ , !names(te.full_D) %in% c('cap_hit','name','Nat')]

## NEED TO NORMALIZE PREDICTORS

norm.tr.full_D <- normalize.unit(tr.full_D)
norm.te.full_D <- normalize.unit(te.full_D)
norm.tr.full_D_hit <- normalize.unit(as.data.frame(tr.full_D_hit))
norm.te.full_D_hit <- normalize.unit(as.data.frame(te.full_D_hit))

norm.tr.full_D_hit <- as.list(norm.tr.full_D_hit)
norm.te.full_D_hit <- as.list(norm.te.full_D_hit)
norm.te.full_D_hit <- unlist(norm.te.full_D_hit)
norm.tr.full_D <- as.data.frame(norm.tr.full_D)
norm.te.full_D <- as.data.frame(norm.te.full_D)

norm.tr.full_D$ev_iFOW <- NULL
norm.te.full_D$ev_iFOW <- NULL

# Random KNN - good for high dimensions and small sample size 
# with rknn
rknn <- rknnReg(tr.full_D, te.full_D, y=tr.full_D_hit, k=5, r=500,  mtry=trunc(sqrt(ncol(tr.full_D))),
        cluster=NULL, seed=NULL)

rknn.support <- rknnSupport(tr.full_D,tr.full_D_hit,k=5)
plot(rknn.support,main="SupportCriterionPlot")

sum(abs(rknn$pred - te.full_D_hit))/18
# 614073.1

######### IS THIS RIGHT??
#rknn with normalization
rknn_norm_full_D <- rknnReg(norm.tr.full_D, norm.te.full_D, y=norm.tr.full_D_hit, k=5, r=100, mtry=trunc(sqrt(ncol(norm.tr.full_D))))
sum(abs(rknn_norm_full_D$pred - norm.te.full_D_hit))/18
# 0.143155
mean(te.full_D_hit) * sum(abs(rknn_norm_full_D$pred - norm.te.full_D_hit))/18
# 292849


# backwards selection
# rknnb <-rknnBeg(data=tr.full_D,y=tr.full_D_hit,k=5,r=500,pk=.03)
# plot(rknnb)
# 
# rknnb
# rknnb_mod <- knn.reg(train = te.full_D[, bestset(rknnb)], y = tr.full_D_hit, k = 3)
# sum(abs(rknnb_mod$pred - te.full_D_hit))/18
# # 1390218

# rfe in caret


# with FNN
#knn.10.loocv <- knn.reg(tr.full_D, y=tr.full_D_hit, k=10)
# knn.10 <- knn.reg(tr.full_D, te.full_D, y=tr.full_D_hit, k=3)
# preds <- knn.10$pred
# 
# result <- data.frame(te.full_D_hit,preds)
# result$diff <- te.full_D_hit - preds
# result$sqdiff <- result$diff^2
# sqrt(sum(result$sqdiff))
# sum(abs(result$diff))/18
# # 1406545


######## FULL F ######

sub <- sample(1:nrow(full_F), size = .8*nrow(full_F))
tr.full_F <- full_F[sub,]
te.full_F <- full_F[-sub,]

tr.full_F_hit <- full_F$cap_hit[sub]
te.full_F_hit <- full_F$cap_hit[-sub]
tr.full_F_name <- tr.full_F$name
te.full_F_name <- te.full_F$name

tr.full_F <- tr.full_F[ , !names(tr.full_F) %in% c('cap_hit','name','Nat')]
te.full_F <- te.full_F[ , !names(te.full_F) %in% c('cap_hit','name','Nat')]

## NEED TO NORMALIZE PREDICTORS

norm.tr.full_F <- normalize.unit(tr.full_F)
norm.te.full_F <- normalize.unit(te.full_F)
norm.tr.full_F_hit <- normalize.unit(as.data.frame(tr.full_F_hit))
norm.te.full_F_hit <- normalize.unit(as.data.frame(te.full_F_hit))

norm.tr.full_F_hit <- as.list(norm.tr.full_F_hit)
norm.te.full_F_hit <- as.list(norm.te.full_F_hit)
norm.te.full_F_hit <- unlist(norm.te.full_F_hit)
norm.tr.full_F <- as.data.frame(norm.tr.full_F)
norm.te.full_F <- as.data.frame(norm.te.full_F)

# norm.tr.full_F$ev_iFOW <- NULL
# norm.te.full_F$ev_iFOW <- NULL

rknn_norm_full_F <- rknnReg(norm.tr.full_F, norm.te.full_F, y=norm.tr.full_F_hit, k=5, r=100, mtry=trunc(sqrt(ncol(norm.tr.full_F))))
sum(abs(rknn_norm_full_F$pred - norm.te.full_F_hit))/18
# 0.1600461
mean(te.full_F_hit) * sum(abs(rknn_norm_full_F$pred - norm.te.full_F_hit))/18
# 241946.9



####### PERF D ######

sub <- sample(1:nrow(perf_D), size = .8*nrow(perf_D))
tr.perf_D <- perf_D[sub,]
te.perf_D <- perf_D[-sub,]

tr.perf_D_hit <- perf_D$cap_hit[sub]
te.perf_D_hit <- perf_D$cap_hit[-sub]
tr.perf_D_name <- tr.perf_D$name
te.perf_D_name <- te.perf_D$name

tr.perf_D <- tr.perf_D[ , !names(tr.perf_D) %in% c('cap_hit','name','Nat')]
te.perf_D <- te.perf_D[ , !names(te.perf_D) %in% c('cap_hit','name','Nat')]

## NEED TO NORMALIZE PREDICTORS

norm.tr.perf_D <- normalize.unit(tr.perf_D)
norm.te.perf_D <- normalize.unit(te.perf_D)
norm.tr.perf_D_hit <- normalize.unit(as.data.frame(tr.perf_D_hit))
norm.te.perf_D_hit <- normalize.unit(as.data.frame(te.perf_D_hit))

norm.tr.perf_D_hit <- as.list(norm.tr.perf_D_hit)
norm.te.perf_D_hit <- as.list(norm.te.perf_D_hit)
norm.te.perf_D_hit <- unlist(norm.te.perf_D_hit)
norm.tr.perf_D <- as.data.frame(norm.tr.perf_D)
norm.te.perf_D <- as.data.frame(norm.te.perf_D)

norm.tr.perf_D$ev_iFOW <- NULL
norm.te.perf_D$ev_iFOW <- NULL

norm.tr.perf_D$pk_G <- NULL
norm.te.perf_D$pk_G <- NULL

colSums(is.na(norm.te.perf_D))
colSums(is.na(norm.tr.perf_D))
sum(is.na(norm.tr.perf_D))
sum(is.na(norm.te.perf_D))

rknn_norm_perf_D <- rknnReg(norm.tr.perf_D, norm.te.perf_D, y=norm.tr.perf_D_hit, k=5, r=100, mtry=trunc(sqrt(ncol(norm.tr.perf_D))))
sum(abs(rknn_norm_perf_D$pred - norm.te.perf_D_hit))/18
# 0.1072962
mean(te.perf_D_hit) * sum(abs(rknn_norm_perf_D$pred - norm.te.perf_D_hit))/18
# 242161.7




###### PERF F ######

sub <- sample(1:nrow(perf_F), size = .8*nrow(perf_F))
tr.perf_F <- perf_F[sub,]
te.perf_F <- perf_F[-sub,]

tr.perf_F_hit <- perf_F$cap_hit[sub]
te.perf_F_hit <- perf_F$cap_hit[-sub]
tr.perf_F_name <- tr.perf_F$name
te.perf_F_name <- te.perf_F$name

tr.perf_F <- tr.perf_F[ , !names(tr.perf_F) %in% c('cap_hit','name','Nat')]
te.perf_F <- te.perf_F[ , !names(te.perf_F) %in% c('cap_hit','name','Nat')]

## NEED TO NORMALIZE PREDICTORS

norm.tr.perf_F <- normalize.unit(tr.perf_F)
norm.te.perf_F <- normalize.unit(te.perf_F)
norm.tr.perf_F_hit <- normalize.unit(as.data.frame(tr.perf_F_hit))
norm.te.perf_F_hit <- normalize.unit(as.data.frame(te.perf_F_hit))

norm.tr.perf_F_hit <- as.list(norm.tr.perf_F_hit)
norm.te.perf_F_hit <- as.list(norm.te.perf_F_hit)
norm.te.perf_F_hit <- unlist(norm.te.perf_F_hit)
norm.tr.perf_F <- as.data.frame(norm.tr.perf_F)
norm.te.perf_F <- as.data.frame(norm.te.perf_F)

# norm.tr.perf_F$ev_iFOW <- NULL
# norm.te.perf_F$ev_iFOW <- NULL

# norm.tr.perf_F$pk_G <- NULL
# norm.te.perf_F$pk_G <- NULL

colSums(is.na(norm.te.perf_F))
colSums(is.na(norm.tr.perf_F))
sum(is.na(norm.tr.perf_F))
sum(is.na(norm.te.perf_F))

rknn_norm_perf_F <- rknnReg(norm.tr.perf_F, norm.te.perf_F, y=norm.tr.perf_F_hit, k=5, r=100, mtry=trunc(sqrt(ncol(norm.tr.perf_F))))
sum(abs(rknn_norm_perf_F$pred - norm.te.perf_F_hit))/18
# 0.1214152
mean(te.perf_F_hit) * sum(abs(rknn_norm_perf_F$pred - norm.te.perf_F_hit))/18
# 283541.4

print(rknn_norm_perf_F)
mat <-varUsed(rknn_norm_perf_F,by.KNN=TRUE)
# perf_F_fitted <- fitted(rknn_norm_perf_F)





