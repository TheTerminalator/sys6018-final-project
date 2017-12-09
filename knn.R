
# setwd("~/Desktop/UVA DSI/SYS6018/NHL_Salary")

library(tidyverse)
library(FNN)
library(rknn)
library(DMwR)
library(caret)

# Read in the data

full_D <- read_csv('full_D.csv')
full_F <- read_csv('full_F.csv')

perf_D <- read_csv('perfonly_D.csv')
perf_F <- read_csv('perfonly_F.csv')

# Drop columns that are irrelevant or categorical
drops <- c('X1','name','Nat')
perf_F <- perf_F[ , !(names(perf_F) %in% drops)]
perf_D <- perf_D[ , !(names(perf_D) %in% drops)]
full_D <- full_D[ , !(names(full_D) %in% drops)]
full_F <- full_F[ , !(names(full_F) %in% drops)]

# Remove response variable -- store it
fullF_hit <- full_F$cap_hit
full_F <- full_F[,-2]
perfF_hit <- perf_F$cap_hit
perf_F <- perf_F[,-2]
fullD_hit <- full_D$cap_hit
full_D <- full_D[,-2]
perfD_hit <- perf_D$cap_hit
perf_D <- perf_D[,-2]




# Full Forward Model
beg1_r2 <- 0
beg1_var <- list()
beg1_pred <- list()

# Run rknn model selection and knn regression model building 100 times, store relevant information
for (i in 1:100){
  beg1 <- rknnBeg(full_F, fullF_hit, k = 5, r = 500, mtry = trunc(sqrt(ncol(full_F))), stopat = 2)
  beg1_m <- knn.reg(train = full_F[, bestset(beg1)], y = fullF_hit, k = 5)
  beg1_var[[i]] <- bestset(beg1)
  beg1_r2[i] <- beg1_m$R2Pred
  beg1_pred[[i]] <- beg1_m$pred
}
mean(beg1_r2)
# 0.7170013

# Extract most used variables
beg1_var_un <- unlist(beg1_var)
beg1_vardf <- as.data.frame(table(beg1_var_un))
beg1_vardf <- beg1_vardf[with(beg1_vardf, order(-Freq)), ]
# xGF, ev_PTS, pp_PTS, pp_TOI, pp_A

# Extract and calculate average predictions
beg1_avg_pred <- list(0)
for(i in 1:190){
  beg1_avg_pred[i] <- (mean(unlist(lapply(beg1_pred, function(l) l[[i]]))))
}
beg1_avg_pred_u <- unlist(beg1_avg_pred)
mean(abs(fullF_hit - beg1_avg_pred_u))
# 527185


# Performance only Forward model
beg2_r2 <- 0
beg2_var <- list()
beg2_pred <- list()
# Run rknn model selection and knn regression model building 100 times, store relevant information
for (i in 1:100){
  beg2 <- rknnBeg(perf_F, perfF_hit, k = 5, r = 500, mtry = trunc(sqrt(ncol(perf_F))), stopat = 2)
  beg2_m <- knn.reg(train = perf_F[, bestset(beg2)], y = perfF_hit, k = 5)
  beg2_var[[i]] <- bestset(beg2)
  beg2_r2[i] <- beg2_m$R2Pred
  beg2_pred[[i]] <- beg2_m$pred
}
mean(beg2_r2)
# 0.7328054

# Extract most used variables
beg2_var_un <- unlist(beg2_var)
beg2_vardf <- as.data.frame(table(beg2_var_un))
beg2_vardf <- beg2_vardf[with(beg2_vardf, order(-Freq)), ]
# xGF, pp_TOI, ev_FF, ev_PTS, pp_PTS

# Extract and calculate average predictions
beg2_avg_pred <- list(0)
for(i in 1:190){
  beg2_avg_pred[i] <- (mean(unlist(lapply(beg2_pred, function(l) l[[i]]))))
}
beg2_avg_pred_u <- unlist(beg2_avg_pred)
mean(abs(perfF_hit - beg2_avg_pred_u))
# 519262.5


# Full Defensive Model
beg3_r2 <- 0
beg3_var <- list()
beg3_pred <- list()
# Run rknn model selection and knn regression model building 100 times, store relevant information
for (i in 1:100){
  beg3 <- rknnBeg(full_D, fullD_hit, k = 5, r = 500, mtry = trunc(sqrt(ncol(full_D))), stopat = 2)
  beg3_m <- knn.reg(train = full_D[, bestset(beg3)], y = fullD_hit, k = 5)
  beg3_var[[i]] <- bestset(beg3)
  beg3_r2[i] <- beg3_m$R2Pred
  beg3_pred[[i]] <- beg3_m$pred
}
mean(beg3_r2)
# 0.5956923

# Extract most used variables
beg3_var_un <- unlist(beg3_var)
beg3_vardf <- as.data.frame(table(beg3_var_un))
beg3_vardf <- beg3_vardf[with(beg3_vardf, order(-Freq)), ]
# xGF, ev_iFF, ev_FF, ev_SCF,ev_iCF 

# Extract and calculate average predictions
beg3_avg_pred <- list(0)
for(i in 1:90){
  beg3_avg_pred[i] <- (mean(unlist(lapply(beg3_pred, function(l) l[[i]]))))
}
beg3_avg_pred_u <- unlist(beg3_avg_pred)
mean(abs(fullD_hit - beg3_avg_pred_u))
# 666516


# Performance Only Defensive Model
beg4_r2 <- 0
beg4_var <- list()
beg4_pred <- list()
# Run rknn model selection and knn regression model building 100 times, store relevant information
for (i in 1:100){
beg4 <- rknnBeg(perf_D, perfD_hit, k = 5, r = 500, mtry = trunc(sqrt(ncol(perf_D))), stopat = 2)
beg4_m <- knn.reg(train = perf_D[, bestset(beg4)], y = perfD_hit, k = 5)
beg4_var[[i]] <- bestset(beg4)
beg4_r2[i] <- beg4_m$R2Pred
beg4_pred[[i]] <- beg4_m$pred
}
mean(beg4_r2)
# 0.5386063

# Extract most used variables
beg4_var_un <- unlist(beg4_var)
beg4_vardf <- as.data.frame(table(beg4_var_un))
beg4_vardf <- beg4_vardf[with(beg4_vardf, order(-Freq)), ]
# xGF, ev_iFF, ev_FF, ev_SCF, ev_iCF

# Extract and calculate average predictions
beg4_avg_pred <- list(0)
for(i in 1:90){
  beg4_avg_pred[i] <- (mean(unlist(lapply(beg4_pred, function(l) l[[i]]))))
  }
beg4_avg_pred_u <- unlist(beg4_avg_pred)
mean(abs(perfD_hit - beg4_avg_pred_u))
# 691129.9



# Plotting
options(scipen=5)
plot(beg4_avg_pred_u,perfD_hit, main='Perf Only Defense Model: Fitted vs Actual', xlab='Fitted Value ($)', ylab='Actual Value ($)')
abline(0,1)

options(scipen=5)
plot(beg1_avg_pred_u,fullF_hit, main='Full Forward Model: Fitted vs Actual', xlab='Fitted Value ($)', ylab='Actual Value ($)')
abline(0,1)

options(scipen=5)
plot(beg2_avg_pred_u,perfF_hit, main='Perf Only Forward Model: Fitted vs Actual', xlab='Fitted Value ($)', ylab='Actual Value ($)')
abline(0,1)

counts <- table(beg3_var_un)
counts <- counts[order(-counts)]
barplot(counts[10:1], main="Number of Times Variable Selected in rknn", horiz=TRUE,
        names.arg=c('xGA','ev_SCA','ev_iSF','Ovrl','ev_iCF','ev_CF','ev_FF',"ev_iFF","ev_SCF","xGF"), xlab='Frequency')





########## Previous work and tests ##########

### Scaling test

# full_F_s <- full_F
# full_F_s$ly_salary <- scale(full_F_s$ly_salary)
# 
# beg1_sc <- rknnBeg(full_F_s, fullF_hit, k = 5, r = 500, mtry = trunc(sqrt(ncol(full_F_s))), stopat = 2)
# beg1_m_sc <- knn.reg(train = full_F_s[, bestset(beg1_sc)], y = fullF_hit, k = 5)
# beg1_m_sc$R2Pred
# 
# 
# # Full F 
# 
# #5 fold cv
# fold1 <- 1:38
# fold2 <- 39:76
# fold3 <- 77:114
# fold4 <- 115:152
# fold5 <- 153:190
# full_F_MSE <- 0
# 
# full_F_train1 <- full_F[-fold1,]
# full_F_test1 <- full_F[fold1,]
# rknn1 <- rknnReg(full_F_train1,full_F_test1,y=fullF_hit[-fold1],k=5,r=500,mtry=5)
# full_F_MSE[1] <- (rknn1$pred - fullF_hit[fold1])^2
# 
# full_F_train2 <- full_F[-fold2,]
# full_F_test2 <- full_F[fold2,]
# rknn2 <- rknnReg(full_F_train2,full_F_test2,y=fullF_hit[-fold2],k=5,r=500,mtry=5)
# full_F_MSE[2] <- (rknn2$pred - fullF_hit[fold2])^2
# 
# full_F_train3 <- full_F[-fold3,]
# full_F_test3 <- full_F[fold3,]
# rknn3 <- rknnReg(full_F_train3,full_F_test3,y=fullF_hit[-fold3],k=5,r=500,mtry=5)
# full_F_MSE[3] <- (rknn3$pred - fullF_hit[fold3])^2
# 
# full_F_train4 <- full_F[-fold4,]
# full_F_test4 <- full_F[fold4,]
# rknn4 <- rknnReg(full_F_train4,full_F_test4,y=fullF_hit[-fold4],k=5,r=500,mtry=5)
# full_F_MSE[4] <- (rknn4$pred - fullF_hit[fold4])^2
# 
# full_F_train5 <- full_F[-fold5,]
# full_F_test5 <- full_F[fold5,]
# rknn5 <- rknnReg(full_F_train5,full_F_test5,y=fullF_hit[-fold5],k=5,r=500,mtry=5)
# full_F_MSE[5] <- (rknn5$pred - fullF_hit[fold5])^2
# 
# full_F_MSE <- mean(full_F_MSE)
# full_F_RMSE <- sqrt(full_F_MSE)
# 
# fitted <- c(rknn1$pred,rknn2$pred,rknn3$pred,rknn4$pred,rknn5$pred)
# full_F_MAE <- mean(abs(fullF_hit - fitted))
# # 722690.2
# 
# 
# # Perf F 
# 
# fold1 <- 1:38
# fold2 <- 39:76
# fold3 <- 77:114
# fold4 <- 115:152
# fold5 <- 153:190
# perf_F_MSE <- 0
# 
# perf_F_train1 <- perf_F[-fold1,]
# perf_F_test1 <- perf_F[fold1,]
# rknn1 <- rknnReg(perf_F_train1,perf_F_test1,y=perfF_hit[-fold1],k=5,r=500,mtry=5)
# perf_F_MSE[1] <- (rknn1$pred - perfF_hit[fold1])^2
# 
# perf_F_train2 <- perf_F[-fold2,]
# perf_F_test2 <- perf_F[fold2,]
# rknn2 <- rknnReg(perf_F_train2,perf_F_test2,y=perfF_hit[-fold2],k=5,r=500,mtry=5)
# perf_F_MSE[2] <- (rknn2$pred - perfF_hit[fold2])^2
# 
# perf_F_train3 <- perf_F[-fold3,]
# perf_F_test3 <- perf_F[fold3,]
# rknn3 <- rknnReg(perf_F_train3,perf_F_test3,y=perfF_hit[-fold3],k=5,r=500,mtry=5)
# perf_F_MSE[3] <- (rknn3$pred - perfF_hit[fold3])^2
# 
# perf_F_train4 <- perf_F[-fold4,]
# perf_F_test4 <- perf_F[fold4,]
# rknn4 <- rknnReg(perf_F_train4,perf_F_test4,y=perfF_hit[-fold4],k=5,r=500,mtry=5)
# perf_F_MSE[4] <- (rknn4$pred - perfF_hit[fold4])^2
# 
# perf_F_train5 <- perf_F[-fold5,]
# perf_F_test5 <- perf_F[fold5,]
# rknn5 <- rknnReg(perf_F_train5,perf_F_test5,y=perfF_hit[-fold5],k=5,r=500,mtry=5)
# perf_F_MSE[5] <- (rknn5$pred - perfF_hit[fold5])^2
# 
# perf_F_MSE <- mean(perf_F_MSE)
# perf_F_RMSE <- sqrt(perf_F_MSE)
# 
# fitted <- c(rknn1$pred,rknn2$pred,rknn3$pred,rknn4$pred,rknn5$pred)
# perf_F_MAE <- mean(abs(perfF_hit - fitted))
# # 720,952.1
# 
# 
# # Full D 
# 
# fold1 <- 1:18
# fold2 <- 18:35
# fold3 <- 36:53
# fold4 <- 54:71
# fold5 <- 72:90
# full_D_MSE <- 0
# 
# 
# full_D_train1 <- full_D[-fold1,]
# full_D_test1 <- full_D[fold1,]
# rknn1 <- rknnReg(full_D_train1,full_D_test1,y=fullD_hit[-fold1],k=5,r=500,mtry=5)
# full_D_MSE[1] <- (rknn1$pred - fullD_hit[fold1])^2
# 
# full_D_train2 <- full_D[-fold2,]
# full_D_test2 <- full_D[fold2,]
# rknn2 <- rknnReg(full_D_train2,full_D_test2,y=fullD_hit[-fold2],k=5,r=500,mtry=5)
# full_D_MSE[2] <- (rknn2$pred - fullD_hit[fold2])^2
# 
# full_D_train3 <- full_D[-fold3,]
# full_D_test3 <- full_D[fold3,]
# rknn3 <- rknnReg(full_D_train3,full_D_test3,y=fullD_hit[-fold3],k=5,r=500,mtry=5)
# full_D_MSE[3] <- (rknn3$pred - fullD_hit[fold3])^2
# 
# full_D_train4 <- full_D[-fold4,]
# full_D_test4 <- full_D[fold4,]
# rknn4 <- rknnReg(full_D_train4,full_D_test4,y=fullD_hit[-fold4],k=5,r=500,mtry=5)
# full_D_MSE[4] <- (rknn4$pred - fullD_hit[fold4])^2
# 
# full_D_train5 <- full_D[-fold5,]
# full_D_test5 <- full_D[fold5,]
# rknn5 <- rknnReg(full_D_train5,full_D_test5,y=fullD_hit[-fold5],k=5,r=500,mtry=5)
# full_D_MSE[5] <- (rknn5$pred - fullD_hit[fold5])^2
# 
# full_D_MSE <- mean(full_D_MSE)
# full_D_RMSE <- sqrt(full_D_MSE)
# 
# fitted <- c(rknn1$pred,rknn2$pred,rknn3$pred,rknn4$pred,rknn5$pred)
# full_D_MAE <- mean(abs(fullD_hit - fitted))
# # 1,359,519
# 
# 
# # Perf D 
# 
# fold1 <- 1:18
# fold2 <- 18:35
# fold3 <- 36:53
# fold4 <- 54:71
# fold5 <- 72:90
# perf_D_MSE <- 0
# 
# perf_D_train1 <- perf_D[-fold1,]
# perf_D_test1 <- perf_D[fold1,]
# rknn1 <- rknnReg(perf_D_train1,perf_D_test1,y=perfD_hit[-fold1],k=5,r=500,mtry=5)
# perf_D_MSE[1] <- (rknn1$pred - perfD_hit[fold1])^2
# 
# perf_D_train2 <- perf_D[-fold2,]
# perf_D_test2 <- perf_D[fold2,]
# rknn2 <- rknnReg(perf_D_train2,perf_D_test2,y=perfD_hit[-fold2],k=5,r=500,mtry=5)
# perf_D_MSE[2] <- (rknn2$pred - perfD_hit[fold2])^2
# 
# perf_D_train3 <- perf_D[-fold3,]
# perf_D_test3 <- perf_D[fold3,]
# rknn3 <- rknnReg(perf_D_train3,perf_D_test3,y=perfD_hit[-fold3],k=5,r=500,mtry=5)
# perf_D_MSE[3] <- (rknn3$pred - perfD_hit[fold3])^2
# 
# perf_D_train4 <- perf_D[-fold4,]
# perf_D_test4 <- perf_D[fold4,]
# rknn4 <- rknnReg(perf_D_train4,perf_D_test4,y=perfD_hit[-fold4],k=5,r=500,mtry=5)
# perf_D_MSE[4] <- (rknn4$pred - perfD_hit[fold4])^2
# 
# perf_D_train5 <- perf_D[-fold5,]
# perf_D_test5 <- perf_D[fold5,]
# rknn5 <- rknnReg(perf_D_train5,perf_D_test5,y=perfD_hit[-fold5],k=5,r=500,mtry=5)
# perf_D_MSE[5] <- (rknn5$pred - perfD_hit[fold5])^2
# 
# perf_D_MSE <- mean(perf_D_MSE)
# perf_D_RMSE <- sqrt(perf_D_MSE)
# 
# fitted <- c(rknn1$pred,rknn2$pred,rknn3$pred,rknn4$pred,rknn5$pred)
# perf_D_MAE <- mean(abs(perfD_hit - fitted))
# # 1363830
# 
# 
# 
# ########################## a
# 
# 
# 
# ######## FULL D 
# 
# sub <- sample(1:nrow(full_D), size = .8*nrow(full_D))
# tr.full_D <- full_D[sub,]
# te.full_D <- full_D[-sub,]
# 
# tr.full_D_hit <- full_D$cap_hit[sub]
# te.full_D_hit <- full_D$cap_hit[-sub]
# tr.full_D_name <- tr.full_D$name
# te.full_D_name <- te.full_D$name
# 
# tr.full_D <- tr.full_D[ , !names(tr.full_D) %in% c('cap_hit','name','Nat')]
# te.full_D <- te.full_D[ , !names(te.full_D) %in% c('cap_hit','name','Nat')]
# 
# ## NEED TO NORMALIZE PREDICTORS
# 
# norm.tr.full_D <- normalize.unit(tr.full_D)
# norm.te.full_D <- normalize.unit(te.full_D)
# norm.tr.full_D_hit <- normalize.unit(as.data.frame(tr.full_D_hit))
# norm.te.full_D_hit <- normalize.unit(as.data.frame(te.full_D_hit))
# 
# norm.tr.full_D_hit <- as.list(norm.tr.full_D_hit)
# norm.te.full_D_hit <- as.list(norm.te.full_D_hit)
# norm.te.full_D_hit <- unlist(norm.te.full_D_hit)
# norm.tr.full_D <- as.data.frame(norm.tr.full_D)
# norm.te.full_D <- as.data.frame(norm.te.full_D)
# 
# norm.tr.full_D$ev_iFOW <- NULL
# norm.te.full_D$ev_iFOW <- NULL
# 
# # Random KNN - good for high dimensions and small sample size 
# # with rknn
# rknn <- rknnReg(tr.full_D, te.full_D, y=tr.full_D_hit, k=5, r=500,  mtry=trunc(sqrt(ncol(tr.full_D))),
#         cluster=NULL, seed=NULL)
# 
# rknn.support <- rknnSupport(tr.full_D,tr.full_D_hit,k=5)
# plot(rknn.support,main="SupportCriterionPlot")
# 
# sum(abs(rknn$pred - te.full_D_hit))/18
# # 614073.1
# 
# ######### IS THIS RIGHT??
# #rknn with normalization
# rknn_norm_full_D <- rknnReg(norm.tr.full_D, norm.te.full_D, y=norm.tr.full_D_hit, k=5, r=100, mtry=trunc(sqrt(ncol(norm.tr.full_D))))
# sum(abs(rknn_norm_full_D$pred - norm.te.full_D_hit))
# # 0.143155
# mean(te.full_D_hit) * sum(abs(rknn_norm_full_D$pred - norm.te.full_D_hit))/18
# # 292849
# 
# 
# # backwards selection
# # rknnb <-rknnBeg(data=tr.full_D,y=tr.full_D_hit,k=5,r=500,pk=.03)
# # plot(rknnb)
# # 
# # rknnb
# # rknnb_mod <- knn.reg(train = te.full_D[, bestset(rknnb)], y = tr.full_D_hit, k = 3)
# # sum(abs(rknnb_mod$pred - te.full_D_hit))/18
# # # 1390218
# 
# 
# # with FNN
# #knn.10.loocv <- knn.reg(tr.full_D, y=tr.full_D_hit, k=10)
# # knn.10 <- knn.reg(tr.full_D, te.full_D, y=tr.full_D_hit, k=3)
# # preds <- knn.10$pred
# # 
# # result <- data.frame(te.full_D_hit,preds)
# # result$diff <- te.full_D_hit - preds
# # result$sqdiff <- result$diff^2
# # sqrt(sum(result$sqdiff))
# # sum(abs(result$diff))/18
# # # 1406545
# 
# 
# ######## FULL F
# 
# sub <- sample(1:nrow(full_F), size = .8*nrow(full_F))
# tr.full_F <- full_F[sub,]
# te.full_F <- full_F[-sub,]
# 
# tr.full_F_hit <- full_F$cap_hit[sub]
# te.full_F_hit <- full_F$cap_hit[-sub]
# tr.full_F_name <- tr.full_F$name
# te.full_F_name <- te.full_F$name
# 
# tr.full_F <- tr.full_F[ , !names(tr.full_F) %in% c('cap_hit','name','Nat')]
# te.full_F <- te.full_F[ , !names(te.full_F) %in% c('cap_hit','name','Nat')]
# 
# ## NEED TO NORMALIZE PREDICTORS
# 
# norm.tr.full_F <- normalize.unit(tr.full_F)
# norm.te.full_F <- normalize.unit(te.full_F)
# norm.tr.full_F_hit <- normalize.unit(as.data.frame(tr.full_F_hit))
# norm.te.full_F_hit <- normalize.unit(as.data.frame(te.full_F_hit))
# 
# norm.tr.full_F_hit <- as.list(norm.tr.full_F_hit)
# norm.te.full_F_hit <- as.list(norm.te.full_F_hit)
# norm.te.full_F_hit <- unlist(norm.te.full_F_hit)
# norm.tr.full_F <- as.data.frame(norm.tr.full_F)
# norm.te.full_F <- as.data.frame(norm.te.full_F)
# 
# # norm.tr.full_F$ev_iFOW <- NULL
# # norm.te.full_F$ev_iFOW <- NULL
# 
# rknn_norm_full_F <- rknnReg(norm.tr.full_F, norm.te.full_F, y=norm.tr.full_F_hit, k=5, r=100, mtry=trunc(sqrt(ncol(norm.tr.full_F))))
# sum(abs(rknn_norm_full_F$pred - norm.te.full_F_hit))/18
# # 0.1600461
# mean(te.full_F_hit) * sum(abs(rknn_norm_full_F$pred - norm.te.full_F_hit))/18
# # 241946.9
# 
# 
# 
# ####### PERF D #
# 
# sub <- sample(1:nrow(perf_D), size = .8*nrow(perf_D))
# tr.perf_D <- perf_D[sub,]
# te.perf_D <- perf_D[-sub,]
# 
# tr.perf_D_hit <- perf_D$cap_hit[sub]
# te.perf_D_hit <- perf_D$cap_hit[-sub]
# tr.perf_D_name <- tr.perf_D$name
# te.perf_D_name <- te.perf_D$name
# 
# tr.perf_D <- tr.perf_D[ , !names(tr.perf_D) %in% c('cap_hit','name','Nat')]
# te.perf_D <- te.perf_D[ , !names(te.perf_D) %in% c('cap_hit','name','Nat')]
# 
# ## NEED TO NORMALIZE PREDICTORS
# 
# norm.tr.perf_D <- normalize.unit(tr.perf_D)
# norm.te.perf_D <- normalize.unit(te.perf_D)
# norm.tr.perf_D_hit <- normalize.unit(as.data.frame(tr.perf_D_hit))
# norm.te.perf_D_hit <- normalize.unit(as.data.frame(te.perf_D_hit))
# 
# norm.tr.perf_D_hit <- as.list(norm.tr.perf_D_hit)
# norm.te.perf_D_hit <- as.list(norm.te.perf_D_hit)
# norm.te.perf_D_hit <- unlist(norm.te.perf_D_hit)
# norm.tr.perf_D <- as.data.frame(norm.tr.perf_D)
# norm.te.perf_D <- as.data.frame(norm.te.perf_D)
# 
# norm.tr.perf_D$ev_iFOW <- NULL
# norm.te.perf_D$ev_iFOW <- NULL
# 
# norm.tr.perf_D$pk_G <- NULL
# norm.te.perf_D$pk_G <- NULL
# 
# colSums(is.na(norm.te.perf_D))
# colSums(is.na(norm.tr.perf_D))
# sum(is.na(norm.tr.perf_D))
# sum(is.na(norm.te.perf_D))
# 
# rknn_norm_perf_D <- rknnReg(norm.tr.perf_D, norm.te.perf_D, y=norm.tr.perf_D_hit, k=5, r=100, mtry=trunc(sqrt(ncol(norm.tr.perf_D))))
# sum(abs(rknn_norm_perf_D$pred - norm.te.perf_D_hit))/18
# # 0.1072962
# mean(te.perf_D_hit) * sum(abs(rknn_norm_perf_D$pred - norm.te.perf_D_hit))/18
# # 242161.7
# 
# 
# 
# 
# ###### PERF F #
# 
# sub <- sample(1:nrow(perf_F), size = .8*nrow(perf_F))
# tr.perf_F <- perf_F[sub,]
# te.perf_F <- perf_F[-sub,]
# 
# tr.perf_F_hit <- perf_F$cap_hit[sub]
# te.perf_F_hit <- perf_F$cap_hit[-sub]
# tr.perf_F_name <- tr.perf_F$name
# te.perf_F_name <- te.perf_F$name
# 
# tr.perf_F <- tr.perf_F[ , !names(tr.perf_F) %in% c('cap_hit','name','Nat')]
# te.perf_F <- te.perf_F[ , !names(te.perf_F) %in% c('cap_hit','name','Nat')]
# 
# ## NEED TO NORMALIZE PREDICTORS
# 
# norm.tr.perf_F <- normalize.unit(tr.perf_F)
# norm.te.perf_F <- normalize.unit(te.perf_F)
# norm.tr.perf_F_hit <- normalize.unit(as.data.frame(tr.perf_F_hit))
# norm.te.perf_F_hit <- normalize.unit(as.data.frame(te.perf_F_hit))
# 
# norm.tr.perf_F_hit <- as.list(norm.tr.perf_F_hit)
# norm.te.perf_F_hit <- as.list(norm.te.perf_F_hit)
# norm.te.perf_F_hit <- unlist(norm.te.perf_F_hit)
# norm.tr.perf_F <- as.data.frame(norm.tr.perf_F)
# norm.te.perf_F <- as.data.frame(norm.te.perf_F)
# 
# # norm.tr.perf_F$ev_iFOW <- NULL
# # norm.te.perf_F$ev_iFOW <- NULL
# 
# # norm.tr.perf_F$pk_G <- NULL
# # norm.te.perf_F$pk_G <- NULL
# 
# colSums(is.na(norm.te.perf_F))
# colSums(is.na(norm.tr.perf_F))
# sum(is.na(norm.tr.perf_F))
# sum(is.na(norm.te.perf_F))
# 
# rknn_norm_perf_F <- rknnReg(norm.tr.perf_F, norm.te.perf_F, y=norm.tr.perf_F_hit, k=5, r=100, mtry=trunc(sqrt(ncol(norm.tr.perf_F))))
# sum(abs(rknn_norm_perf_F$pred - norm.te.perf_F_hit))/18
# # 0.1214152
# (max(te.perf_F_hit)-min(te.perf_F_hit)) * sum(abs(rknn_norm_perf_F$pred - norm.te.perf_F_hit))/18
# # 283541.4
# 
# # sc.tr.perf_F <- scale()
# sc.hit <- scale(tr.perf_F_hit)
# rknn_scale<- rknnReg(scale(tr.perf_F), scale(te.perf_F), y=sc.hit, 
#                             k=5, r=100, mtry=trunc(sqrt(ncol(te.perf_F))))
# pred <- rknn_scale$pred
# sc.hit
# 
# sc <- scale(te.perf_F_hit)
# fit <- unscale(pred,sc.hit)
# 
# sum(abs(te.perf_F_hit - fit))
# 
# 
# 
# ?unscale
# ?scale
# print(rknn_norm_perf_F)
# mat <-varUsed(rknn_norm_perf_F,by.KNN=TRUE)
# # perf_F_fitted <- fitted(rknn_norm_perf_F)
# 
# 









