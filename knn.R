

library(tidyverse)
library(FNN)
library(rknn)


full_D <- read_csv('full_D.csv')
full_F <- read_csv('full_F.csv')

perf_D <- read_csv('perfonly_D.csv')
perf_F <- read_csv('perfonly_F.csv')

full_D$X1 <- NULL
full_F$X1 <- NULL
perf_D$X1 <- NULL
perf_F$X1 <- NULL


sub = sample(1:nrow(clean_F), size = 0.8*nrow(clean_F))
train = clean_F[sub, ]
valid = clean_F[-sub, ]


sub <- sample(1:nrow(full_D), size = .8*nrow(full_D))
tr.full_D <- full_D[sub,]
te.full_D <- full_D[-sub,]

tr.full_D_hit <- full_D$cap_hit[sub]
te.full_D_hit <- full_D$cap_hit[-sub]
tr.full_D_name <- tr.full_D$name
te.full_D_name <- te.full_D$name

tr.full_D <- tr.full_D[ , !names(tr.full_D) %in% c('cap_hit','name','Nat')]
te.full_D <- te.full_D[ , !names(te.full_D) %in% c('cap_hit','name','Nat')]

## NEED TO NORMALIZE PREDICTORS?

# with rknn


# rfe in caret


# with FNN
#knn.10.loocv <- knn.reg(tr.full_D, y=tr.full_D_hit, k=10)
#knn.5 <- knn.reg(tr.full_D, te.full_D, y=tr.full_D_hit, k=5)



