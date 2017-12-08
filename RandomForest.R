#Court Haworth
#ach2wd
#Final Project
#Random Forest



#NOTE: Data cleaning and exploration have been thoroughly done in a separate R file.
#The data in the csv's below have already been cleaned and explored.


library(randomForest)

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


#-------------------------------------------Random Forest------------------------------------------------
full_D$position = "D"
full_F$position = "F"
perfonly_D$position = "D"
perfonly_F$position = "F"

full = rbind(full_D,full_F)
perf = rbind(perfonly_D,perfonly_F)




#---------------------------------------------K-Fold Cross Validation--------------------------------------------
#I am selecting K-fold Cross-Validation for its ease of use and will be 
#using 5 as my k-fold value. To derive this value, I performed three different K-fold CV's using k = 3,
#k=5, and k=10. For the purpose of this assignment, K=5 made the most sense both computationally and
#logically. 

# K-fold cross-validation
set.seed(1)
rf.full=randomForest(cap_hit~.,data=full,mtry=6,importance =TRUE)


rf.perf=randomForest(cap_hit~.,data=perf,mtry=6,importance =TRUE)

write.csv(full,"Full.csv")
write.csv(perf,"Perf.csv")