# Data Cleaning

library(calibrate)
library(tidyverse)
library(stats)
library(MASS)
library(leaps)
library(DAAG)
library(olsrr)
library(glmnet)

# setwd("~/Desktop/UVA DSI/STAT 6021/NHL_WAR")

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
fa[fa$PLAYER == 'Alexander Wennberg','PLAYER'] <- 'Alex Wennberg'



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

# Combine names
name_concat <- function(df){
  df$FullName <- paste(df$`First Name`,df$`Last Name`, sep=' ')
  drops <- c('First Name','Last Name')
  df <- df[ , !(names(df) %in% drops)]
}

main17_cl <- name_concat(main17_cl)
ev17_cl <- name_concat(ev17_cl)
pp17_cl <- name_concat(pp17_cl)
pk17_cl <- name_concat(pk17_cl)

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

# aggregated performance for the past 3 years
# Prev 3 - GPG APG PTSPG, TOIPG 

# 2016
main16_c <- main16[,c('First Name','Last Name','GP','G','A','PTS')] 
pp16_c <- pp16[,c('First Name','Last Name','PPG','PPA','PPP','54TOI')]
pk16_c <- pk16[,c('First Name','Last Name','SHBlk','SHHitF','45TOI')]
ev16_c <- ev16[,c('First Name','Last Name','ESG','ESA','ESP','Corsi','55TOI')]

# 2015
main15_c <- main15[,c('First Name','Last Name','GP','G','A','PTS')]
pp15_c <- pp15[,c('First Name','Last Name','PPG','PPA','PPP','54TOI')]
pk15_c <- pk15[,c('First Name','Last Name','SHBlk','SHHitF','45TOI')]
ev15_c <- ev15[,c('First Name','Last Name','ESG','ESA','ESP','Corsi','ESTOI')]

# 2014
main14_c <- main14[,c('First Name','Last Name','GP','G','A','PTS')]
pp14_c <- pp14[,c('First Name','Last Name','PPG','PPA','PPP','PPTOI')]
pk14_c <- pk14[,c('First Name','Last Name','SHBlk','SHHitF','SHTOI')]
ev14_c <- ev14[,c('First Name','Last Name','ESG','ESA','ESP','Corsi','ESTOI')]


# Clean variable names and merge Prev3 together
main16_c <- name_concat(main16_c)
pp16_c <- name_concat(pp16_c)
pk16_c <- name_concat(pk16_c)
ev16_c <- name_concat(ev16_c)

main15_c <- name_concat(main15_c)
pp15_c <- name_concat(pp15_c)
pk15_c <- name_concat(pk15_c)
ev15_c <- name_concat(ev15_c)

main14_c <- name_concat(main14_c)
pp14_c <- name_concat(pp14_c)
pk14_c <- name_concat(pk14_c)
ev14_c <- name_concat(ev14_c)

prev_pp <- merge(pp16_c, pp15_c, by = "FullName", all = TRUE)
prev_pp <- merge(prev_pp, pp14_c, by = "FullName", all = TRUE)
prev_pp[is.na(prev_pp)] <- 0

prev_pp$totppg <- prev_pp$PPG.x + prev_pp$PPG.y + prev_pp$PPG
prev_pp$totppa <- prev_pp$PPA.x + prev_pp$PPA.y + prev_pp$PPA
prev_pp$totppp <- prev_pp$totppa + prev_pp$totppg
prev_pp$totpptoi <- prev_pp$`54TOI.x` + prev_pp$`54TOI.y` + prev_pp$PPTOI

prev_main <- merge(main16_c, main15_c, by = "FullName", all = TRUE)
prev_main <- merge(prev_main, main14_c, by = "FullName", all = TRUE)
prev_main[is.na(prev_main)] <- 0

# Calculate aggregated statistics
prev_main$totgp <- prev_main$GP.x + prev_main$GP.y + prev_main$GP

prev_ev <- merge(ev16_c, ev15_c, by = "FullName", all = TRUE)
prev_ev <- merge(prev_ev, ev14_c, by = "FullName", all = TRUE)
prev_ev[is.na(prev_ev)] <- 0

prev_ev$totevg <- prev_ev$ESG.x + prev_ev$ESG.y + prev_ev$ESG
prev_ev$toteva <- prev_ev$ESA.x + prev_ev$ESA.y + prev_ev$ESA
prev_ev$totevp <- prev_ev$ESP.x + prev_ev$ESP.y + prev_ev$ESP
prev_ev$totevtoi <- prev_ev$ESTOI.x + prev_ev$ESTOI.y + prev_ev$`55TOI`
prev_ev$totevcorsi <- prev_ev$Corsi.x + prev_ev$Corsi.y + prev_ev$Corsi

prev_pk <- merge(pk16_c, pk15_c, by = "FullName", all = TRUE)
prev_pk <- merge(prev_pk, pk14_c, by = "FullName", all = TRUE)
prev_pk[is.na(prev_pk)] <- 0

prev_pk$totshblk <- prev_pk$SHBlk.x + prev_pk$SHBlk.y + prev_pk$SHBlk
prev_pk$totshhitf <- prev_pk$SHHitF.x + prev_pk$SHHitF.y + prev_pk$SHHitF
prev_pk$totshtoi <- prev_pk$`45TOI.x` + prev_pk$`45TOI.y` + prev_pk$SHTOI

prevstats <- data_frame(prev_main$FullName, prev_main$totgp, prev_ev$totevg, prev_ev$toteva, prev_ev$totevp, 
                        prev_ev$totevtoi, prev_ev$totevcorsi, prev_pp$totppg, 
                        prev_pp$totppa, prev_pp$totppp, prev_pp$totpptoi, prev_pk$totshblk,
                        prev_pk$totshhitf, prev_pk$totshtoi)


names(prevstats) <- c('FullName','Prev3GP','ev_Prev3G','ev_Prev3A','ev_Prev3PTS','ev_Prev3TOI','ev_Prev3Corsi',
                      'pp_Prev3PPG','pp_Prev3PPA','pp_Prev3PPP','pp_Prev3TOI','pk_Prev3Blk','pk_Prev3Hits','pk_Prev3TOI')

# Merge previous 3 year stats with 2017
fa_clean <- left_join(fa_clean, prevstats, by= c('PLAYER'='FullName'))


# mean height, weight, get draftyr from age, drf rd and year and ovrl max
fa_clean$DftRd[is.na(fa_clean$DftRd)] <- max(na.omit(fa_clean$DftRd)) + 1
fa_clean$Ovrl[is.na(fa_clean$Ovrl)] <- max(na.omit(fa_clean$Ovrl)) + 1
fa_clean$DftYr[is.na(fa_clean$DftYr)] <- 2017 - fa_clean$AGE[is.na(fa_clean$DftYr)] + 18

fa_clean$Ht[is.na(fa_clean$Ht)] <- mean(na.omit(fa_clean$Ht))
fa_clean$Wt[is.na(fa_clean$Wt)] <- mean(na.omit(fa_clean$Wt))

# Drop rows with 0 in GP
fa_clean <- fa_clean[!is.na(fa_clean$GP),]

# 1st 2nd 3rd
fa_clean$`1st`[is.na(fa_clean$`1st`)] <- 0
fa_clean$`2nd`[is.na(fa_clean$`2nd`)] <- 0
fa_clean$`3rd`[is.na(fa_clean$`3rd`)] <- 0
fa_clean$stars <- fa_clean$`1st` + fa_clean$`2nd` + fa_clean$`3rd`
fa_clean$`1st` <- NULL
fa_clean$`2nd` <- NULL
fa_clean$`3rd` <- NULL

sum(is.na(fa_clean))
colSums(is.na(fa_clean))

fa_clean[is.na(fa_clean)] <- 0

#clean names
names(fa_clean)[names(fa_clean) == 'CAP HIT'] = 'cap_hit'
names(fa_clean)[names(fa_clean) == 'LY Salary'] = 'ly_salary'
names(fa_clean)[names(fa_clean) == 'LY Cap Hit'] = 'ly_cap_hit'
names(fa_clean)[names(fa_clean) == 'pk_TOI/GP'] = 'pk_TOIGP'
names(fa_clean)[names(fa_clean) == 'E+/-'] = 'Eplusminus'
names(fa_clean)[names(fa_clean) == '+/-'] = 'plusminus'
names(fa_clean)[names(fa_clean) == 'PLAYER'] = 'name'

#remove other contract data
fa_clean$VALUE = NULL
fa_clean$DATE = NULL
fa_clean$LENGTH = NULL

#convert percentages
percent_cols = names(fa_clean)[grepl('%', names(fa_clean)) == T]

remove_special = function(x) gsub('[$%,]', '', x)
fa_clean[, percent_cols] = apply(fa_clean[, percent_cols], 2, remove_special)
fa_clean[, percent_cols] = apply(fa_clean[, percent_cols], 2, as.numeric)
fa_clean[fa_clean$name == 'Barclay Goodrow','ev_IPP%'] <- 0

#convert dollars
fa_clean$cap_hit = lapply(fa_clean$cap_hit, remove_special)
fa_clean$ly_cap_hit = lapply(fa_clean$ly_cap_hit, remove_special)
fa_clean$ly_salary = lapply(fa_clean$ly_salary, remove_special)

fa_clean$cap_hit = lapply(fa_clean$cap_hit, as.numeric)
fa_clean$ly_cap_hit = lapply(fa_clean$ly_cap_hit, as.numeric)
fa_clean$ly_salary = lapply(fa_clean$ly_salary, as.numeric)


fa_clean$cap_hit = unlist(fa_clean$cap_hit)
fa_clean$ly_cap_hit = unlist(fa_clean$ly_cap_hit)
fa_clean$ly_salary = unlist(fa_clean$ly_salary)

colSums(is.na(fa_clean))
sapply(fa_clean, class)

names <- c('name','POS','Nat')
allnames <- names(fa_clean)
num_names<- subset(allnames, !(allnames %in% names))

fa_clean_char <- data.frame(fa_clean[names])

fa_clean <- as.data.frame(apply(fa_clean[num_names], 2, as.numeric))
fa_clean[is.na(fa_clean)] <- 0
fa_clean <- cbind(fa_clean_char,fa_clean)

# Separate out non-performance stats

non_perf <- c('Pos','Nat', 'AGE','Ht','Wt','DftYr','DftRd','Ovrl','ly_salary','ly_cap_hit')

fa_clean_full <- fa_clean
fa_clean_perf <- fa_clean[, !(names(fa_clean) %in% non_perf)]

# Separate D
full_d <- fa_clean_full[fa_clean_full$POS == 'D',]
full_f <- fa_clean_full[!fa_clean_full$POS == 'D',]

perf_d <- fa_clean_perf[fa_clean_perf$POS == 'D',]
perf_f <- fa_clean_perf[!fa_clean_perf$POS == 'D',]

#drop POS
full_d$POS <- NULL
full_f$POS <- NULL
perf_d$POS <- NULL
perf_f$POS <- NULL


write.csv(full_d, file = "full_D.csv")
write.csv(full_f, file = "full_F.csv")

write.csv(perf_d, file = "perfonly_D.csv")
write.csv(perf_f, file = "perfonly_F.csv")


