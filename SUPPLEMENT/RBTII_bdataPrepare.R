############################
### RBTII_bdataPrepare.R ###
############################

# Author: Alexander V. Lebedev
# Date: 2017-03-15

# DESCRIPTION:
# The present R-script reads behavioural output from REBOOTII
# tests and generates individual score summaries for data analysis

# Load libraries:
library(xlsx)
library(R.matlab)
library(zoo)

# Set working directory:
dir <- '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-11-29/'



#############
# REASONING #
#############


# Beta:
beta <- read.csv2(paste(dir, 'transfer_beta.csv', sep=''),sep=';')
beta <- beta[which(nchar(beta$id)>3),]
ids <- names(table(beta$id))

bet <- as.data.frame(matrix(NA, length(ids), 17))

colnames(bet) <- c('ID',
                   'v1.Acc', 'v1.RT', 'v1.RTCORR',
                   'v2.Acc', 'v2.RT', 'v2.RTCORR',
                   'v3.Acc', 'v3.RT', 'v3.RTCORR',
                   'DATE.v1', 'DATE.v2', 'DATE.v3', 'DATE-diff12','DATE-diff23',
                   'NumIt.v1','NumIt.v2')


for (i in 1:length(ids)) {
  
  tmp <- subset(beta, beta$id==ids[i])
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  
  bet[i,'ID'] <- ids[i]
  
  # Visit 1:
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  bet[i,'NumIt.v1'] <- dim(tmpv1)[1]
  bet[i,'v1.Acc'] <- sum(tmpv1$answer_is_correct)
  bet[i,'v1.RT'] <- mean(tmpv1$response_time)
  bet[i,'v1.RTCORR'] <- mean(tmpv1$response_time[tmpv1$answer_is_correct==1])
  bet[i,'DATE.v1'] <- dates[1]
  
  # Visit 2:
  tmpv2 <- tmp[as.Date(tmp$date) == dates[2],]
  bet[i,'NumIt.v2'] <- dim(tmpv2)[1]
  bet[i,'v2.Acc'] <- sum(tmpv2$answer_is_correct)
  bet[i,'v2.RT'] <- mean(tmpv2$response_time)
  bet[i,'v2.RTCORR'] <- mean(tmpv2$response_time[tmpv2$answer_is_correct==1])
  bet[i,'DATE.v2'] <- dates[2]
  
  bet[i,'DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  
  if (length(dates)==3){
    # Visit 3:
    tmpv3 <- tmp[as.Date(tmp$date) == dates[3],]
    bet[i,'NumIt.v3'] <- dim(tmpv3)[1]
    bet[i,'v3.Acc'] <- sum(tmpv3$answer_is_correct)
    bet[i,'v3.RT'] <- mean(tmpv3$response_time)
    bet[i,'v3.RTCORR'] <- mean(tmpv3$response_time[tmpv3$answer_is_correct==1])
    bet[i,'DATE.v3'] <- dates[3]
    bet[i,'DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  }
}
bet[is.na(bet$DATE.v2),c('NumIt.v2')] <- NA
write.xlsx(bet, paste(dir, 'summary/', 'transfer_beta', '.xlsx', sep=''), row.names = F)
rm(bet)


# Ravens:
raven <- read.csv2(paste(dir, 'transfer_raven.csv', sep=''),sep=';')
raven <- raven[which(nchar(raven$id)>3),]
ids <- names(table(raven$id))


rav <- as.data.frame(matrix(NA, length(ids), 21))


colnames(rav) <- c('ID',
                   'v1.Score', 'v1.RT', 'v1.RTCORR', 'numR.v1',
                   'v2.Score', 'v2.RT', 'v2.RTCORR', 'numR.v2',
                   'v3.Score', 'v3.RT', 'v3.RTCORR', 'numR.v3',
                   'DATE.v1', 'DATE.v2', 'DATE.v3','DATE-diff12', 'DATE-diff23',
                   'NumIt.v1','NumIt.v2','NumIt.v3')

for (i in 1:length(ids)) {
  
  tmp <- subset(raven, raven$id==ids[i])
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  
  rav[i,'ID'] <- ids[i]
  
  # Visit 1:
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  
  rav[i,'NumIt.v1'] <- dim(tmpv1)[1]
  rav[i,'v1.Score'] <- sum(tmpv1$answer_is_correct)
  rav[i,'v1.RT'] <- mean(tmpv1$response_time)
  rav[i,'v1.RTCORR'] <- mean(tmpv1$response_time[tmpv1$answer_is_correct==1])
  rav[i,'numR.v1'] <- dim(tmpv1)[1]
  rav[i,'DATE.v1'] <- dates[1]
  
  
  
  # Visit 2:
  tmpv2 <- tmp[as.Date(tmp$date) == dates[2],]
  
  rav[i,'NumIt.v2'] <- dim(tmpv2)[1]
  rav[i,'v2.Score'] <- sum(tmpv2$answer_is_correct)
  rav[i,'v2.RT'] <- mean(tmpv2$response_time)
  rav[i,'v2.RTCORR'] <- mean(tmpv2$response_time[tmpv2$answer_is_correct==1])
  rav[i,'numR.v2'] <- dim(tmpv2)[1]
  rav[i,'DATE.v2'] <- dates[2]
  rav[i,'DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  
  if (length(dates)==3){
    # Visit 3:
    tmpv3 <- tmp[as.Date(tmp$date) == dates[3],]
    
    rav[i,'NumIt.v3'] <- dim(tmpv3)[1]
    rav[i,'v3.Score'] <- sum(tmpv3$answer_is_correct)
    rav[i,'v3.RT'] <- mean(tmpv3$response_time)
    rav[i,'v3.RTCORR'] <- mean(tmpv3$response_time[tmpv3$answer_is_correct==1])
    rav[i,'numR.v3'] <- dim(tmpv3)[1]
    rav[i,'DATE.v3'] <- dates[1]
    rav[i,'DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  }
}
rav[is.na(rav$DATE.v2),c('NumIt.v2')] <- NA
write.xlsx(rav, paste(dir, 'summary/', 'transfer_raven', '.xlsx', sep=''), row.names = F)
rm(rav)

# WASI:
wasitransf <- read.csv2(paste(dir, 'transfer_wasi.csv', sep=''),sep=';')
wasitransf <- wasitransf[which(nchar(wasitransf$id)>3),]
ids <- names(table(wasitransf$id))



wasi <- as.data.frame(matrix(NA, length(ids), 18))


colnames(wasi) <- c('ID',
                    'v1.Acc', 'v1.RT', 'v1.RTCORR',
                    'v2.Acc', 'v2.RT', 'v2.RTCORR',
                    'v3.Acc', 'v3.RT', 'v3.RTCORR',
                    'DATE.v1', 'DATE.v2', 'DATE.v3','DATE-diff12', 'DATE-diff23',
                    'NumIt.v1','NumIt.v2','NumIt.v3')



for (i in 1:length(ids)) {
  
  tmp <- subset(wasitransf, wasitransf$id==ids[i])
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  
  
  wasi[i,'ID'] <- ids[i]
  
  # Visit 1:
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  wasi[i,'NumIt.v1'] <- dim(tmpv1)[1]
  wasi[i,'v1.Acc'] <- sum(tmpv1$answer_is_correct)
  wasi[i,'v1.RT'] <- mean(tmpv1$response_time)
  wasi[i,'v1.RTCORR'] <- mean(tmpv1$response_time[tmpv1$answer_is_correct==1])
  
  # Visit 2:
  tmpv2 <- tmp[as.Date(tmp$date) == dates[2],]
  wasi[i,'NumIt.v2'] <- dim(tmpv2)[1]
  wasi[i,'v2.Acc'] <- sum(tmpv2$answer_is_correct)
  wasi[i,'v2.RT'] <- mean(tmpv2$response_time)
  wasi[i,'v2.RTCORR'] <- mean(tmpv2$response_time[tmpv2$answer_is_correct==1])
  
  wasi[i,'DATE.v1'] <- dates[1]
  wasi[i,'DATE.v2'] <- dates[2]
  wasi[i,'DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  
  if (length(dates)==3){
    # Visit 3:
    tmpv2 <- tmp[as.Date(tmp$date) == dates[3],]
    wasi[i,'NumIt.v3'] <- dim(tmpv3)[1]
    wasi[i,'v3.Acc'] <- sum(tmpv3$answer_is_correct)
    wasi[i,'v3.RT'] <- mean(tmpv3$response_time)
    wasi[i,'v3.RTCORR'] <- mean(tmpv3$response_time[tmpv3$answer_is_correct==1])
    wasi[i,'DATE.v3'] <- dates[3]
    wasi[i,'DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  }
}
wasi[is.na(wasi$DATE.v2),c('NumIt.v2')] <- NA
write.xlsx(wasi, paste(dir, 'summary/', 'transfer_wasi', '.xlsx', sep=''), row.names = F)
rm(wasi)


# Analogies:
analogies <- read.csv2(paste(dir, 'transfer_analogies.csv', sep=''),sep=';')
analogies <- analogies[which(nchar(analogies$id)>3),]
ids <- names(table(analogies$id))


anl <- as.data.frame(matrix(NA, length(ids), 17))


colnames(anl) <- c('ID',
                   'v1.Acc', 'v1.RT', 'v1.RTCORR',
                   'v2.Acc', 'v2.RT', 'v2.RTCORR',
                   'v3.Acc', 'v3.RT', 'v3.RTCORR',
                   'DATE.v1', 'DATE.v2', 'DATE.v3', 'DATE-diff12','DATE-diff23',
                   'NumIt.v1','NumIt.v2')



for (i in 1:length(ids)) {
  
  tmp <- subset(analogies, analogies$id==ids[i])
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  tmpv2 <- tmp[as.Date(tmp$date) == dates[2],]
  anl[i,'NumIt.v1'] <- dim(tmpv1)[1]
  anl[i,'NumIt.v2'] <- dim(tmpv2)[1]
  
  anl[i,'ID'] <- ids[i]
  anl[i,'v1.Acc'] <- sum(tmpv1$answer_is_correct)
  anl[i,'v1.RT'] <- mean(tmpv1$response_time)
  anl[i,'v1.RTCORR']<- mean(tmpv1$response_time[tmpv1$answer_is_correct==1])
  
  anl[i,'v2.Acc'] <- sum(tmpv2$answer_is_correct)
  anl[i,'v2.RT'] <- mean(tmpv2$response_time)
  anl[i,'v2.RTCORR']<- mean(tmpv2$response_time[tmpv2$answer_is_correct==1])
  
  anl[i,'DATE.v1'] <- dates[1]
  anl[i,'DATE.v2'] <- dates[2]
  anl[i,'DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  
  if (length(dates)==3){
    tmpv3 <- tmp[as.Date(tmp$date) == dates[3],]
    anl[i,'NumIt.v3'] <- dim(tmpv3)[1]
    anl[i,'v3.Acc'] <- sum(tmpv3$answer_is_correct)
    anl[i,'v3.RT'] <- mean(tmpv3$response_time)
    anl[i,'v3.RTCORR']<- mean(tmpv3$response_time[tmpv3$answer_is_correct==1])
    
    anl[i,'DATE.v3'] <- dates[3]
    anl[i,'DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  }
}
anl[is.na(anl$DATE.v2),c('NumIt.v2')] <- NA

write.xlsx(anl, paste(dir, 'summary/', 'transfer_analogies', '.xlsx', sep=''), row.names = F)
rm(anl)

# Syllogisms:
syllogisms <- read.csv2(paste(dir, 'transfer_syllogism.csv', sep=''),sep=';')
syllogisms <- syllogisms[which(nchar(syllogisms$id)>3),]
ids <- names(table(syllogisms$id))

syll <- as.data.frame(matrix(NA, length(ids), 18))
colnames(syll) <- c('ID',
                    'v1.Acc', 'v1.RT', 'v1.RTCORR',
                    'v2.Acc', 'v2.RT', 'v2.RTCORR',
                    'v3.Acc', 'v3.RT', 'v3.RTCORR',
                    'DATE.v1', 'DATE.v2', 'DATE.v3','DATE-diff12', 'DATE-diff23',
                    'NumIt.v1','NumIt.v2','NumIt.v3')


for (i in 1:length(ids)) {
  
  tmp <- subset(syllogisms, syllogisms$id==ids[i])
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  
  syll[i,'ID'] <- ids[i]
  
  # Visit 1:
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  syll[i,'NumIt.v1'] <- dim(tmpv1)[1]
  
  syll[i,'v1.Acc'] <- sum(tmpv1$answer_is_correct)
  syll[i,'v1.RT'] <- mean(tmpv1$response_time)
  syll[i,'v1.RTCORR'] <- mean(tmpv1$response_time[tmpv1$answer_is_correct==1])
  
  # Visit 2:
  tmpv2 <- tmp[as.Date(tmp$date) == dates[2],]
  syll[i,'NumIt.v2'] <- dim(tmpv2)[1]
  
  syll[i,'v2.Acc'] <- sum(tmpv2$answer_is_correct)
  syll[i,'v2.RT'] <- mean(tmpv2$response_time)
  syll[i,'v2.RTCORR'] <- mean(tmpv2$response_time[tmpv2$answer_is_correct==1])
  
  syll[i,'DATE.v1'] <- dates[1]
  syll[i,'DATE.v2'] <- dates[2]
  syll[i,'DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  
  if (length(dates)==3){
    # Visit 3:
    tmpv3 <- tmp[as.Date(tmp$date) == dates[3],]
    syll[i,'NumIt.v3'] <- dim(tmpv3)[1]
    
    syll[i,'v3.Acc'] <- sum(tmpv3$answer_is_correct)
    syll[i,'v3.RT'] <- mean(tmpv3$response_time)
    syll[i,'v3.RTCORR'] <- mean(tmpv3$response_time[tmpv3$answer_is_correct==1])
    syll[i,'DATE.v3'] <- dates[3]
    syll[i,'DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  }
}
syll[is.na(syll$DATE.v2),c('NumIt.v2')] <- NA
write.xlsx(syll, paste(dir, 'summary/', 'transfer_syllogism', '.xlsx', sep=''), row.names = F)
rm(syll)

# Word Comprehension:
wordcompr <- read.csv2(paste(dir, 'transfer_word_comprehension.csv', sep=''),sep=';')
wordcompr <- wordcompr[which(nchar(wordcompr$id)>3),]
ids <- names(table(wordcompr$id))



wcomp <- as.data.frame(matrix(NA, length(ids), 18))


colnames(wcomp) <- c('ID',
                     'v1.Acc', 'v1.RT', 'v1.RTCORR',
                     'v2.Acc', 'v2.RT', 'v2.RTCORR',
                     'v3.Acc', 'v3.RT', 'v3.RTCORR',
                     'DATE.v1', 'DATE.v2', 'DATE.v3','DATE-diff12', 'DATE-diff23',
                     'NumIt.v1','NumIt.v2','NumIt.v3')



for (i in 1:length(ids)) {
  
  tmp <- subset(wordcompr, wordcompr$id==ids[i])
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  
  wcomp[i,'ID'] <- ids[i]
  
  # Visit 1:
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  wcomp[i,'NumIt.v1'] <- dim(tmpv1)[1]
  wcomp[i,'v1.Acc'] <- sum(tmpv1$answer_is_correct)
  wcomp[i,'v1.RT'] <- mean(tmpv1$response_time)
  wcomp[i,'v1.RTCORR'] <- mean(tmpv1$response_time[tmpv1$answer_is_correct==1])
  
  # Visit 2:
  tmpv2 <- tmp[as.Date(tmp$date) == dates[2],]
  wcomp[i,'NumIt.v2'] <- dim(tmpv2)[1]
  wcomp[i,'v2.Acc'] <- sum(tmpv2$answer_is_correct)
  wcomp[i,'v2.RT'] <- mean(tmpv2$response_time)
  wcomp[i,'v2.RTCORR'] <- mean(tmpv2$response_time[tmpv2$answer_is_correct==1])
  
  wcomp[i,'DATE.v1'] <- dates[1]
  wcomp[i,'DATE.v2'] <- dates[2]
  wcomp[i,'DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  
  
  if (length(dates)==3){
    # Visit 3:
    tmpv3 <- tmp[as.Date(tmp$date) == dates[3],]
    wcomp[i,'NumIt.v3'] <- dim(tmpv3)[1]
    wcomp[i,'v3.Acc'] <- sum(tmpv3$answer_is_correct)
    wcomp[i,'v3.RT'] <- mean(tmpv3$response_time)
    wcomp[i,'v3.RTCORR'] <- mean(tmpv3$response_time[tmpv3$answer_is_correct==1])
    wcomp[i,'DATE.v3'] <- dates[3]
    wcomp[i,'DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  }
}
wcomp[is.na(wcomp$DATE.v2),c('NumIt.v2')] <- NA
write.xlsx(wcomp, paste(dir, 'summary/', 'transfer_word_comprehension', '.xlsx', sep=''), row.names = F)
rm(wcomp)

# Verbal Inference:
vinference <- read.csv2(paste(dir, 'transfer_verbal_inference.csv', sep=''),sep=';')
vinference <- vinference[which(nchar(vinference$id)>3),]
ids <- names(table(vinference$id))

vinf <- as.data.frame(matrix(NA, length(ids), 18))


colnames(vinf) <- c('ID',
                    'v1.Acc', 'v1.RT', 'v1.RTCORR',
                    'v2.Acc', 'v2.RT', 'v2.RTCORR',
                    'v3.Acc', 'v3.RT', 'v3.RTCORR',
                    'DATE.v1', 'DATE.v2', 'DATE.v3','DATE-diff12', 'DATE-diff23',
                    'NumIt.v1','NumIt.v2','NumIt.v3')


for (i in 1:length(ids)) {
  tmp <- subset(vinference, vinference$id==ids[i])
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  
  
  vinf[i,'ID'] <- ids[i]
  
  # Visit 1:
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  vinf[i,'NumIt.v1'] <- dim(tmpv1)[1]
  
  vinf[i,'v1.Acc'] <- sum(tmpv1$answer_is_correct)
  vinf[i,'v1.RT'] <- mean(tmpv1$response_time)
  vinf[i,'v1.RTCORR'] <- mean(tmpv1$response_time[tmpv1$answer_is_correct==1])
  
  
  
  # Visit 2:
  tmpv2 <- tmp[as.Date(tmp$date) == dates[2],]
  vinf[i,'NumIt.v2'] <- dim(tmpv2)[1]
  vinf[i,'v2.Acc'] <- sum(tmpv2$answer_is_correct)
  vinf[i,'v2.RT'] <- mean(tmpv2$response_time)
  vinf[i,'v2.RTCORR'] <- mean(tmpv2$response_time[tmpv2$answer_is_correct==1])
  
  vinf[i,'DATE.v1'] <- dates[1]
  vinf[i,'DATE.v2'] <- dates[2]
  vinf[i,'DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  
  if (length(dates)==3){
    # Visit 3:
    tmpv3 <- tmp[as.Date(tmp$date) == dates[3],]
    vinf[i,'NumIt.v3'] <- dim(tmpv3)[1]
    vinf[i,'v3.Acc'] <- sum(tmpv3$answer_is_correct)
    vinf[i,'v3.RT'] <- mean(tmpv3$response_time)
    vinf[i,'v3.RTCORR'] <- mean(tmpv3$response_time[tmpv3$answer_is_correct==1])
    vinf[i,'DATE.v3'] <- dates[3]
    vinf[i,'DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  }
}
vinf[is.na(vinf$DATE.v2),c('NumIt.v2')] <- NA
write.xlsx(vinf, paste(dir, 'summary/', 'transfer_verbal_inference', '.xlsx', sep=''), row.names = F)
rm(vinf)


##################
# WORKING MEMORY #
##################

# NEAR N-back:
nback <- read.csv2(paste(dir, 'transfer_near_nback_trial.csv', sep=''),sep=';')
nback <- nback[which(nchar(nback$id)>3),]
nback2 <- subset(nback, is.element(nback$level, 2:5))
nback3 <- subset(nback, is.element(nback$level, 7:10))

ids <- names(table(nback2$id))

nb2 <- as.data.frame(matrix(NA, length(ids), 22))
nb3 <- nb2
colnames(nb2) <- paste('NB2.',c('ID',
                                'TP.v1', 'TN.v1', 'SimpAcc.v1','OA.v1','RT.v1',
                                'TP.v2', 'TN.v2', 'SimpAcc.v2','OA.v2','RT.v2',
                                'TP.v3', 'TN.v3', 'SimpAcc.v3','OA.v3','RT.v3',
                                'DATE.v1', 'DATE.v2', 'DATE.v3',
                                'DATE-diff12', 'DATE-diff23','NumIt'), sep='')


colnames(nb3) <- paste('NB3.',c('ID',
                                'TP.v1', 'TN.v1', 'SimpAcc.v1','OA.v1','RT.v1',
                                'TP.v2', 'TN.v2', 'SimpAcc.v2','OA.v2','RT.v2',
                                'TP.v3', 'TN.v3', 'SimpAcc.v3','OA.v3','RT.v3',
                                'DATE.v1', 'DATE.v2', 'DATE.v3',
                                'DATE-diff12', 'DATE-diff23', 'NumIt'), sep='')                               


for (i in 1:length(ids)) {
  
  # 2-back:
  tmp2 <- subset(nback2, nback2$id==ids[i])
  dates <- as.Date(names(table(as.Date(tmp2$date)))) # extract dates for each subject
  
  tmp2v1 <- tmp2[as.Date(tmp2$date) == dates[1],]
  tmp2v2 <- tmp2[as.Date(tmp2$date) == dates[2],]
  
  tmp3 <- subset(nback3, nback3$id==ids[i])
  tmp3v1 <- tmp3[as.Date(tmp3$date) == dates[1],]
  tmp3v2 <- tmp3[as.Date(tmp3$date) == dates[2],]
  
  
  nb2[i,'NB2.ID'] <- ids[i]
  nb2[i,'NB2.TP.v1'] <- mean(tmp2v1[tmp2v1$switch==1,'answercorrect']) #tp
  nb2[i,'NB2.TN.v1'] <- mean(tmp2v1[tmp2v1$switch==0,'answercorrect']) #tn
  nb2[i,'NB2.SimpAcc.v1'] <- mean(tmp2v1$answercorrect)
  nb2[i,'NB2.OA.v1'] <- (nb2[i,'NB2.TP.v1']+nb2[i,'NB2.TN.v1'])/2
  nb2[i,'NB2.RT.v1'] <- mean(tmp2v1$responsetime[tmp2v1$responsetime>-1 & tmp2v1$switch==1])
  nb2[i,'NB2.TP.v2'] <- mean(tmp2v2[tmp2v2$switch==1,'answercorrect']) #tp
  nb2[i,'NB2.TN.v2'] <- mean(tmp2v2[tmp2v2$switch==0,'answercorrect']) #tn
  nb2[i,'NB2.SimpAcc.v2'] <- mean(tmp2v2$answercorrect)
  nb2[i,'NB2.OA.v2'] <- (nb2[i,'NB2.TP.v2']+nb2[i,'NB2.TN.v2'])/2
  nb2[i,'NB2.RT.v2'] <- mean(tmp2v2$responsetime[tmp2v2$responsetime>-1 & tmp2v2$switch==1])
  
  nb2[i,'NB2.DATE.v1'] <- dates[1]
  nb2[i,'NB2.DATE.v2'] <- dates[2]
  
  if (length(dates)==3){
    tmp2v3 <- tmp2[as.Date(tmp2$date) == dates[3],]
    nb2[i,'NB2.TP.v3'] <- mean(tmp2v3[tmp2v3$switch==1,'answercorrect']) #tp
    nb2[i,'NB2.TN.v3'] <- mean(tmp2v3[tmp2v3$switch==0,'answercorrect']) #tn
    nb2[i,'NB2.SimpAcc.v3'] <- mean(tmp2v3$answercorrect)
    nb2[i,'NB2.OA.v3'] <- (nb2[i,'NB2.TP.v3']+nb2[i,'NB2.TN.v3'])/2
    nb2[i,'NB2.RT.v3'] <- mean(tmp2v3$responsetime[tmp2v3$responsetime>-1 & tmp2v3$switch==1])
    nb2[i,'NB2.DATE.v3'] <- dates[3]
  }
  else {
    ## do nothing
  }
  
  nb2[i,'NB2.DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  nb2[i,'NB2.DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  nb2[i,'NB2.NumIt'] <- dim(tmp2)[1]
  
  nb3[i,'NB3.ID'] <- ids[i]
  nb3[i,'NB3.TP.v1'] <- mean(tmp3v1[tmp3v1$switch==1,'answercorrect']) #tp
  nb3[i,'NB3.TN.v1'] <- mean(tmp3v1[tmp3v1$switch==0,'answercorrect']) #tn
  nb3[i,'NB3.SimpAcc.v1'] <- mean(tmp3v1$answercorrect)
  nb3[i,'NB3.OA.v1'] <- (nb3[i,'NB3.TP.v1']+nb3[i,'NB3.TN.v1'])/2
  nb3[i,'NB3.RT.v1'] <- mean(tmp3v1$responsetime[tmp3v1$responsetime>-1 & tmp3v1$switch==1])
  nb3[i,'NB3.TP.v2'] <- mean(tmp3v2[tmp3v2$switch==1,'answercorrect']) #tp
  nb3[i,'NB3.TN.v2'] <- mean(tmp3v2[tmp3v2$switch==0,'answercorrect']) #tn
  nb3[i,'NB3.SimpAcc.v2'] <- mean(tmp3v2$answercorrect)
  nb3[i,'NB3.OA.v2'] <- (nb3[i,'NB3.TP.v2']+nb3[i,'NB3.TN.v2'])/2
  nb3[i,'NB3.RT.v2'] <- mean(tmp3v2$responsetime[tmp3v2$responsetime>-1 & tmp3v2$switch==1])
  
  nb3[i,'NB3.DATE.v1'] <- dates[1]
  nb3[i,'NB3.DATE.v2'] <- dates[2]
  
  if (length(dates)==3){
    tmp3v3 <- tmp3[as.Date(tmp3$date) == dates[3],]
    nb3[i,'NB3.TP.v3'] <- mean(tmp3v3[tmp3v3$switch==1,'answercorrect']) #tp
    nb3[i,'NB3.TN.v3'] <- mean(tmp3v3[tmp3v3$switch==0,'answercorrect']) #tn
    nb3[i,'NB3.SimpAcc.v3'] <- mean(tmp3v3$answercorrect)
    nb3[i,'NB3.OA.v3'] <- (nb3[i,'NB3.TP.v3']+nb3[i,'NB3.TN.v3'])/2
    nb3[i,'NB3.RT.v3'] <- mean(tmp3v3$responsetime[tmp3v3$responsetime>-1 & tmp3v3$switch==1])
    nb3[i,'NB3.DATE.v3'] <- dates[3]
  }
  
  nb3[i,'NB3.DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  nb3[i,'NB3.DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  nb3[i,'NB3.NumIt'] <- dim(tmp3)[1]
}

write.xlsx(nb2, paste(dir, 'summary/', 'transfer_near_2back', '.xlsx', sep=''), row.names = F)
write.xlsx(nb3, paste(dir, 'summary/', 'transfer_near_3back', '.xlsx', sep=''), row.names = F)
rm(nb2,nb3)



# Near rule-switching:
rswitch <- read.csv2(paste(dir, 'transfer_near_ruleswitching_trial.csv', sep=''),sep=';')
rswitch <- rswitch[which(nchar(rswitch$id)>3),]
ids <- names(table(rswitch$id))


rsw <- as.data.frame(matrix(NA, length(ids), 39))



colnames(rsw) <- c('ID',
                   'v1.Acc.sw', 'v1.RT.sw', 'v1.Acc.ps', 'v1.RT.ps', 'v1.Acc.ns', 'v1.RT.ns',
                   'v1.RT.swCORR', 'v1.RT.psCORR', 'v1.RT.nsCORR',
                   'v2.Acc.sw', 'v2.RT.sw', 'v2.Acc.ps', 'v2.RT.ps', 'v2.Acc.ns', 'v2.RT.ns',
                   'v2.RT.swCORR', 'v2.RT.psCORR','v2.RT.nsCORR',
                   'v3.Acc.sw', 'v3.RT.sw', 'v3.Acc.ps', 'v3.RT.ps', 'v3.Acc.ns', 'v3.RT.ns',
                   'v3.RT.swCORR', 'v3.RT.psCORR', 'v3.RT.nsCORR',
                   'DATE.v1', 'DATE.v2', 'DATE.v3','DATE-diff12', 'DATE-diff23',
                   'NumItPre.v1','NumItPost.v1','NumItPre.v2','NumItPost.v2','NumItPre.v3','NumItPost.v3')



for (i in 1:length(ids)) {
  # Switch 3: non-switch
  # Switch 1: switch
  # Switch 2: post-switch
  tmp <- subset(rswitch, rswitch$id==ids[i])
  
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  tmpv2 <- tmp[as.Date(tmp$date) == dates[2],]
  rsw[i,'NumItPre.v1'] <- dim(tmpv1)[1]
  rsw[i,'NumItPre.v2'] <- dim(tmpv2)[1]
  
  rsw[i,'ID'] <- ids[i]
  rsw[i,'v1.Acc.sw'] <- mean(tmpv1[tmpv1$switch==1,'answercorrect'])
  rsw[i,'v1.RT.sw'] <- mean(tmpv1[tmpv1$switch==1,'responsetime'])
  rsw[i,'v1.Acc.ps'] <- mean(tmpv1[tmpv1$switch==2,'answercorrect'])
  rsw[i,'v1.RT.ps'] <- mean(tmpv1[tmpv1$switch==2,'responsetime'])
  rsw[i,'v1.Acc.ns'] <- mean(tmpv1[tmpv1$switch==3,'answercorrect'])
  rsw[i,'v1.RT.ns'] <- mean(tmpv1[tmpv1$switch==3,'responsetime'])
  rsw[i,'v1.RT.swCORR'] <- mean(tmpv1[tmpv1$switch==1 & tmpv1$answercorrect==1,'responsetime'])
  rsw[i,'v1.RT.psCORR'] <- mean(tmpv1[tmpv1$switch==2 & tmpv1$answercorrect==1,'responsetime'])
  rsw[i,'v1.RT.nsCORR'] <- mean(tmpv1[tmpv1$switch==3 & tmpv1$answercorrect==1,'responsetime'])
  
  rsw[i,'v2.Acc.sw'] <- mean(tmpv2[tmpv2$switch==1,'answercorrect'])
  rsw[i,'v2.RT.sw'] <- mean(tmpv2[tmpv2$switch==1,'responsetime'])
  rsw[i,'v2.Acc.ps'] <- mean(tmpv2[tmpv2$switch==2,'answercorrect'])
  rsw[i,'v2.RT.ps'] <- mean(tmpv2[tmpv2$switch==2,'responsetime'])
  rsw[i,'v2.Acc.ns'] <- mean(tmpv2[tmpv2$switch==3,'answercorrect'])
  rsw[i,'v2.RT.ns'] <- mean(tmpv2[tmpv2$switch==3,'responsetime'])
  rsw[i,'v2.RT.swCORR'] <- mean(tmpv2[tmpv2$switch==1 & tmpv2$answercorrect==1,'responsetime'])
  rsw[i,'v2.RT.psCORR'] <- mean(tmpv2[tmpv2$switch==2 & tmpv2$answercorrect==1,'responsetime'])
  rsw[i,'v2.RT.nsCORR'] <- mean(tmpv2[tmpv2$switch==3 & tmpv2$answercorrect==1,'responsetime'])
  
  rsw[i,'DATE.v1'] <- dates[1]
  rsw[i,'DATE.v2'] <- dates[2]
  rsw[i,'DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  
  if (length(dates)==3){
    tmpv3 <- tmp[as.Date(tmp$date) == dates[3],]
    rsw[i,'NumItPre.v3'] <- dim(tmpv3)[1]
    
    rsw[i,'v3.Acc.sw'] <- mean(tmpv3[tmpv3$switch==1,'answercorrect'])
    rsw[i,'v3.RT.sw'] <- mean(tmpv3[tmpv3$switch==1,'responsetime'])
    rsw[i,'v3.Acc.ps'] <- mean(tmpv3[tmpv3$switch==2,'answercorrect'])
    rsw[i,'v3.RT.ps'] <- mean(tmpv3[tmpv3$switch==2,'responsetime'])
    rsw[i,'v3.Acc.ns'] <- mean(tmpv3[tmpv3$switch==3,'answercorrect'])
    rsw[i,'v3.RT.ns'] <- mean(tmpv3[tmpv3$switch==3,'responsetime'])
    rsw[i,'v3.RT.swCORR'] <- mean(tmpv3[tmpv3$switch==1 & tmpv3$answercorrect==1,'responsetime'])
    rsw[i,'v3.RT.psCORR'] <- mean(tmpv3[tmpv3$switch==2 & tmpv3$answercorrect==1,'responsetime'])
    rsw[i,'v3.RT.nsCORR'] <- mean(tmpv3[tmpv3$switch==3 & tmpv3$answercorrect==1,'responsetime'])
    rsw[i,'DATE.v3'] <- dates[3]
    rsw[i,'DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  }
}
rsw[is.na(rsw$DATE.v2),c('NumItPre.v2','NumItPost.v2')] <- NA

write.xlsx(rsw, paste(dir, 'summary/', 'transfer_near_ruleswitching', '.xlsx', sep=''), row.names = F)
rm(rsw)


# Near task-switching:
tswitch <- read.csv2(paste(dir, 'transfer_near_taskswitching_trial.csv', sep=''),sep=';')
tswitch <- tswitch[which(nchar(tswitch$id)>3),]
ids <- names(table(tswitch$id))

tsw <- as.data.frame(matrix(NA, length(ids), 27))


colnames(tsw) <- c('ID', 
                   'v1.Acc.sw0.lvl23', 'v1.RT.sw0.lvl23',
                   'v1.Acc.sw1.lvl23', 'v1.RT.sw1.lvl23', 
                   'v2.Acc.sw0.lvl456', 'v2.RT.sw0.lvl456', 
                   'v2.Acc.sw1.lvl456', 'v2.RT.sw1.lvl456',
                   'v3.Acc.sw0.lv789', 'v3.RT.sw0.lvl789', 
                   'v3.Acc.sw1.lv789', 'v3.RT.sw1.lvl789',
                   'v1.nearTSW.lvl23.cost','v2.nearTSW.lvl23.cost','v3.nearTSW.lvl23.cost',
                   'DATE.v0.lvl23', 'DATE.v2', 'DATE.v3','DATE-diff12', 'DATE-diff23',
                   'NumItPre.v0.lvl23','NumItPost.v0.lvl23','NumItPre.v2','NumItPost.v2','NumItPre.v3','NumItPost.v3')

for (i in 1:length(ids)) {
  
  tmp <- subset(tswitch, tswitch$id==ids[i])
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  tmpv2 <- tmp[as.Date(tmp$date) == dates[2],]
  tsw[i,'NumItPre.v0.lvl23'] <- dim(tmpv1)[1]
  tsw[i,'NumItPre.v2'] <- dim(tmpv2)[1]
  
  tsw[i,'ID'] <- ids[i]
  
  # Visit 1:
  tsw[i,'v1.Acc.sw0.lvl23'] <- mean(tmpv1[tmpv1$switch==0 & (tmpv1$level==2 | tmpv1$level==3),'answercorrect'])
  tsw[i,'v1.RT.sw0.lvl23'] <- mean(tmpv1[tmpv1$switch==0 & (tmpv1$level==2 | tmpv1$level==3),'responsetime'])
  tsw[i,'v1.Acc.sw1.lvl23'] <- mean(tmpv1[tmpv1$switch==1 & (tmpv1$level==2 | tmpv1$level==3),'answercorrect'])
  tsw[i,'v1.RT.sw1.lvl23'] <- mean(tmpv1[tmpv1$switch==1 & (tmpv1$level==2 | tmpv1$level==3),'responsetime'])
  tsw[i,'v1.nearTSW.lvl23.cost'] <- tsw[i,'v1.RT.sw1.lvl23']-tsw[i,'v1.RT.sw0.lvl23']
  
  tsw[i,'v1.Acc.sw0.lvl456'] <- mean(tmpv1[tmpv1$switch==0 & (tmpv1$level==4 | tmpv1$level==5 | tmpv1$level==6),'answercorrect'])
  tsw[i,'v1.RT.sw0.lvl456'] <- mean(tmpv1[tmpv1$switch==0 & (tmpv1$level==4 | tmpv1$level==5 | tmpv1$level==6),'responsetime'])
  tsw[i,'v1.Acc.sw1.lvl456'] <- mean(tmpv1[tmpv1$switch==1 & (tmpv1$level==4 | tmpv1$level==5 | tmpv1$level==6),'answercorrect'])
  tsw[i,'v1.RT.sw1.lvl456'] <- mean(tmpv1[tmpv1$switch==1 & (tmpv1$level==4 | tmpv1$level==5 | tmpv1$level==6),'responsetime'])
  tsw[i,'v1.nearTSW.lvl456.cost'] <- tsw[i,'v1.RT.sw1.lvl456']-tsw[i,'v1.RT.sw0.lvl456']
  
  tsw[i,'v1.Acc.sw0.lvl789'] <- mean(tmpv1[tmpv1$switch==0 & (tmpv1$level==7 | tmpv1$level==8 | tmpv1$level==9),'answercorrect'])
  tsw[i,'v1.RT.sw0.lvl789'] <- mean(tmpv1[tmpv1$switch==0 & (tmpv1$level==7 | tmpv1$level==8 | tmpv1$level==9),'responsetime'])
  tsw[i,'v1.Acc.sw1.lvl789'] <- mean(tmpv1[tmpv1$switch==1 & (tmpv1$level==7 | tmpv1$level==8 | tmpv1$level==9),'answercorrect'])
  tsw[i,'v1.RT.sw1.lvl789'] <- mean(tmpv1[tmpv1$switch==1 & (tmpv1$level==7 | tmpv1$level==8 | tmpv1$level==9),'responsetime'])
  tsw[i,'v1.nearTSW.lvl789.cost'] <- tsw[i,'v1.RT.sw1.lvl789']-tsw[i,'v1.RT.sw0.lvl789']
  
  # Visit 2:
  tsw[i,'v2.Acc.sw0.lvl23'] <- mean(tmpv2[tmpv2$switch==0 & (tmpv2$level==2 | tmpv2$level==3),'answercorrect'])
  tsw[i,'v2.RT.sw0.lvl23'] <- mean(tmpv2[tmpv2$switch==0 & (tmpv2$level==2 | tmpv2$level==3),'responsetime'])
  tsw[i,'v2.Acc.sw1.lvl23'] <- mean(tmpv2[tmpv2$switch==1 & (tmpv2$level==2 | tmpv2$level==3),'answercorrect'])
  tsw[i,'v2.RT.sw1.lvl23'] <- mean(tmpv2[tmpv2$switch==1 & (tmpv2$level==2 | tmpv2$level==3),'responsetime'])
  tsw[i,'v2.nearTSW.lvl23.cost'] <- tsw[i,'v2.RT.sw1.lvl23']-tsw[i,'v2.RT.sw0.lvl23']
  
  tsw[i,'v2.Acc.sw0.lvl456'] <- mean(tmpv2[tmpv2$switch==0 & (tmpv2$level==4 | tmpv2$level==5 | tmpv2$level==6),'answercorrect'])
  tsw[i,'v2.RT.sw0.lvl456'] <- mean(tmpv2[tmpv2$switch==0 & (tmpv2$level==4 | tmpv2$level==5 | tmpv2$level==6),'responsetime'])
  tsw[i,'v2.Acc.sw1.lvl456'] <- mean(tmpv2[tmpv2$switch==1 & (tmpv2$level==4 | tmpv2$level==5 | tmpv2$level==6),'answercorrect'])
  tsw[i,'v2.RT.sw1.lvl456'] <- mean(tmpv2[tmpv2$switch==1 & (tmpv2$level==4 | tmpv2$level==5 | tmpv2$level==6),'responsetime'])
  tsw[i,'v2.nearTSW.lvl456.cost'] <- tsw[i,'v2.RT.sw1.lvl456']-tsw[i,'v2.RT.sw0.lvl456']
  
  tsw[i,'v2.Acc.sw0.lvl789'] <- mean(tmpv2[tmpv2$switch==0 & (tmpv2$level==7 | tmpv2$level==8 | tmpv2$level==9),'answercorrect'])
  tsw[i,'v2.RT.sw0.lvl789'] <- mean(tmpv2[tmpv2$switch==0 & (tmpv2$level==7 | tmpv2$level==8 | tmpv2$level==9),'responsetime'])
  tsw[i,'v2.Acc.sw1.lvl789'] <- mean(tmpv2[tmpv2$switch==1 & (tmpv2$level==7 | tmpv2$level==8 | tmpv2$level==9),'answercorrect'])
  tsw[i,'v2.RT.sw1.lvl789'] <- mean(tmpv2[tmpv2$switch==1 & (tmpv2$level==7 | tmpv2$level==8 | tmpv2$level==9),'responsetime'])
  tsw[i,'v2.nearTSW.lvl789.cost'] <- tsw[i,'v2.RT.sw1.lvl789']-tsw[i,'v2.RT.sw0.lvl789']
  
  
  tsw[i,'DATE.v0.lvl23'] <- dates[1]
  tsw[i,'DATE.v2'] <- dates[2]
  tsw[i,'DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  
  if (length(dates)==3){
    tmpv3 <- tmp[as.Date(tmp$date) == dates[3],]
    tsw[i,'NumItPre.v3'] <- dim(tmpv3)[1]
    
    # Visit 3:
    
    tsw[i,'v3.Acc.sw0.lvl23'] <- mean(tmpv3[tmpv3$switch==0 & (tmpv3$level==2 | tmpv3$level==3),'answercorrect'])
    tsw[i,'v3.RT.sw0.lvl23'] <- mean(tmpv3[tmpv3$switch==0 & (tmpv3$level==2 | tmpv3$level==3),'responsetime'])
    tsw[i,'v3.Acc.sw1.lvl23'] <- mean(tmpv3[tmpv3$switch==1 & (tmpv3$level==2 | tmpv3$level==3),'answercorrect'])
    tsw[i,'v3.RT.sw1.lvl23'] <- mean(tmpv3[tmpv3$switch==1 & (tmpv3$level==2 | tmpv3$level==3),'responsetime'])
    tsw[i,'v3.nearTSW.lvl23.cost'] <- tsw[i,'v3.RT.sw1.lvl23']-tsw[i,'v3.RT.sw0.lvl23']
    
    tsw[i,'v3.Acc.sw0.lvl456'] <- mean(tmpv3[tmpv3$switch==0 & (tmpv3$level==4 | tmpv3$level==5 | tmpv3$level==6),'answercorrect'])
    tsw[i,'v3.RT.sw0.lvl456'] <- mean(tmpv3[tmpv3$switch==0 & (tmpv3$level==4 | tmpv3$level==5 | tmpv3$level==6),'responsetime'])
    tsw[i,'v3.Acc.sw1.lvl456'] <- mean(tmpv3[tmpv3$switch==1 & (tmpv3$level==4 | tmpv3$level==5 | tmpv3$level==6),'answercorrect'])
    tsw[i,'v3.RT.sw1.lvl456'] <- mean(tmpv3[tmpv3$switch==1 & (tmpv3$level==4 | tmpv3$level==5 | tmpv3$level==6),'responsetime'])
    tsw[i,'v3.nearTSW.lvl456.cost'] <- tsw[i,'v3.RT.sw1.lvl456']-tsw[i,'v3.RT.sw0.lvl456']
    
    tsw[i,'v3.Acc.sw0.lvl789'] <- mean(tmpv3[tmpv3$switch==0 & (tmpv3$level==7 | tmpv3$level==8 | tmpv3$level==9),'answercorrect'])
    tsw[i,'v3.RT.sw0.lvl789'] <- mean(tmpv3[tmpv3$switch==0 & (tmpv3$level==7 | tmpv3$level==8 | tmpv3$level==9),'responsetime'])
    tsw[i,'v3.Acc.sw1.lvl789'] <- mean(tmpv3[tmpv3$switch==1 & (tmpv3$level==7 | tmpv3$level==8 | tmpv3$level==9),'answercorrect'])
    tsw[i,'v3.RT.sw1.lvl789'] <- mean(tmpv3[tmpv3$switch==1 & (tmpv3$level==7 | tmpv3$level==8 | tmpv3$level==9),'responsetime'])
    tsw[i,'v3.nearTSW.lvl789.cost'] <- tsw[i,'v3.RT.sw1.lvl789']-tsw[i,'v3.RT.sw0.lvl789']
    
    tsw[i,'DATE.v3'] <- dates[3]
    tsw[i,'DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  }
}
tsw[is.na(tsw$DATE.v2),c('NumItPre.v2','NumItPost.v2')] <- NA
write.xlsx(tsw, paste(dir, 'summary/', 'transfer_near_taskswitching', '.xlsx', sep=''), row.names = F)
rm(tsw)


# Flanker:
flanker <- read.csv2(paste(dir, 'transfer_numeric_flanker_task_trial.csv', sep=''),sep=';')
flanker <- flanker[which(nchar(flanker$id)>3),]
ids <- names(table(flanker$id))
ids <- names(table(flanker$id))

fl <- as.data.frame(matrix(NA, length(ids), 30))

colnames(fl) <- c('ID',
                  'v1.Acc.sw0', 'v1.RT.sw0', 'v1.Acc.sw1', 'v1.RT.sw1', 
                  'v1.RT.sw0CORR', 'v1.RT.sw1CORR',
                  'v2.Acc.sw0', 'v2.RT.sw0', 'v2.Acc.sw1', 'v2.RT.sw1', 
                  'v2.RT.sw0CORR', 'v2.RT.sw1CORR',
                  'v3.Acc.sw0', 'v3.RT.sw0', 'v3.Acc.sw1', 'v3.RT.sw1', 
                  'v3.RT.sw0CORR', 'v3.RT.sw1CORR',
                  'DATE.v1', 'DATE.v2', 'DATE.v3','DATE-diff12', 'DATE-diff23',
                  'NumItPre.v1','NumItPost.v1','NumItPre.v2','NumItPost.v2','NumItPre.v3','NumItPost.v3')



for (i in 1:length(ids)) {
  tmp <- subset(flanker, flanker$id==ids[i])
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  
  fl[i,'ID'] <- ids[i]
  
  # Visit 1:
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  fl[i,'NumItPre.v1'] <- dim(tmpv1)[1]
  
  fl[i,'v1.Acc.sw0'] <- mean(tmpv1[tmpv1$switch==0,'answercorrect'])
  fl[i,'v1.RT.sw0'] <- mean(tmpv1[tmpv1$switch==0,'responsetime'])
  fl[i,'v1.Acc.sw1'] <- mean(tmpv1[tmpv1$switch==1,'answercorrect'])
  fl[i,'v1.RT.sw1'] <- mean(tmpv1[tmpv1$switch==1,'responsetime'])
  fl[i,'v1.RT.sw0CORR'] <- mean(tmpv1[tmpv1$switch==0 & tmpv1$answercorrect==1,'responsetime'])
  fl[i,'v1.RT.sw1CORR'] <- mean(tmpv1[tmpv1$switch==1 & tmpv1$answercorrect==1,'responsetime'])
  fl[i,'DATE.v1'] <- dates[1]
  
  # Visit 2:
  tmpv2 <- tmp[as.Date(tmp$date) == dates[2],]
  fl[i,'NumItPre.v2'] <- dim(tmpv2)[1]
  
  fl[i,'v2.Acc.sw0'] <- mean(tmpv2[tmpv2$switch==0,'answercorrect'])
  fl[i,'v2.RT.sw0'] <- mean(tmpv2[tmpv2$switch==0,'responsetime'])
  fl[i,'v2.Acc.sw1'] <- mean(tmpv2[tmpv2$switch==1,'answercorrect'])
  fl[i,'v2.RT.sw1'] <- mean(tmpv2[tmpv2$switch==1,'responsetime'])
  fl[i,'v2.RT.sw0CORR'] <- mean(tmpv2[tmpv2$switch==0 & tmpv2$answercorrect==1,'responsetime'])
  fl[i,'v2.RT.sw1CORR'] <- mean(tmpv2[tmpv2$switch==1 & tmpv2$answercorrect==1,'responsetime'])
  fl[i,'DATE.v2'] <- dates[2]
  fl[i,'DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  
  if (length(dates)==3){
    # Visit 3:
    tmpv3 <- tmp[as.Date(tmp$date) == dates[3],]
    fl[i,'NumItPre.v3'] <- dim(tmpv3)[1]
    
    fl[i,'v3.Acc.sw0'] <- mean(tmpv3[tmpv3$switch==0,'answercorrect'])
    fl[i,'v3.RT.sw0'] <- mean(tmpv3[tmpv3$switch==0,'responsetime'])
    fl[i,'v3.Acc.sw1'] <- mean(tmpv3[tmpv3$switch==1,'answercorrect'])
    fl[i,'v3.RT.sw1'] <- mean(tmpv3[tmpv3$switch==1,'responsetime'])
    fl[i,'v3.RT.sw0CORR'] <- mean(tmpv3[tmpv3$switch==0 & tmpv3$answercorrect==1,'responsetime'])
    fl[i,'v3.RT.sw1CORR'] <- mean(tmpv3[tmpv3$switch==1 & tmpv3$answercorrect==1,'responsetime'])
    fl[i,'DATE.v3'] <- dates[1]
    fl[i,'DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  }
}
fl[is.na(fl$DATE.v2),c('NumItPre.v2','NumItPost.v2')] <- NA
write.xlsx(fl, paste(dir, 'summary/', 'transfer_numeric_flanker', '.xlsx', sep=''), row.names = F)
rm(fl)



# Numerical Updating:
numupdating <- read.csv2(paste(dir, 'transfer_numerical_updating.csv', sep=''),sep=';')
numupdating <- numupdating[which(nchar(numupdating$id)>3),]
ids <- names(table(numupdating$id))
ids <- names(table(numupdating$id))

nup <- as.data.frame(matrix(NA, length(ids), 21))
colnames(nup) <- c('ID',
                   'v1.CorrCount', 'v1.CorrYN', 'v1.RT','v1.RTCORR',
                   'v2.CorrCount', 'v2.CorrYN', 'v2.RT','v2.RTCORR',
                   'v3.CorrCount', 'v3.CorrYN', 'v3.RT','v3.RTCORR',
                   'DATE.v1', 'DATE.v2', 'DATE.v3','DATE-diff12', 'DATE-diff23',
                   'NumIt.v1','NumIt.v2','NumIt.v3')


for (i in 1:length(ids)) {
  tmp <- subset(numupdating, numupdating$id==ids[i])
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  
  
  nup[i,'ID'] <- ids[i]
  
  # Visit 1:
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  
  nup[i,'NumIt.v1'] <- dim(tmpv1)[1]
  nup[i,'v1.CorrCount'] <- mean(tmpv1$correct_count)
  nup[i,'v1.CorrYN'] <- mean(tmpv1$answer_is_correct)
  nup[i,'v1.RT'] <- mean(tmpv1$response_time)
  nup[i,'v1.RTCORR'] <- mean(tmpv1$response_time[tmpv1$answer_is_correct==1])
  
  # Visit 2:
  tmpv2 <- tmp[as.Date(tmp$date) == dates[2],]
  
  nup[i,'NumIt.v2'] <- dim(tmpv2)[1]
  nup[i,'v2.CorrCount'] <- mean(tmpv2$correct_count)
  nup[i,'v2.CorrYN'] <- mean(tmpv2$answer_is_correct)
  nup[i,'v2.RT'] <- mean(tmpv2$response_time)
  nup[i,'v2.RTCORR'] <- mean(tmpv2$response_time[tmpv2$answer_is_correct==1])
  
  nup[i,'DATE.v1'] <- dates[1]
  nup[i,'DATE.v2'] <- dates[2]
  nup[i,'DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  
  if (length(dates)==3){
    # Visit 3:
    tmpv3 <- tmp[as.Date(tmp$date) == dates[3],]
    
    nup[i,'NumIt.v3'] <- dim(tmpv3)[1]
    nup[i,'v3.CorrCount'] <- mean(tmpv3$correct_count)
    nup[i,'v3.CorrYN'] <- mean(tmpv3$answer_is_correct)
    nup[i,'v3.RT'] <- mean(tmpv3$response_time)
    nup[i,'v3.RTCORR'] <- mean(tmpv3$response_time[tmpv3$answer_is_correct==1])
    nup[i,'DATE.v3'] <- dates[3]
    nup[i,'DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  }
}
nup[is.na(nup$DATE.v2),c('NumIt.v2')] <- NA
write.xlsx(nup, paste(dir, 'summary/', 'transfer_numerical_upd', '.xlsx', sep=''), row.names = F)
rm(nup)


# Transfer Near Updating:
nnumupdating <- read.csv2(paste(dir, 'transfer_near_updating_trial.csv', sep=''),sep=';')
nnumupdating <- nnumupdating[which(nchar(nnumupdating$id)>3),]
ids <- names(table(nnumupdating$id))

nnup <- as.data.frame(matrix(NA, length(ids), 33))


colnames(nnup) <- c('ID',
                    'v1.CorrCount.lvl2', 'v1.CorrYN.lvl2', 'v1.RT.lvl2', 'v1.RT.lvl2CORR','v1.CorrCount.lvl4', 'v1.CorrYN.lvl4', 'v1.RT.lvl4', 'v1.RT.lvl4CORR',
                    'v2.CorrCount.lvl2', 'v2.CorrYN.lvl2', 'v2.RT.lvl2', 'v2.RT.lvl2CORR','v2.CorrCount.lvl4', 'v2.CorrYN.lvl4', 'v2.RT.lvl4', 'v2.RT.lvl4CORR',
                    'v3.CorrCount.lvl2', 'v3.CorrYN.lvl2', 'v3.RT.lvl2', 'v3.RT.lvl2CORR','v3.CorrCount.lvl4', 'v3.CorrYN.lvl4', 'v3.RT.lvl4', 'v3.RT.lvl4CORR',
                    'DATE.v1', 'DATE.v2', 'DATE.v3','DATE-diff12', 'DATE-diff23',
                    'NumIt.v1','NumIt.v2','NumIt.v3')



for (i in 1:length(ids)) {
  
  tmp <- subset(nnumupdating, nnumupdating$id==ids[i])
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  
  nnup[i,'ID'] <- ids[i]
  
  # Visit 1:
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  
  nnup[i,'NumIt.v1'] <- dim(tmpv1)[1]
  nnup[i,'v1.CorrCount.lvl2'] <- mean(tmpv1$switch[tmpv1$level==2])
  nnup[i,'v1.CorrYN.lvl2'] <- mean(tmpv1$answercorrect[tmpv1$level==2])
  nnup[i,'v1.RT.lvl2'] <- mean(tmpv1$responsetime[tmpv1$level==2])
  nnup[i,'v1.RT.lvl2CORR'] <- mean(tmpv1$responsetime[tmpv1$level==2 & tmpv1$answercorrect==1])
  nnup[i,'v1.CorrCount.lvl4'] <- mean(tmpv1$switch[tmpv1$level==4])
  nnup[i,'v1.CorrYN.lvl4'] <- mean(tmpv1$answercorrect[tmpv1$level==4])
  nnup[i,'v1.RT.lvl4'] <- mean(tmpv1$responsetime[tmpv1$level==4])
  nnup[i,'v1.RT.lvl4CORR'] <- mean(tmpv1$responsetime[tmpv1$level==4 & tmpv1$answercorrect==1])
  
  # Visit 2:
  tmpv2 <- tmp[as.Date(tmp$date) == dates[2],]
  
  nnup[i,'NumIt.v2'] <- dim(tmpv2)[1]
  nnup[i,'v2.CorrCount.lvl2'] <- mean(tmpv2$switch[tmpv2$level==2])
  nnup[i,'v2.CorrYN.lvl2'] <- mean(tmpv2$answercorrect[tmpv2$level==2])
  nnup[i,'v2.RT.lvl2'] <- mean(tmpv2$responsetime[tmpv2$level==2])
  nnup[i,'v2.RT.lvl2CORR'] <- mean(tmpv2$responsetime[tmpv2$level==2 & tmpv2$answercorrect==1])
  nnup[i,'v2.CorrCount.lvl4'] <- mean(tmpv2$switch[tmpv2$level==4])
  nnup[i,'v2.CorrYN.lvl4'] <- mean(tmpv2$answercorrect[tmpv2$level==4])
  nnup[i,'v2.RT.lvl4'] <- mean(tmpv2$responsetime[tmpv2$level==4])
  nnup[i,'v2.RT.lvl4CORR'] <- mean(tmpv2$responsetime[tmpv2$level==4 & tmpv2$answercorrect==1])
  
  
  nnup[i,'DATE.v1'] <- dates[1]
  nnup[i,'DATE.v2'] <- dates[2]
  nnup[i,'DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  
  if (length(dates)==3){
    # Visit 3:
    tmpv3 <- tmp[as.Date(tmp$date) == dates[3],]
    
    nnup[i,'NumIt.v3'] <- dim(tmpv3)[1]
    nnup[i,'v3.CorrCount.lvl2'] <- mean(tmpv3$switch[tmpv3$level==2])
    nnup[i,'v3.CorrYN.lvl2'] <- mean(tmpv3$answercorrect[tmpv3$level==2])
    nnup[i,'v3.RT.lvl2'] <- mean(tmpv3$responsetime[tmpv3$level==2])
    nnup[i,'v3.RT.lvl2CORR'] <- mean(tmpv3$responsetime[tmpv3$level==2 & tmpv3$answercorrect==1])
    nnup[i,'v3.CorrCount.lvl4'] <- mean(tmpv3$switch[tmpv3$level==4])
    nnup[i,'v3.CorrYN.lvl4'] <- mean(tmpv3$answercorrect[tmpv3$level==4])
    nnup[i,'v3.RT.lvl4'] <- mean(tmpv3$responsetime[tmpv3$level==4])
    nnup[i,'v3.RT.lvl4CORR'] <- mean(tmpv3$responsetime[tmpv3$level==4 & tmpv3$answercorrect==1])
    nnup[i,'DATE.v3'] <- dates[3]
    nnup[i,'DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  }
}
nnup[is.na(nnup$DATE.v2),c('NumIt.v2')] <- NA
write.xlsx(nnup, paste(dir, 'summary/', 'transfer_near_upd', '.xlsx', sep=''), row.names = F)
rm(nnup)


# Spatial Flanker:
# [!!! IMPLEMENT SWITCH FREQUENCY !!!!]
sflanker <- read.csv2(paste(dir, 'transfer_spatial_flanker_task_trial.csv', sep=''),sep=';')
sflanker <- sflanker[which(nchar(sflanker$id)>3),]
ids <- names(table(sflanker$id))
ids <- names(table(sflanker$id))

sfl <- as.data.frame(matrix(NA, length(ids), 30))


colnames(sfl) <- c('ID',
                   'v1.Acc.sw0', 'v1.RT.sw0', 'v1.Acc.sw1', 'v1.RT.sw1', 
                   'v1.RT.sw0CORR', 'v1.RT.sw1CORR',
                   'v2.Acc.sw0', 'v2.RT.sw0', 'v2.Acc.sw1', 'v2.RT.sw1', 
                   'v2.RT.sw0CORR', 'v2.RT.sw1CORR',
                   'v3.Acc.sw0', 'v3.RT.sw0', 'v3.Acc.sw1', 'v3.RT.sw1', 
                   'v3.RT.sw0CORR', 'v3.RT.sw1CORR',
                   'DATE.v1', 'DATE.v2', 'DATE.v3','DATE-diff12', 'DATE-diff23',
                   'NumItPre.v1','NumItPost.v1','NumItPre.v2','NumItPost.v2','NumItPre.v3','NumItPost.v3')



for (i in 1:length(ids)) {
  tmp <- subset(sflanker, sflanker$id==ids[i])
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  
  sfl[i,'ID'] <- ids[i]
  
  # Visit 1:
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  sfl[i,'NumItPre.v1'] <- dim(tmpv1)[1]
  
  sfl[i,'v1.Acc.sw0'] <- mean(tmpv1[tmpv1$switch==0,'answercorrect'])
  sfl[i,'v1.RT.sw0'] <- mean(tmpv1[tmpv1$switch==0,'responsetime'])
  sfl[i,'v1.Acc.sw1'] <- mean(tmpv1[tmpv1$switch==1,'answercorrect'])
  sfl[i,'v1.RT.sw1'] <- mean(tmpv1[tmpv1$switch==1,'responsetime'])
  sfl[i,'v1.RT.sw0CORR'] <- mean(tmpv1[tmpv1$switch==0 & tmpv1$answercorrect==1,'responsetime'])
  sfl[i,'v1.RT.sw1CORR'] <- mean(tmpv1[tmpv1$switch==1 & tmpv1$answercorrect==1,'responsetime'])
  sfl[i,'DATE.v1'] <- dates[1]
  
  # Visit 2:
  tmpv2 <- tmp[as.Date(tmp$date) == dates[2],]
  sfl[i,'NumItPre.v2'] <- dim(tmpv2)[1]
  
  sfl[i,'v2.Acc.sw0'] <- mean(tmpv2[tmpv2$switch==0,'answercorrect'])
  sfl[i,'v2.RT.sw0'] <- mean(tmpv2[tmpv2$switch==0,'responsetime'])
  sfl[i,'v2.Acc.sw1'] <- mean(tmpv2[tmpv2$switch==1,'answercorrect'])
  sfl[i,'v2.RT.sw1'] <- mean(tmpv2[tmpv2$switch==1,'responsetime'])
  sfl[i,'v2.RT.sw0CORR'] <- mean(tmpv2[tmpv2$switch==0 & tmpv2$answercorrect==1,'responsetime'])
  sfl[i,'v2.RT.sw1CORR'] <- mean(tmpv2[tmpv2$switch==1 & tmpv2$answercorrect==1,'responsetime'])
  sfl[i,'DATE.v2'] <- dates[2]
  sfl[i,'DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  
  if (length(dates)==3){
    # Visit 3:
    tmpv3 <- tmp[as.Date(tmp$date) == dates[3],]
    sfl[i,'NumItPre.v3'] <- dim(tmpv3)[1]
    
    sfl[i,'v3.Acc.sw0'] <- mean(tmpv3[tmpv3$switch==0,'answercorrect'])
    sfl[i,'v3.RT.sw0'] <- mean(tmpv3[tmpv3$switch==0,'responsetime'])
    sfl[i,'v3.Acc.sw1'] <- mean(tmpv3[tmpv3$switch==1,'answercorrect'])
    sfl[i,'v3.RT.sw1'] <- mean(tmpv3[tmpv3$switch==1,'responsetime'])
    sfl[i,'v3.RT.sw0CORR'] <- mean(tmpv3[tmpv3$switch==0 & tmpv3$answercorrect==1,'responsetime'])
    sfl[i,'v3.RT.sw1CORR'] <- mean(tmpv3[tmpv3$switch==1 & tmpv3$answercorrect==1,'responsetime'])
    sfl[i,'DATE.v3'] <- dates[1]
    sfl[i,'DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  }
}
sfl[is.na(sfl$DATE.v2),c('NumItPre.v2','NumItPost.v2')] <- NA
write.xlsx(sfl, paste(dir, 'summary/', 'transfer_spatial_flanker', '.xlsx', sep=''), row.names = F)
rm(sfl)


# Spatial Recall:
srecall <- read.csv2(paste(dir, 'transfer_spatial_recall.csv', sep=''),sep=';')
srecall <- srecall[which(nchar(srecall$id)>3),]
ids <- names(table(srecall$id))

src <- as.data.frame(matrix(NA, length(ids), 12))


colnames(src) <- c('ID',
                   'v1.Acc', 'v2.Acc', 'v3.Acc',
                   'DATE.v1', 'DATE.v2', 'DATE.v3','DATE-diff12', 'DATE-diff23',
                   'NumIt.v1','NumIt.v2','NumIt.v3')


for (i in 1:length(ids)) {
  
  tmp <- subset(srecall, srecall$id==ids[i])
  tmp <- subset(srecall, srecall$id==ids[i])
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  
  src[i,'ID'] <- ids[i]
  
  # Visit 1:
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  
  src[i,'NumIt.v1'] <- dim(tmpv1)[1]
  src[i,'v1.Acc'] <- sum(tmpv1$answer_is_correct)
  src[i,'DATE.v1'] <- dates[1]
  
  # Visit 2:
  tmpv2 <- tmp[as.Date(tmp$date) == dates[2],]
  src[i,'NumIt.v2'] <- dim(tmpv2)[1]
  src[i,'v2.Acc'] <- sum(tmpv2$answer_is_correct)
  src[i,'DATE.v2'] <- dates[2]
  
  src[i,'DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  
  if (length(dates)==3){
    # Visit 3:
    tmpv3 <- tmp[as.Date(tmp$date) == dates[3],]
    src[i,'NumIt.v3'] <- dim(tmpv3)[1]
    src[i,'v3.Acc'] <- sum(tmpv3$answer_is_correct)
    src[i,'DATE.v3'] <- dates[3]
    src[i,'DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  }
}
src[is.na(src$DATE.v2),c('NumIt.v2')] <- NA
write.xlsx(src, paste(dir, 'summary/', 'transfer_spatial_recall', '.xlsx', sep=''), row.names = F)
rm(src)

# Spatial Updating:
supdating <- read.csv2(paste(dir, 'transfer_spatial_updating.csv', sep=''),sep=';')
supdating <- supdating[which(nchar(supdating$id)>3),]
ids <- names(table(supdating$id))


sup <- as.data.frame(matrix(NA, length(ids), 21))


colnames(sup) <- c('ID',
                   'v1.CorrCount', 'v1.CorrYN', 'v1.RT','v1.RTCORR',
                   'v2.CorrCount', 'v2.CorrYN', 'v2.RT','v2.RTCORR',
                   'v3.CorrCount', 'v3.CorrYN', 'v3.RT','v3.RTCORR',
                   'DATE.v1', 'DATE.v2', 'DATE.v3','DATE-diff12', 'DATE-diff23',
                   'NumIt.v1','NumIt.v2','NumIt.v3')


for (i in 1:length(ids)) {
  tmp <- subset(supdating, supdating$id==ids[i])
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  
  
  sup[i,'ID'] <- ids[i]
  
  # Visit 1:
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  
  sup[i,'NumIt.v1'] <- dim(tmpv1)[1]
  sup[i,'v1.CorrCount'] <- mean(tmpv1$correct_count)
  sup[i,'v1.CorrYN'] <- mean(tmpv1$answer_is_correct)
  sup[i,'v1.RT'] <- mean(tmpv1$response_time)
  sup[i,'v1.RTCORR'] <- mean(tmpv1$response_time[tmpv1$answer_is_correct==1])
  
  # Visit 2:
  tmpv2 <- tmp[as.Date(tmp$date) == dates[2],]
  
  sup[i,'NumIt.v2'] <- dim(tmpv2)[1]
  sup[i,'v2.CorrCount'] <- mean(tmpv2$correct_count)
  sup[i,'v2.CorrYN'] <- mean(tmpv2$answer_is_correct)
  sup[i,'v2.RT'] <- mean(tmpv2$response_time)
  sup[i,'v2.RTCORR'] <- mean(tmpv2$response_time[tmpv2$answer_is_correct==1])
  
  sup[i,'DATE.v1'] <- dates[1]
  sup[i,'DATE.v2'] <- dates[2]
  sup[i,'DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  
  if (length(dates)==3){
    # Visit 3:
    tmpv3 <- tmp[as.Date(tmp$date) == dates[3],]
    
    sup[i,'NumIt.v3'] <- dim(tmpv3)[1]
    sup[i,'v3.CorrCount'] <- mean(tmpv3$correct_count)
    sup[i,'v3.CorrYN'] <- mean(tmpv3$answer_is_correct)
    sup[i,'v3.RT'] <- mean(tmpv3$response_time)
    sup[i,'v3.RTCORR'] <- mean(tmpv3$response_time[tmpv3$answer_is_correct==1])
    sup[i,'DATE.v3'] <- dates[3]
    sup[i,'DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  }
}
sup[is.na(sup$DATE.v2),c('NumIt.v2')] <- NA
write.xlsx(sup, paste(dir, 'summary/', 'transfer_spatial_updating', '.xlsx', sep=''), row.names = F)
rm(sup)


# Trained N-back:
tnback <- read.csv2(paste(dir, 'transfer_trained_nback_trial.csv', sep=''),sep=';')
tnback <- tnback[which(nchar(tnback$id)>3),]
tnback2 <- subset(tnback, is.element(tnback$level, 2:5))
tnback3 <- subset(tnback, is.element(tnback$level, 7:10))
ids <- names(table(tnback2$id))

tnb2 <- as.data.frame(matrix(NA, length(ids), 22))
tnb3 <- tnb2
colnames(tnb2) <- paste('tnb2.',c('ID',
                                  'TP.v1', 'TN.v1', 'SimpAcc.v1','OA.v1','RT.v1',
                                  'TP.v2', 'TN.v2', 'SimpAcc.v2','OA.v2','RT.v2',
                                  'TP.v3', 'TN.v3', 'SimpAcc.v3','OA.v3','RT.v3',
                                  'DATE.v1', 'DATE.v2', 'DATE.v3',
                                  'DATE-diff12', 'DATE-diff23','NumIt'), sep='')


colnames(tnb3) <- paste('tnb3.',c('ID',
                                  'TP.v1', 'TN.v1', 'SimpAcc.v1','OA.v1','RT.v1',
                                  'TP.v2', 'TN.v2', 'SimpAcc.v2','OA.v2','RT.v2',
                                  'TP.v3', 'TN.v3', 'SimpAcc.v3','OA.v3','RT.v3',
                                  'DATE.v1', 'DATE.v2', 'DATE.v3',
                                  'DATE-diff12', 'DATE-diff23', 'NumIt'), sep='')                               


for (i in 1:length(ids)) {
  
  # 2-back:
  tmp2 <- subset(tnback2, tnback2$id==ids[i])
  dates <- as.Date(names(table(as.Date(tmp2$date)))) # extract dates for each subject
  
  tmp2v1 <- tmp2[as.Date(tmp2$date) == dates[1],]
  tmp2v2 <- tmp2[as.Date(tmp2$date) == dates[2],]
  
  tmp3 <- subset(tnback3, tnback3$id==ids[i])
  tmp3v1 <- tmp3[as.Date(tmp3$date) == dates[1],]
  tmp3v2 <- tmp3[as.Date(tmp3$date) == dates[2],]
  
  
  tnb2[i,'tnb2.ID'] <- ids[i]
  tnb2[i,'tnb2.TP.v1'] <- mean(tmp2v1[tmp2v1$switch==1,'answercorrect']) #tp
  tnb2[i,'tnb2.TN.v1'] <- mean(tmp2v1[tmp2v1$switch==0,'answercorrect']) #tn
  tnb2[i,'tnb2.SimpAcc.v1'] <- mean(tmp2v1$answercorrect)
  tnb2[i,'tnb2.OA.v1'] <- (tnb2[i,'tnb2.TP.v1']+tnb2[i,'tnb2.TN.v1'])/2
  tnb2[i,'tnb2.RT.v1'] <- mean(tmp2v1$responsetime[tmp2v1$responsetime>-1 & tmp2v1$switch==1])
  tnb2[i,'tnb2.TP.v2'] <- mean(tmp2v2[tmp2v2$switch==1,'answercorrect']) #tp
  tnb2[i,'tnb2.TN.v2'] <- mean(tmp2v2[tmp2v2$switch==0,'answercorrect']) #tn
  tnb2[i,'tnb2.SimpAcc.v2'] <- mean(tmp2v2$answercorrect)
  tnb2[i,'tnb2.OA.v2'] <- (tnb2[i,'tnb2.TP.v2']+tnb2[i,'tnb2.TN.v2'])/2
  tnb2[i,'tnb2.RT.v2'] <- mean(tmp2v2$responsetime[tmp2v2$responsetime>-1 & tmp2v2$switch==1])
  
  tnb2[i,'tnb2.DATE.v1'] <- dates[1]
  tnb2[i,'tnb2.DATE.v2'] <- dates[2]
  
  if (length(dates)==3){
    tmp2v3 <- tmp2[as.Date(tmp2$date) == dates[3],]
    tnb2[i,'tnb2.TP.v3'] <- mean(tmp2v3[tmp2v3$switch==1,'answercorrect']) #tp
    tnb2[i,'tnb2.TN.v3'] <- mean(tmp2v3[tmp2v3$switch==0,'answercorrect']) #tn
    tnb2[i,'tnb2.SimpAcc.v3'] <- mean(tmp2v3$answercorrect)
    tnb2[i,'tnb2.OA.v3'] <- (tnb2[i,'tnb2.TP.v3']+tnb2[i,'tnb2.TN.v3'])/2
    tnb2[i,'tnb2.RT.v3'] <- mean(tmp2v3$responsetime[tmp2v3$responsetime>-1 & tmp2v3$switch==1])
    tnb2[i,'tnb2.DATE.v3'] <- dates[3]
  }
  else {
    ## do nothing
  }
  
  tnb2[i,'tnb2.DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  tnb2[i,'tnb2.DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  tnb2[i,'tnb2.NumIt'] <- dim(tmp2)[1]
  
  tnb3[i,'tnb3.ID'] <- ids[i]
  tnb3[i,'tnb3.TP.v1'] <- mean(tmp3v1[tmp3v1$switch==1,'answercorrect']) #tp
  tnb3[i,'tnb3.TN.v1'] <- mean(tmp3v1[tmp3v1$switch==0,'answercorrect']) #tn
  tnb3[i,'tnb3.SimpAcc.v1'] <- mean(tmp3v1$answercorrect)
  tnb3[i,'tnb3.OA.v1'] <- (tnb3[i,'tnb3.TP.v1']+tnb3[i,'tnb3.TN.v1'])/2
  tnb3[i,'tnb3.RT.v1'] <- mean(tmp3v1$responsetime[tmp3v1$responsetime>-1 & tmp3v1$switch==1])
  tnb3[i,'tnb3.TP.v2'] <- mean(tmp3v2[tmp3v2$switch==1,'answercorrect']) #tp
  tnb3[i,'tnb3.TN.v2'] <- mean(tmp3v2[tmp3v2$switch==0,'answercorrect']) #tn
  tnb3[i,'tnb3.SimpAcc.v2'] <- mean(tmp3v2$answercorrect)
  tnb3[i,'tnb3.OA.v2'] <- (tnb3[i,'tnb3.TP.v2']+tnb3[i,'tnb3.TN.v2'])/2
  tnb3[i,'tnb3.RT.v2'] <- mean(tmp3v2$responsetime[tmp3v2$responsetime>-1 & tmp3v2$switch==1])
  
  tnb3[i,'tnb3.DATE.v1'] <- dates[1]
  tnb3[i,'tnb3.DATE.v2'] <- dates[2]
  
  if (length(dates)==3){
    tmp3v3 <- tmp3[as.Date(tmp3$date) == dates[3],]
    tnb3[i,'tnb3.TP.v3'] <- mean(tmp3v3[tmp3v3$switch==1,'answercorrect']) #tp
    tnb3[i,'tnb3.TN.v3'] <- mean(tmp3v3[tmp3v3$switch==0,'answercorrect']) #tn
    tnb3[i,'tnb3.SimpAcc.v3'] <- mean(tmp3v3$answercorrect)
    tnb3[i,'tnb3.OA.v3'] <- (tnb3[i,'tnb3.TP.v3']+tnb3[i,'tnb3.TN.v3'])/2
    tnb3[i,'tnb3.RT.v3'] <- mean(tmp3v3$responsetime[tmp3v3$responsetime>-1 & tmp3v3$switch==1])
    tnb3[i,'tnb3.DATE.v3'] <- dates[3]
  }
  
  tnb3[i,'tnb3.DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  tnb3[i,'tnb3.DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  tnb3[i,'tnb3.NumIt'] <- dim(tmp3)[1]
}

write.xlsx(tnb2, paste(dir, 'summary/', 'transfer_trained_2back', '.xlsx', sep=''), row.names = F)
write.xlsx(tnb3, paste(dir, 'summary/', 'transfer_trained_3back', '.xlsx', sep=''), row.names = F)
rm(tnb2, tnb3)



# Trained Perceptual Matching
tpercmatch <- read.csv2(paste(dir, 'transfer_trained_perceptual_matching_trial.csv', sep=''),sep=';')
tpercmatch <- tpercmatch[which(nchar(tpercmatch$id)>3),]
ids <- names(table(tpercmatch$id))

tpmtch <- as.data.frame(matrix(NA, length(ids), 30))


colnames(tpmtch) <- c('ID',
                      'v1.Acc.sw0', 'v1.RT.sw0', 'v1.Acc.sw1', 'v1.RT.sw1', 
                      'v1.RT.sw0CORR', 'v1.RT.sw1CORR',
                      'v2.Acc.sw0', 'v2.RT.sw0', 'v2.Acc.sw1', 'v2.RT.sw1', 
                      'v2.RT.sw0CORR', 'v2.RT.sw1CORR',
                      'v3.Acc.sw0', 'v3.RT.sw0', 'v3.Acc.sw1', 'v3.RT.sw1', 
                      'v3.RT.sw0CORR', 'v3.RT.sw1CORR',
                      'DATE.v1', 'DATE.v2', 'DATE.v3','DATE-diff12', 'DATE-diff23',
                      'NumItPre.v1','NumItPost.v1','NumItPre.v2','NumItPost.v2','NumItPre.v3','NumItPost.v3')

for (i in 1:length(ids)) {
  
  tmp <- subset(tpercmatch, tpercmatch$id==ids[i])
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  
  tpmtch[i,'ID'] <- ids[i]
  
  
  # Visit 1:
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  tpmtch[i,'NumItPre.v1'] <- dim(tmpv1)[1]
  
  tpmtch[i,'v1.Acc.sw0'] <- mean(tmpv1[tmpv1$switch==0,'answercorrect'])
  tpmtch[i,'v1.RT.sw0'] <- mean(tmpv1[tmpv1$switch==0,'responsetime'])
  tpmtch[i,'v1.Acc.sw1'] <- mean(tmpv1[tmpv1$switch==1,'answercorrect'])
  tpmtch[i,'v1.RT.sw1'] <- mean(tmpv1[tmpv1$switch==1,'responsetime'])
  tpmtch[i,'v1.RT.sw0CORR'] <- mean(tmpv1[tmpv1$switch==0 & tmpv1$answercorrect==1,'responsetime'])
  tpmtch[i,'v1.RT.sw1CORR'] <- mean(tmpv1[tmpv1$switch==1 & tmpv1$answercorrect==1,'responsetime'])
  tpmtch[i,'DATE.v1'] <- dates[1]
  
  # Visit 2:
  tmpv2 <- tmp[as.Date(tmp$date) == dates[2],]
  tpmtch[i,'NumItPre.v2'] <- dim(tmpv2)[1]
  
  tpmtch[i,'v2.Acc.sw0'] <- mean(tmpv2[tmpv2$switch==0,'answercorrect'])
  tpmtch[i,'v2.RT.sw0'] <- mean(tmpv2[tmpv2$switch==0,'responsetime'])
  tpmtch[i,'v2.Acc.sw1'] <- mean(tmpv2[tmpv2$switch==1,'answercorrect'])
  tpmtch[i,'v2.RT.sw1'] <- mean(tmpv2[tmpv2$switch==1,'responsetime'])
  tpmtch[i,'v2.RT.sw0CORR'] <- mean(tmpv2[tmpv2$switch==0 & tmpv2$answercorrect==1,'responsetime'])
  tpmtch[i,'v2.RT.sw1CORR'] <- mean(tmpv2[tmpv2$switch==1 & tmpv2$answercorrect==1,'responsetime'])
  tpmtch[i,'DATE.v2'] <- dates[2]
  
  tpmtch[i,'DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  
  if (length(dates)==3){
    # Visit 3:
    tmpv3 <- tmp[as.Date(tmp$date) == dates[3],]
    tpmtch[i,'NumItPre.v3'] <- dim(tmpv3)[1]
    
    tpmtch[i,'v3.Acc.sw0'] <- mean(tmpv3[tmpv3$switch==0,'answercorrect'])
    tpmtch[i,'v3.RT.sw0'] <- mean(tmpv3[tmpv3$switch==0,'responsetime'])
    tpmtch[i,'v3.Acc.sw1'] <- mean(tmpv3[tmpv3$switch==1,'answercorrect'])
    tpmtch[i,'v3.RT.sw1'] <- mean(tmpv3[tmpv3$switch==1,'responsetime'])
    tpmtch[i,'v3.RT.sw0CORR'] <- mean(tmpv3[tmpv3$switch==0 & tmpv3$answercorrect==1,'responsetime'])
    tpmtch[i,'v3.RT.sw1CORR'] <- mean(tmpv3[tmpv3$switch==1 & tmpv3$answercorrect==1,'responsetime'])
    tpmtch[i,'DATE.v3'] <- dates[1]
    tpmtch[i,'DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  }
}
tpmtch[is.na(tpmtch$DATE.v2),c('NumItPre.v2','NumItPost.v2')] <- NA
write.xlsx(tpmtch, paste(dir, 'summary/', 'transfer_trained_perceptual_matching', '.xlsx', sep=''), row.names = F)
rm(tpmtch)


# Trained Rule-switching:
ttrswitch <- read.csv2(paste(dir, 'transfer_trained_ruleswitching_trial.csv', sep=''),sep=';')
ttrswitch <- ttrswitch[which(nchar(ttrswitch$id)>3),]
ids <- names(table(ttrswitch$id))



ttrsw <- as.data.frame(matrix(NA, length(ids), 39))

colnames(ttrsw) <- c('ID',
                     'v1.Acc.sw', 'v1.RT.sw', 'v1.Acc.ps', 'v1.RT.ps', 'v1.Acc.ns', 'v1.RT.ns',
                     'v1.RT.swCORR', 'v1.RT.psCORR', 'v1.RT.nsCORR',
                     'v2.Acc.sw', 'v2.RT.sw', 'v2.Acc.ps', 'v2.RT.ps', 'v2.Acc.ns', 'v2.RT.ns',
                     'v2.RT.swCORR', 'v2.RT.psCORR','v2.RT.nsCORR',
                     'v3.Acc.sw', 'v3.RT.sw', 'v3.Acc.ps', 'v3.RT.ps', 'v3.Acc.ns', 'v3.RT.ns',
                     'v3.RT.swCORR', 'v3.RT.psCORR', 'v3.RT.nsCORR',
                     'DATE.v1', 'DATE.v2', 'DATE.v3','DATE-diff12', 'DATE-diff23',
                     'NumItPre.v1','NumItPost.v1','NumItPre.v2','NumItPost.v2','NumItPre.v3','NumItPost.v3')



for (i in 1:length(ids)) {
  # Switch 3: non-switch
  # Switch 1: switch
  # Switch 2: post-switch
  tmp <- subset(ttrswitch, ttrswitch$id==ids[i])
  
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  tmpv2 <- tmp[as.Date(tmp$date) == dates[2],]
  ttrsw[i,'NumItPre.v1'] <- dim(tmpv1)[1]
  ttrsw[i,'NumItPre.v2'] <- dim(tmpv2)[1]
  
  ttrsw[i,'ID'] <- ids[i]
  ttrsw[i,'v1.Acc.sw'] <- mean(tmpv1[tmpv1$switch==1,'answercorrect'])
  ttrsw[i,'v1.RT.sw'] <- mean(tmpv1[tmpv1$switch==1,'responsetime'])
  ttrsw[i,'v1.Acc.ps'] <- mean(tmpv1[tmpv1$switch==2,'answercorrect'])
  ttrsw[i,'v1.RT.ps'] <- mean(tmpv1[tmpv1$switch==2,'responsetime'])
  ttrsw[i,'v1.Acc.ns'] <- mean(tmpv1[tmpv1$switch==3,'answercorrect'])
  ttrsw[i,'v1.RT.ns'] <- mean(tmpv1[tmpv1$switch==3,'responsetime'])
  ttrsw[i,'v1.RT.swCORR'] <- mean(tmpv1[tmpv1$switch==1 & tmpv1$answercorrect==1,'responsetime'])
  ttrsw[i,'v1.RT.psCORR'] <- mean(tmpv1[tmpv1$switch==2 & tmpv1$answercorrect==1,'responsetime'])
  ttrsw[i,'v1.RT.nsCORR'] <- mean(tmpv1[tmpv1$switch==3 & tmpv1$answercorrect==1,'responsetime'])
  
  ttrsw[i,'v2.Acc.sw'] <- mean(tmpv2[tmpv2$switch==1,'answercorrect'])
  ttrsw[i,'v2.RT.sw'] <- mean(tmpv2[tmpv2$switch==1,'responsetime'])
  ttrsw[i,'v2.Acc.ps'] <- mean(tmpv2[tmpv2$switch==2,'answercorrect'])
  ttrsw[i,'v2.RT.ps'] <- mean(tmpv2[tmpv2$switch==2,'responsetime'])
  ttrsw[i,'v2.Acc.ns'] <- mean(tmpv2[tmpv2$switch==3,'answercorrect'])
  ttrsw[i,'v2.RT.ns'] <- mean(tmpv2[tmpv2$switch==3,'responsetime'])
  ttrsw[i,'v2.RT.swCORR'] <- mean(tmpv2[tmpv2$switch==1 & tmpv2$answercorrect==1,'responsetime'])
  ttrsw[i,'v2.RT.psCORR'] <- mean(tmpv2[tmpv2$switch==2 & tmpv2$answercorrect==1,'responsetime'])
  ttrsw[i,'v2.RT.nsCORR'] <- mean(tmpv2[tmpv2$switch==3 & tmpv2$answercorrect==1,'responsetime'])
  
  ttrsw[i,'DATE.v1'] <- dates[1]
  ttrsw[i,'DATE.v2'] <- dates[2]
  ttrsw[i,'DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  
  if (length(dates)==3){
    tmpv3 <- tmp[as.Date(tmp$date) == dates[3],]
    ttrsw[i,'NumItPre.v3'] <- dim(tmpv3)[1]
    
    ttrsw[i,'v3.Acc.sw'] <- mean(tmpv3[tmpv3$switch==1,'answercorrect'])
    ttrsw[i,'v3.RT.sw'] <- mean(tmpv3[tmpv3$switch==1,'responsetime'])
    ttrsw[i,'v3.Acc.ps'] <- mean(tmpv3[tmpv3$switch==2,'answercorrect'])
    ttrsw[i,'v3.RT.ps'] <- mean(tmpv3[tmpv3$switch==2,'responsetime'])
    ttrsw[i,'v3.Acc.ns'] <- mean(tmpv3[tmpv3$switch==3,'answercorrect'])
    ttrsw[i,'v3.RT.ns'] <- mean(tmpv3[tmpv3$switch==3,'responsetime'])
    ttrsw[i,'v3.RT.swCORR'] <- mean(tmpv3[tmpv3$switch==1 & tmpv3$answercorrect==1,'responsetime'])
    ttrsw[i,'v3.RT.psCORR'] <- mean(tmpv3[tmpv3$switch==2 & tmpv3$answercorrect==1,'responsetime'])
    ttrsw[i,'v3.RT.nsCORR'] <- mean(tmpv3[tmpv3$switch==3 & tmpv3$answercorrect==1,'responsetime'])
    ttrsw[i,'DATE.v3'] <- dates[3]
    ttrsw[i,'DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  }
}
ttrsw[is.na(ttrsw$DATE.v2),c('NumItPre.v2','NumItPost.v2')] <- NA
write.xlsx(ttrsw, paste(dir, 'summary/', 'transfer_trained_ruleswitching_trial', '.xlsx', sep=''), row.names = F)
rm(ttrsw)

# Trained Task-Switching:
ttswitch <- read.csv2(paste(dir, 'transfer_trained_taskswitching_trial.csv', sep=''),sep=';')
ttswitch <- ttswitch[which(nchar(ttswitch$id)>3),]
ids <- names(table(ttswitch$id))
ttsw <- as.data.frame(matrix(NA, length(ids), 27))


colnames(ttsw) <- c('ID', 
                    'v1.Acc.sw0.lvl23', 'v1.RT.sw0.lvl23',
                    'v1.Acc.sw1.lvl23', 'v1.RT.sw1.lvl23', 
                    'v2.Acc.sw0.lvl456', 'v2.RT.sw0.lvl456', 
                    'v2.Acc.sw1.lvl456', 'v2.RT.sw1.lvl456',
                    'v3.Acc.sw0.lv789', 'v3.RT.sw0.lvl789', 
                    'v3.Acc.sw1.lv789', 'v3.RT.sw1.lvl789',
                    'v1.trainedTSW.lvl23.cost','v2.trainedTSW.lvl23.cost','v3.trainedTSW.lvl23.cost',
                    'DATE.v0.lvl23', 'DATE.v2', 'DATE.v3','DATE-diff12', 'DATE-diff23',
                    'NumItPre.v0.lvl23','NumItPost.v0.lvl23','NumItPre.v2','NumItPost.v2','NumItPre.v3','NumItPost.v3')


for (i in 1:length(ids)) {
  
  tmp <- subset(ttswitch, ttswitch$id==ids[i])
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  tmpv2 <- tmp[as.Date(tmp$date) == dates[2],]
  ttsw[i,'NumItPre.v0.lvl23'] <- dim(tmpv1)[1]
  ttsw[i,'NumItPre.v2'] <- dim(tmpv2)[1]
  
  ttsw[i,'ID'] <- ids[i]
  
  # Visit 1:
  ttsw[i,'v1.Acc.sw0.lvl23'] <- mean(tmpv1[tmpv1$switch==0 & (tmpv1$level==2 | tmpv1$level==3),'answercorrect'])
  ttsw[i,'v1.RT.sw0.lvl23'] <- mean(tmpv1[tmpv1$switch==0 & (tmpv1$level==2 | tmpv1$level==3),'responsetime'])
  ttsw[i,'v1.Acc.sw1.lvl23'] <- mean(tmpv1[tmpv1$switch==1 & (tmpv1$level==2 | tmpv1$level==3),'answercorrect'])
  ttsw[i,'v1.RT.sw1.lvl23'] <- mean(tmpv1[tmpv1$switch==1 & (tmpv1$level==2 | tmpv1$level==3),'responsetime'])
  ttsw[i,'v1.trainedTSW.lvl23.cost'] <- c(ttsw[i,'v1.RT.sw1.lvl23']-ttsw[i,'v1.RT.sw0.lvl23'])
  
  ttsw[i,'v1.Acc.sw0.lvl456'] <- mean(tmpv1[tmpv1$switch==0 & (tmpv1$level==4 | tmpv1$level==5 | tmpv1$level==6),'answercorrect'])
  ttsw[i,'v1.RT.sw0.lvl456'] <- mean(tmpv1[tmpv1$switch==0 & (tmpv1$level==4 | tmpv1$level==5 | tmpv1$level==6),'responsetime'])
  ttsw[i,'v1.Acc.sw1.lvl456'] <- mean(tmpv1[tmpv1$switch==1 & (tmpv1$level==4 | tmpv1$level==5 | tmpv1$level==6),'answercorrect'])
  ttsw[i,'v1.RT.sw1.lvl456'] <- mean(tmpv1[tmpv1$switch==1 & (tmpv1$level==4 | tmpv1$level==5 | tmpv1$level==6),'responsetime'])
  ttsw[i,'v1.trainedTSW.lvl456.cost'] <- ttsw[i,'v1.RT.sw1.lvl456']-ttsw[i,'v1.RT.sw0.lvl456']
  
  ttsw[i,'v1.Acc.sw0.lvl789'] <- mean(tmpv1[tmpv1$switch==0 & (tmpv1$level==7 | tmpv1$level==8 | tmpv1$level==9),'answercorrect'])
  ttsw[i,'v1.RT.sw0.lvl789'] <- mean(tmpv1[tmpv1$switch==0 & (tmpv1$level==7 | tmpv1$level==8 | tmpv1$level==9),'responsetime'])
  ttsw[i,'v1.Acc.sw1.lvl789'] <- mean(tmpv1[tmpv1$switch==1 & (tmpv1$level==7 | tmpv1$level==8 | tmpv1$level==9),'answercorrect'])
  ttsw[i,'v1.RT.sw1.lvl789'] <- mean(tmpv1[tmpv1$switch==1 & (tmpv1$level==7 | tmpv1$level==8 | tmpv1$level==9),'responsetime'])
  ttsw[i,'v1.trainedTSW.lvl789.cost'] <- ttsw[i,'v1.RT.sw1.lvl789']-ttsw[i,'v1.RT.sw0.lvl789']
  
  # Visit 2:
  ttsw[i,'v2.Acc.sw0.lvl23'] <- mean(tmpv2[tmpv2$switch==0 & (tmpv2$level==2 | tmpv2$level==3),'answercorrect'])
  ttsw[i,'v2.RT.sw0.lvl23'] <- mean(tmpv2[tmpv2$switch==0 & (tmpv2$level==2 | tmpv2$level==3),'responsetime'])
  ttsw[i,'v2.Acc.sw1.lvl23'] <- mean(tmpv2[tmpv2$switch==1 & (tmpv2$level==2 | tmpv2$level==3),'answercorrect'])
  ttsw[i,'v2.RT.sw1.lvl23'] <- mean(tmpv2[tmpv2$switch==1 & (tmpv2$level==2 | tmpv2$level==3),'responsetime'])
  ttsw[i,'v2.trainedTSW.lvl23.cost'] <- ttsw[i,'v2.RT.sw1.lvl23']-ttsw[i,'v2.RT.sw0.lvl23']
  
  ttsw[i,'v2.Acc.sw0.lvl456'] <- mean(tmpv2[tmpv2$switch==0 & (tmpv2$level==4 | tmpv2$level==5 | tmpv2$level==6),'answercorrect'])
  ttsw[i,'v2.RT.sw0.lvl456'] <- mean(tmpv2[tmpv2$switch==0 & (tmpv2$level==4 | tmpv2$level==5 | tmpv2$level==6),'responsetime'])
  ttsw[i,'v2.Acc.sw1.lvl456'] <- mean(tmpv2[tmpv2$switch==1 & (tmpv2$level==4 | tmpv2$level==5 | tmpv2$level==6),'answercorrect'])
  ttsw[i,'v2.RT.sw1.lvl456'] <- mean(tmpv2[tmpv2$switch==1 & (tmpv2$level==4 | tmpv2$level==5 | tmpv2$level==6),'responsetime'])
  ttsw[i,'v2.trainedTSW.lvl456.cost'] <- ttsw[i,'v2.RT.sw1.lvl456']-ttsw[i,'v2.RT.sw0.lvl456']
  
  ttsw[i,'v2.Acc.sw0.lvl789'] <- mean(tmpv2[tmpv2$switch==0 & (tmpv2$level==7 | tmpv2$level==8 | tmpv2$level==9),'answercorrect'])
  ttsw[i,'v2.RT.sw0.lvl789'] <- mean(tmpv2[tmpv2$switch==0 & (tmpv2$level==7 | tmpv2$level==8 | tmpv2$level==9),'responsetime'])
  ttsw[i,'v2.Acc.sw1.lvl789'] <- mean(tmpv2[tmpv2$switch==1 & (tmpv2$level==7 | tmpv2$level==8 | tmpv2$level==9),'answercorrect'])
  ttsw[i,'v2.RT.sw1.lvl789'] <- mean(tmpv2[tmpv2$switch==1 & (tmpv2$level==7 | tmpv2$level==8 | tmpv2$level==9),'responsetime'])
  ttsw[i,'v2.trainedTSW.lvl789.cost'] <- ttsw[i,'v2.RT.sw1.lvl789']-ttsw[i,'v2.RT.sw0.lvl789']
  
  
  ttsw[i,'DATE.v0.lvl23'] <- dates[1]
  ttsw[i,'DATE.v2'] <- dates[2]
  ttsw[i,'DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  
  if (length(dates)==3){
    tmpv3 <- tmp[as.Date(tmp$date) == dates[3],]
    ttsw[i,'NumItPre.v3'] <- dim(tmpv3)[1]
    
    # Visit 3:
    
    ttsw[i,'v3.Acc.sw0.lvl23'] <- mean(tmpv3[tmpv3$switch==0 & (tmpv3$level==2 | tmpv3$level==3),'answercorrect'])
    ttsw[i,'v3.RT.sw0.lvl23'] <- mean(tmpv3[tmpv3$switch==0 & (tmpv3$level==2 | tmpv3$level==3),'responsetime'])
    ttsw[i,'v3.Acc.sw1.lvl23'] <- mean(tmpv3[tmpv3$switch==1 & (tmpv3$level==2 | tmpv3$level==3),'answercorrect'])
    ttsw[i,'v3.RT.sw1.lvl23'] <- mean(tmpv3[tmpv3$switch==1 & (tmpv3$level==2 | tmpv3$level==3),'responsetime'])
    ttsw[i,'v3.trainedTSW.lvl23.cost'] <- ttsw[i,'v3.RT.sw1.lvl23']-ttsw[i,'v3.RT.sw0.lvl23']
    
    ttsw[i,'v3.Acc.sw0.lvl456'] <- mean(tmpv3[tmpv3$switch==0 & (tmpv3$level==4 | tmpv3$level==5 | tmpv3$level==6),'answercorrect'])
    ttsw[i,'v3.RT.sw0.lvl456'] <- mean(tmpv3[tmpv3$switch==0 & (tmpv3$level==4 | tmpv3$level==5 | tmpv3$level==6),'responsetime'])
    ttsw[i,'v3.Acc.sw1.lvl456'] <- mean(tmpv3[tmpv3$switch==1 & (tmpv3$level==4 | tmpv3$level==5 | tmpv3$level==6),'answercorrect'])
    ttsw[i,'v3.RT.sw1.lvl456'] <- mean(tmpv3[tmpv3$switch==1 & (tmpv3$level==4 | tmpv3$level==5 | tmpv3$level==6),'responsetime'])
    ttsw[i,'v3.trainedTSW.lvl456.cost'] <- ttsw[i,'v3.RT.sw1.lvl456']-ttsw[i,'v3.RT.sw0.lvl456']
    
    ttsw[i,'v3.Acc.sw0.lvl789'] <- mean(tmpv3[tmpv3$switch==0 & (tmpv3$level==7 | tmpv3$level==8 | tmpv3$level==9),'answercorrect'])
    ttsw[i,'v3.RT.sw0.lvl789'] <- mean(tmpv3[tmpv3$switch==0 & (tmpv3$level==7 | tmpv3$level==8 | tmpv3$level==9),'responsetime'])
    ttsw[i,'v3.Acc.sw1.lvl789'] <- mean(tmpv3[tmpv3$switch==1 & (tmpv3$level==7 | tmpv3$level==8 | tmpv3$level==9),'answercorrect'])
    ttsw[i,'v3.RT.sw1.lvl789'] <- mean(tmpv3[tmpv3$switch==1 & (tmpv3$level==7 | tmpv3$level==8 | tmpv3$level==9),'responsetime'])
    ttsw[i,'v3.trainedTSW.lvl789.cost'] <- ttsw[i,'v3.RT.sw1.lvl789']-ttsw[i,'v3.RT.sw0.lvl789']
    
    ttsw[i,'DATE.v3'] <- dates[3]
    ttsw[i,'DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  }
}

#ttsw[is.na(ttsw$DATE.v2),c('NumItPre.v2','NumItPost.v2')] <- NA
write.xlsx(ttsw, paste(dir, 'summary/', 'transfer_trained_taskswitching', '.xlsx', sep=''), row.names = F)
rm(ttsw)

# Trained Updating:
tnumupdating <- read.csv2(paste(dir, 'transfer_trained_updating_trial.csv', sep=''),sep=';')
tnumupdating <- tnumupdating[which(nchar(tnumupdating$id)>3),]
ids <- names(table(tnumupdating$id))

tnup <- as.data.frame(matrix(NA, length(ids), 33))
colnames(tnup) <- c('ID',
                    'v1.CorrCount.lvl2', 'v1.CorrYN.lvl2', 'v1.RT.lvl2', 'v1.RT.lvl2CORR','v1.CorrCount.lvl4', 'v1.CorrYN.lvl4', 'v1.RT.lvl4', 'v1.RT.lvl4CORR',
                    'v2.CorrCount.lvl2', 'v2.CorrYN.lvl2', 'v2.RT.lvl2', 'v2.RT.lvl2CORR','v2.CorrCount.lvl4', 'v2.CorrYN.lvl4', 'v2.RT.lvl4', 'v2.RT.lvl4CORR',
                    'v3.CorrCount.lvl2', 'v3.CorrYN.lvl2', 'v3.RT.lvl2', 'v3.RT.lvl2CORR','v3.CorrCount.lvl4', 'v3.CorrYN.lvl4', 'v3.RT.lvl4', 'v3.RT.lvl4CORR',
                    'DATE.v1', 'DATE.v2', 'DATE.v3','DATE-diff12', 'DATE-diff23',
                    'NumIt.v1','NumIt.v2','NumIt.v3')


for (i in 1:length(ids)) {
  tmp <- subset(tnumupdating, tnumupdating$id==ids[i])
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  
  
  tnup[i,'ID'] <- ids[i]
  
  # Visit 1:
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  
  tnup[i,'NumIt.v1'] <- dim(tmpv1)[1]
  tnup[i,'v1.CorrCount.lvl2'] <- mean(tmpv1$switch[tmpv1$level==2])
  tnup[i,'v1.CorrYN.lvl2'] <- mean(tmpv1$answercorrect[tmpv1$level==2])
  tnup[i,'v1.RT.lvl2'] <- mean(tmpv1$responsetime[tmpv1$level==2])
  tnup[i,'v1.RT.lvl2CORR'] <- mean(tmpv1$responsetime[tmpv1$level==2 & tmpv1$answercorrect==1])
  tnup[i,'v1.CorrCount.lvl4'] <- mean(tmpv1$switch[tmpv1$level==4])
  tnup[i,'v1.CorrYN.lvl4'] <- mean(tmpv1$answercorrect[tmpv1$level==4])
  tnup[i,'v1.RT.lvl4'] <- mean(tmpv1$responsetime[tmpv1$level==4])
  tnup[i,'v1.RT.lvl4CORR'] <- mean(tmpv1$responsetime[tmpv1$level==4 & tmpv1$answercorrect==1])
  
  # Visit 2:
  tmpv2 <- tmp[as.Date(tmp$date) == dates[2],]
  
  tnup[i,'NumIt.v2'] <- dim(tmpv2)[1]
  tnup[i,'v2.CorrCount.lvl2'] <- mean(tmpv2$switch[tmpv2$level==2])
  tnup[i,'v2.CorrYN.lvl2'] <- mean(tmpv2$answercorrect[tmpv2$level==2])
  tnup[i,'v2.RT.lvl2'] <- mean(tmpv2$responsetime[tmpv2$level==2])
  tnup[i,'v2.RT.lvl2CORR'] <- mean(tmpv2$responsetime[tmpv2$level==2 & tmpv2$answercorrect==1])
  tnup[i,'v2.CorrCount.lvl4'] <- mean(tmpv2$switch[tmpv2$level==4])
  tnup[i,'v2.CorrYN.lvl4'] <- mean(tmpv2$answercorrect[tmpv2$level==4])
  tnup[i,'v2.RT.lvl4'] <- mean(tmpv2$responsetime[tmpv2$level==4])
  tnup[i,'v2.RT.lvl4CORR'] <- mean(tmpv2$responsetime[tmpv2$level==4 & tmpv2$answercorrect==1])
  
  
  tnup[i,'DATE.v1'] <- dates[1]
  tnup[i,'DATE.v2'] <- dates[2]
  tnup[i,'DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  
  if (length(dates)==3){
    # Visit 3:
    tmpv3 <- tmp[as.Date(tmp$date) == dates[3],]
    
    tnup[i,'NumIt.v3'] <- dim(tmpv3)[1]
    tnup[i,'v3.CorrCount.lvl2'] <- mean(tmpv3$switch[tmpv3$level==2])
    tnup[i,'v3.CorrYN.lvl2'] <- mean(tmpv3$answercorrect[tmpv3$level==2])
    tnup[i,'v3.RT.lvl2'] <- mean(tmpv3$responsetime[tmpv3$level==2])
    tnup[i,'v3.RT.lvl2CORR'] <- mean(tmpv3$responsetime[tmpv3$level==2 & tmpv3$answercorrect==1])
    tnup[i,'v3.CorrCount.lvl4'] <- mean(tmpv3$switch[tmpv3$level==4])
    tnup[i,'v3.CorrYN.lvl4'] <- mean(tmpv3$answercorrect[tmpv3$level==4])
    tnup[i,'v3.RT.lvl4'] <- mean(tmpv3$responsetime[tmpv3$level==4])
    tnup[i,'v3.RT.lvl4CORR'] <- mean(tmpv3$responsetime[tmpv3$level==4 & tmpv3$answercorrect==1])
    tnup[i,'DATE.v3'] <- dates[3]
    tnup[i,'DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  }
}
tnup[is.na(tnup$DATE.v2),c('NumIt.v2')] <- NA
write.xlsx(tnup, paste(dir, 'summary/', 'transfer_trained_upd', '.xlsx', sep=''), row.names = F)
rm(tnup)


# Verbal Recall:
vrecall <- read.csv2(paste(dir, 'transfer_verbal_recall.csv', sep=''),sep=';')
vrecall <- vrecall[which(nchar(vrecall$id)>3),]
ids <- names(table(vrecall$id))

vrc <- as.data.frame(matrix(NA, length(ids), 12))


colnames(vrc) <- c('ID','v1.Acc', 'v2.Acc', 'v3.Acc',
                   'DATE.v1', 'DATE.v2', 'DATE.v3','DATE-diff12', 'DATE-diff23',
                   'NumIt.v1','NumIt.v2','NumIt.v3')




for (i in 1:length(ids)) {
  
  tmp <- subset(vrecall, vrecall$id==ids[i])
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  
  
  vrc[i,'ID'] <- ids[i]
  
  # Visit 1:
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  vrc[i,'NumIt.v1'] <- dim(tmpv1)[1]
  vrc[i,'v1.Acc'] <- sum(tmpv1$answer_is_correct)
  
  # Visit 2:
  tmpv2 <- tmp[as.Date(tmp$date) == dates[2],]
  vrc[i,'NumIt.v2'] <- dim(tmpv2)[1]
  vrc[i,'v2.Acc'] <- sum(tmpv2$answer_is_correct)
  
  
  vrc[i,'DATE.v1'] <- dates[1]
  vrc[i,'DATE.v2'] <- dates[2]
  vrc[i,'DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  
  if (length(dates)==3){
    # Visit 3:
    tmpv3 <- tmp[as.Date(tmp$date) == dates[3],]
    vrc[i,'NumIt.v3'] <- dim(tmpv3)[1]
    vrc[i,'v3.Acc'] <- sum(tmpv3$answer_is_correct)
    vrc[i,'DATE.v3'] <- dates[3]
    vrc[i,'DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  }
}
vrc[is.na(vrc$DATE.v2),c('NumIt.v2')] <- NA
write.xlsx(vrc, paste(dir, 'summary/', 'transfer_verbal_recall', '.xlsx', sep=''), row.names = F)
rm(vrc)



##################
# QUESTIONNAIRES #
##################

# NEO-PI
qneo <- read.csv2(paste(dir, 'questioner_neo_pi.csv', sep=''),sep=';')
qneo <- qneo[which(nchar(qneo$id)>3),]
ids <- names(table(qneo$id))

neo <- as.data.frame(matrix(NA, length(ids), 111))


colnames(neo) <- c('ID',
                   'v1.N1', 'v1.N2', 'v1.N3','v1.N4','v1.N5','v1.N6', 'v1.N',
                   'v1.E1', 'v1.E2', 'v1.E3','v1.E4','v1.E5','v1.E6', 'v1.E',
                   'v1.O1', 'v1.O2', 'v1.O3','v1.O4','v1.O5','v1.O6', 'v1.O',
                   'v1.A1', 'v1.A2', 'v1.A3','v1.A4','v1.A5','v1.A6', 'v1.A',
                   'v1.C1', 'v1.C2', 'v1.C3','v1.C4','v1.C5','v1.C6', 'v1.C',
                   
                   'v2.N1', 'v2.N2', 'v2.N3','v2.N4','v2.N5','v2.N6', 'v2.N',
                   'v2.E1', 'v2.E2', 'v2.E3','v2.E4','v2.E5','v2.E6', 'v2.E',
                   'v2.O1', 'v2.O2', 'v2.O3','v2.O4','v2.O5','v2.O6', 'v2.O',
                   'v2.A1', 'v2.A2', 'v2.A3','v2.A4','v2.A5','v2.A6', 'v2.A',
                   'v2.C1', 'v2.C2', 'v2.C3','v2.C4','v2.C5','v2.C6', 'v2.C',
                   
                   'v3.N1', 'v3.N2', 'v3.N3','v3.N4','v3.N5','v3.N6', 'v3.N',
                   'v3.E1', 'v3.E2', 'v3.E3','v3.E4','v3.E5','v3.E6', 'v3.E',
                   'v3.O1', 'v3.O2', 'v3.O3','v3.O4','v3.O5','v3.O6', 'v3.O',
                   'v3.A1', 'v3.A2', 'v3.A3','v3.A4','v3.A5','v3.A6', 'v3.A',
                   'v3.C1', 'v3.C2', 'v3.C3','v3.C4','v3.C5','v3.C6', 'v3.C',
                   
                   'DATE.v1', 'DATE.v2', 'DATE.v3','DATE-diff12', 'DATE-diff23')


for (i in 1:length(ids)) {
  tmp <- subset(qneo, qneo$id==ids[i])
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  
  neo[i,'ID'] <- ids[i]
  
  # Visit 1:
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  
  neo[i,'v1.N1'] <- sum(tmpv1$value[tmpv1$scale=='N1'])
  neo[i,'v1.N2'] <- sum(tmpv1$value[tmpv1$scale=='N2'])
  neo[i,'v1.N3'] <- sum(tmpv1$value[tmpv1$scale=='N3'])
  neo[i,'v1.N4'] <- sum(tmpv1$value[tmpv1$scale=='N4'])
  neo[i,'v1.N5'] <- sum(tmpv1$value[tmpv1$scale=='N5'])
  neo[i,'v1.N6'] <- sum(tmpv1$value[tmpv1$scale=='N6'])
  neo[i,'v1.N'] <- sum(c(neo[i,'v1.N1'],neo[i,'v1.N2'],
                         neo[i,'v1.N3'],neo[i,'v1.N4'],
                         neo[i,'v1.N5'],neo[i,'v1.N6']))
  
  neo[i,'v1.E1'] <- sum(tmpv1$value[tmpv1$scale=='E1'])
  neo[i,'v1.E2'] <- sum(tmpv1$value[tmpv1$scale=='E2'])
  neo[i,'v1.E3'] <- sum(tmpv1$value[tmpv1$scale=='E3'])
  neo[i,'v1.E4'] <- sum(tmpv1$value[tmpv1$scale=='E4'])
  neo[i,'v1.E5'] <- sum(tmpv1$value[tmpv1$scale=='E5'])
  neo[i,'v1.E6'] <- sum(tmpv1$value[tmpv1$scale=='E6'])
  neo[i,'v1.E'] <- sum(c(neo[i,'v1.E1'],neo[i,'v1.E2'],
                         neo[i,'v1.E3'],neo[i,'v1.E4'],
                         neo[i,'v1.E5'],neo[i,'v1.E6']))
  
  
  neo[i,'v1.O1'] <- sum(tmpv1$value[tmpv1$scale=='O1'])
  neo[i,'v1.O2'] <- sum(tmpv1$value[tmpv1$scale=='O2'])
  neo[i,'v1.O3'] <- sum(tmpv1$value[tmpv1$scale=='O3'])
  neo[i,'v1.O4'] <- sum(tmpv1$value[tmpv1$scale=='O4'])
  neo[i,'v1.O5'] <- sum(tmpv1$value[tmpv1$scale=='O5'])
  neo[i,'v1.O6'] <- sum(tmpv1$value[tmpv1$scale=='O6'])
  neo[i,'v1.O'] <- sum(c(neo[i,'v1.O1'],neo[i,'v1.O2'],
                         neo[i,'v1.O3'],neo[i,'v1.O4'],
                         neo[i,'v1.O5'],neo[i,'v1.O6']))
  
  neo[i,'v1.A1'] <- sum(tmpv1$value[tmpv1$scale=='A1'])
  neo[i,'v1.A2'] <- sum(tmpv1$value[tmpv1$scale=='A2'])
  neo[i,'v1.A3'] <- sum(tmpv1$value[tmpv1$scale=='A3'])
  neo[i,'v1.A4'] <- sum(tmpv1$value[tmpv1$scale=='A4'])
  neo[i,'v1.A5'] <- sum(tmpv1$value[tmpv1$scale=='A5'])
  neo[i,'v1.A6'] <- sum(tmpv1$value[tmpv1$scale=='A6'])
  neo[i,'v1.A'] <- sum(c(neo[i,'v1.A1'],neo[i,'v1.A2'],
                         neo[i,'v1.A3'],neo[i,'v1.A4'],
                         neo[i,'v1.A5'],neo[i,'v1.A6']))
  
  neo[i,'v1.C1'] <- sum(tmpv1$value[tmpv1$scale=='C1'])
  neo[i,'v1.C2'] <- sum(tmpv1$value[tmpv1$scale=='C2'])
  neo[i,'v1.C3'] <- sum(tmpv1$value[tmpv1$scale=='C3'])
  neo[i,'v1.C4'] <- sum(tmpv1$value[tmpv1$scale=='C4'])
  neo[i,'v1.C5'] <- sum(tmpv1$value[tmpv1$scale=='C5'])
  neo[i,'v1.C6'] <- sum(tmpv1$value[tmpv1$scale=='C6'])
  neo[i,'v1.C'] <- sum(c(neo[i,'v1.C1'],neo[i,'v1.C2'],
                         neo[i,'v1.C3'],neo[i,'v1.C4'],
                         neo[i,'v1.C5'],neo[i,'v1.C6']))
  
  neo[i,'numR.v1'] <- dim(tmpv1)[1]
  neo[i,'DATE.v1'] <- dates[1]
  
  
  tmpv2 <- tmp[as.Date(tmp$date) == dates[2],]
  
  neo[i,'v2.N1'] <- sum(tmpv2$value[tmpv2$scale=='N1'])
  neo[i,'v2.N2'] <- sum(tmpv2$value[tmpv2$scale=='N2'])
  neo[i,'v2.N3'] <- sum(tmpv2$value[tmpv2$scale=='N3'])
  neo[i,'v2.N4'] <- sum(tmpv2$value[tmpv2$scale=='N4'])
  neo[i,'v2.N5'] <- sum(tmpv2$value[tmpv2$scale=='N5'])
  neo[i,'v2.N6'] <- sum(tmpv2$value[tmpv2$scale=='N6'])
  neo[i,'v2.N'] <- sum(c(neo[i,'v2.N1'],neo[i,'v2.N2'],
                         neo[i,'v2.N3'],neo[i,'v2.N4'],
                         neo[i,'v2.N5'],neo[i,'v2.N6']))
  
  neo[i,'v2.E1'] <- sum(tmpv2$value[tmpv2$scale=='E1'])
  neo[i,'v2.E2'] <- sum(tmpv2$value[tmpv2$scale=='E2'])
  neo[i,'v2.E3'] <- sum(tmpv2$value[tmpv2$scale=='E3'])
  neo[i,'v2.E4'] <- sum(tmpv2$value[tmpv2$scale=='E4'])
  neo[i,'v2.E5'] <- sum(tmpv2$value[tmpv2$scale=='E5'])
  neo[i,'v2.E6'] <- sum(tmpv2$value[tmpv2$scale=='E6'])
  neo[i,'v2.E'] <- sum(c(neo[i,'v2.E1'],neo[i,'v2.E2'],
                         neo[i,'v2.E3'],neo[i,'v2.E4'],
                         neo[i,'v2.E5'],neo[i,'v2.E6']))
  
  
  neo[i,'v2.O1'] <- sum(tmpv2$value[tmpv2$scale=='O1'])
  neo[i,'v2.O2'] <- sum(tmpv2$value[tmpv2$scale=='O2'])
  neo[i,'v2.O3'] <- sum(tmpv2$value[tmpv2$scale=='O3'])
  neo[i,'v2.O4'] <- sum(tmpv2$value[tmpv2$scale=='O4'])
  neo[i,'v2.O5'] <- sum(tmpv2$value[tmpv2$scale=='O5'])
  neo[i,'v2.O6'] <- sum(tmpv2$value[tmpv2$scale=='O6'])
  neo[i,'v2.O'] <- sum(c(neo[i,'v2.O1'],neo[i,'v2.O2'],
                         neo[i,'v2.O3'],neo[i,'v2.O4'],
                         neo[i,'v2.O5'],neo[i,'v2.O6']))
  
  neo[i,'v2.A1'] <- sum(tmpv2$value[tmpv2$scale=='A1'])
  neo[i,'v2.A2'] <- sum(tmpv2$value[tmpv2$scale=='A2'])
  neo[i,'v2.A3'] <- sum(tmpv2$value[tmpv2$scale=='A3'])
  neo[i,'v2.A4'] <- sum(tmpv2$value[tmpv2$scale=='A4'])
  neo[i,'v2.A5'] <- sum(tmpv2$value[tmpv2$scale=='A5'])
  neo[i,'v2.A6'] <- sum(tmpv2$value[tmpv2$scale=='A6'])
  neo[i,'v2.A'] <- sum(c(neo[i,'v2.A1'],neo[i,'v2.A2'],
                         neo[i,'v2.A3'],neo[i,'v2.A4'],
                         neo[i,'v2.A5'],neo[i,'v2.A6']))
  
  neo[i,'v2.C1'] <- sum(tmpv2$value[tmpv2$scale=='C1'])
  neo[i,'v2.C2'] <- sum(tmpv2$value[tmpv2$scale=='C2'])
  neo[i,'v2.C3'] <- sum(tmpv2$value[tmpv2$scale=='C3'])
  neo[i,'v2.C4'] <- sum(tmpv2$value[tmpv2$scale=='C4'])
  neo[i,'v2.C5'] <- sum(tmpv2$value[tmpv2$scale=='C5'])
  neo[i,'v2.C6'] <- sum(tmpv2$value[tmpv2$scale=='C6'])
  neo[i,'v2.C'] <- sum(c(neo[i,'v2.C1'],neo[i,'v2.C2'],
                         neo[i,'v2.C3'],neo[i,'v2.C4'],
                         neo[i,'v2.C5'],neo[i,'v2.C6']))
  
  neo[i,'numR.v2'] <- dim(tmpv2)[1]
  neo[i,'DATE.v2'] <- dates[2]
  neo[i,'DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  
  if (length(dates)==3) {
    # Visit 3:
    tmpv3 <- tmp[as.Date(tmp$date) == dates[3],]
    neo[i,'v3.N1'] <- sum(tmpv3$value[tmpv3$scale=='N1'])
    neo[i,'v3.N2'] <- sum(tmpv3$value[tmpv3$scale=='N2'])
    neo[i,'v3.N3'] <- sum(tmpv3$value[tmpv3$scale=='N3'])
    neo[i,'v3.N4'] <- sum(tmpv3$value[tmpv3$scale=='N4'])
    neo[i,'v3.N5'] <- sum(tmpv3$value[tmpv3$scale=='N5'])
    neo[i,'v3.N6'] <- sum(tmpv3$value[tmpv3$scale=='N6'])
    neo[i,'v3.N'] <- sum(c(neo[i,'v3.N1'],neo[i,'v3.N2'],
                           neo[i,'v3.N3'],neo[i,'v3.N4'],
                           neo[i,'v3.N5'],neo[i,'v3.N6']))
    
    neo[i,'v3.E1'] <- sum(tmpv3$value[tmpv3$scale=='E1'])
    neo[i,'v3.E2'] <- sum(tmpv3$value[tmpv3$scale=='E2'])
    neo[i,'v3.E3'] <- sum(tmpv3$value[tmpv3$scale=='E3'])
    neo[i,'v3.E4'] <- sum(tmpv3$value[tmpv3$scale=='E4'])
    neo[i,'v3.E5'] <- sum(tmpv3$value[tmpv3$scale=='E5'])
    neo[i,'v3.E6'] <- sum(tmpv3$value[tmpv3$scale=='E6'])
    neo[i,'v3.E'] <- sum(c(neo[i,'v3.E1'],neo[i,'v3.E2'],
                           neo[i,'v3.E3'],neo[i,'v3.E4'],
                           neo[i,'v3.E5'],neo[i,'v3.E6']))
    
    
    neo[i,'v3.O1'] <- sum(tmpv3$value[tmpv3$scale=='O1'])
    neo[i,'v3.O2'] <- sum(tmpv3$value[tmpv3$scale=='O2'])
    neo[i,'v3.O3'] <- sum(tmpv3$value[tmpv3$scale=='O3'])
    neo[i,'v3.O4'] <- sum(tmpv3$value[tmpv3$scale=='O4'])
    neo[i,'v3.O5'] <- sum(tmpv3$value[tmpv3$scale=='O5'])
    neo[i,'v3.O6'] <- sum(tmpv3$value[tmpv3$scale=='O6'])
    neo[i,'v3.O'] <- sum(c(neo[i,'v3.O1'],neo[i,'v3.O2'],
                           neo[i,'v3.O3'],neo[i,'v3.O4'],
                           neo[i,'v3.O5'],neo[i,'v3.O6']))
    
    neo[i,'v3.A1'] <- sum(tmpv3$value[tmpv3$scale=='A1'])
    neo[i,'v3.A2'] <- sum(tmpv3$value[tmpv3$scale=='A2'])
    neo[i,'v3.A3'] <- sum(tmpv3$value[tmpv3$scale=='A3'])
    neo[i,'v3.A4'] <- sum(tmpv3$value[tmpv3$scale=='A4'])
    neo[i,'v3.A5'] <- sum(tmpv3$value[tmpv3$scale=='A5'])
    neo[i,'v3.A6'] <- sum(tmpv3$value[tmpv3$scale=='A6'])
    neo[i,'v3.A'] <- sum(c(neo[i,'v3.A1'],neo[i,'v3.A2'],
                           neo[i,'v3.A3'],neo[i,'v3.A4'],
                           neo[i,'v3.A5'],neo[i,'v3.A6']))
    
    neo[i,'v3.C1'] <- sum(tmpv3$value[tmpv3$scale=='C1'])
    neo[i,'v3.C2'] <- sum(tmpv3$value[tmpv3$scale=='C2'])
    neo[i,'v3.C3'] <- sum(tmpv3$value[tmpv3$scale=='C3'])
    neo[i,'v3.C4'] <- sum(tmpv3$value[tmpv3$scale=='C4'])
    neo[i,'v3.C5'] <- sum(tmpv3$value[tmpv3$scale=='C5'])
    neo[i,'v3.C6'] <- sum(tmpv3$value[tmpv3$scale=='C6'])
    neo[i,'v3.C'] <- sum(c(neo[i,'v3.C1'],neo[i,'v3.C2'],
                           neo[i,'v3.C3'],neo[i,'v3.C4'],
                           neo[i,'v3.C5'],neo[i,'v3.C6']))
    
    neo[i,'numR.v3'] <- dim(tmpv3)[1]
    neo[i,'DATE.v3'] <- dates[3]
    neo[i,'DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  }
}
write.xlsx(neo, paste(dir, 'summary/', 'NEO', '.xlsx', sep=''), row.names = F)
rm(neo)




# BARATT:
# (http://www.impulsivity.org/measurement/bis11)
qbaratt <- read.csv2(paste(dir, 'questioner_baratt.csv', sep=''),sep=';')
qbaratt <- qbaratt[which(nchar(qbaratt$id)>3),]
ids <- names(table(qbaratt$id))
bar <- as.data.frame(matrix(NA, length(ids), 36))
colnames(bar) <- c('ID',
                   'v1.A.Att', 'v1.A.CogInst', 'v1.A',
                   'v1.M.Mot', 'v1.M.Pers', 'v1.M',
                   'v1.N.SelfCon', 'v1.N.CogComp', 'v1.N',
                   'numR.v1',
                   'v2.A.Att', 'v2.A.CogInst', 'v2.A',
                   'v2.M.Mot', 'v2.M.Pers', 'v2.M',
                   'v2.N.SelfCon', 'v2.N.CogComp', 'v2.N',
                   'numR.v2',  
                   'v3.A.Att', 'v3.A.CogInst', 'v3.A',
                   'v3.M.Mot', 'v3.M.Pers', 'v3.M',
                   'v3.N.SelfCon', 'v3.N.CogComp', 'v3.N',
                   'numR.v3',
                   'DATE.v1', 'DATE.v2', 'DATE.v3','DATE-diff12', 'DATE-diff23')

for (i in 1:length(ids)) {
  tmp <- subset(qbaratt, qbaratt$id==ids[i])
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  
  bar[i,'ID'] <- ids[i]
  
  # Visit 1:
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  
  bar[i,'v1.A.Att'] <- sum(c(tmpv1$answer[is.element(tmpv1$question_nr, c(5,11,28))],
                             5-tmpv1$answer[is.element(tmpv1$question_nr, c(9,20))]))
  bar[i,'v1.A.CogInst'] <- sum(c(tmpv1$answer[is.element(tmpv1$question_nr, c(6,24,26))]))
  bar[i,'v1.A'] <- bar[i,'v1.A.Att']+bar[i,'v1.A.CogInst']
  bar[i,'v1.M.Mot'] <- sum(c(tmpv1$answer[is.element(tmpv1$question_nr,
                                                     c(2,3,4,17,19,22,25))]))
  bar[i,'v1.M.Pers'] <- sum(c(tmpv1$answer[is.element(tmpv1$question_nr, c(16,21,23))],
                              5-tmpv1$answer[is.element(tmpv1$question_nr, c(30))]))
  bar[i,'v1.M'] <-  bar[i,'v1.M.Mot']+bar[i,'v1.M.Pers']
  bar[i,'v1.N.SelfCon'] <- sum(c(tmpv1$answer[is.element(tmpv1$question_nr, c(14))],
                                 5-tmpv1$answer[is.element(tmpv1$question_nr, c(1,7,8,12,13))]))
  bar[i,'v1.N.CogComp'] <- sum(c(tmpv1$answer[is.element(tmpv1$question_nr, c(18,27))],
                                 5-tmpv1$answer[is.element(tmpv1$question_nr, c(10,15,29))]))
  bar[i,'v1.N'] <-  bar[i,'v1.N.SelfCon']+bar[i,'v1.N.CogComp']
  bar[i,'numR.v1'] <- dim(tmpv1)[1]
  
  
  # Visit 2:
  tmpv2 <- tmp[as.Date(tmp$date) == dates[2],]
  
  bar[i,'v2.A.Att'] <- sum(c(tmpv2$answer[is.element(tmpv2$question_nr, c(5,11,28))],
                             5-tmpv2$answer[is.element(tmpv2$question_nr, c(9,20))]))
  bar[i,'v2.A.CogInst'] <- sum(c(tmpv2$answer[is.element(tmpv2$question_nr, c(6,24,26))]))
  bar[i,'v2.A'] <- bar[i,'v2.A.Att']+bar[i,'v2.A.CogInst']
  bar[i,'v2.M.Mot'] <- sum(c(tmpv2$answer[is.element(tmpv2$question_nr,
                                                     c(2,3,4,17,19,22,25))]))
  bar[i,'v2.M.Pers'] <- sum(c(tmpv2$answer[is.element(tmpv2$question_nr, c(16,21,23))],
                              5-tmpv2$answer[is.element(tmpv2$question_nr, c(30))]))
  bar[i,'v2.M'] <-  bar[i,'v2.M.Mot']+bar[i,'v2.M.Pers']
  bar[i,'v2.N.SelfCon'] <- sum(c(tmpv2$answer[is.element(tmpv2$question_nr, c(14))],
                                 5-tmpv2$answer[is.element(tmpv2$question_nr, c(1,7,8,12,13))]))
  bar[i,'v2.N.CogComp'] <- sum(c(tmpv2$answer[is.element(tmpv2$question_nr, c(18,27))],
                                 5-tmpv2$answer[is.element(tmpv2$question_nr, c(10,15,29))]))
  bar[i,'v2.N'] <-  bar[i,'v2.N.SelfCon']+bar[i,'v2.N.CogComp']
  bar[i,'numR.v2'] <- dim(tmpv2)[1]
  bar[i,'DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  
  if (length(dates)==3){
    # Visit 3:
    tmpv3 <- tmp[as.Date(tmp$date) == dates[3],]
    bar[i,'v3.A.Att'] <- sum(c(tmpv3$answer[is.element(tmpv3$question_nr, c(5,11,28))],
                               5-tmpv3$answer[is.element(tmpv3$question_nr, c(9,20))]))
    bar[i,'v3.A.CogInst'] <- sum(c(tmpv3$answer[is.element(tmpv3$question_nr, c(6,24,26))]))
    bar[i,'v3.A'] <- bar[i,'v3.A.Att']+bar[i,'v3.A.CogInst']
    bar[i,'v3.M.Mot'] <- sum(c(tmpv3$answer[is.element(tmpv3$question_nr,
                                                       c(2,3,4,17,19,22,25))]))
    bar[i,'v3.M.Pers'] <- sum(c(tmpv3$answer[is.element(tmpv3$question_nr, c(16,21,23))],
                                5-tmpv3$answer[is.element(tmpv3$question_nr, c(30))]))
    bar[i,'v3.M'] <-  bar[i,'v3.M.Mot']+bar[i,'v3.M.Pers']
    bar[i,'v3.N.SelfCon'] <- sum(c(tmpv3$answer[is.element(tmpv3$question_nr, c(14))],
                                   5-tmpv3$answer[is.element(tmpv3$question_nr, c(1,7,8,12,13))]))
    bar[i,'v3.N.CogComp'] <- sum(c(tmpv3$answer[is.element(tmpv3$question_nr, c(18,27))],
                                   5-tmpv3$answer[is.element(tmpv3$question_nr, c(10,15,29))]))
    bar[i,'v3.N'] <-  bar[i,'v3.N.SelfCon']+bar[i,'v3.N.CogComp']
    bar[i,'numR.v3'] <- dim(tmpv3)[1]
    bar[i,'DATE.v3'] <- dates[3]
    bar[i,'DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  }
}
write.xlsx(bar, paste(dir, 'summary/', 'baratt', '.xlsx', sep=''), row.names = F)
rm(bar)





# PDI:
qpdi <- read.csv2(paste(dir, 'questioner_pdi.csv', sep=''),sep=';')
qpdi <- qpdi[which(nchar(qpdi$id)>3),]
qpdi$answer1<- qpdi$answer1+1
qpdi$answer2<- qpdi$answer2+1
qpdi$answer3<- qpdi$answer3+1

ids <- names(table(qpdi$id))

pdi <- as.data.frame(matrix(NA, length(ids), 18))


colnames(pdi) <- c('ID',
                   'v1.Score_dist', 'v1.Score_time', 'v1.Score_conf', 'numR.v1',
                   'v2.Score_dist', 'v2.Score_time', 'v2.Score_conf', 'numR.v2',
                   'v3.Score_dist', 'v3.Score_time', 'v3.Score_conf', 'numR.v3',
                   'DATE.v1', 'DATE.v2', 'DATE.v3','DATE-diff12', 'DATE-diff23')


for (i in 1:length(ids)) {
  tmp <- subset(qpdi, qpdi$id==ids[i])
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  
  pdi[i,'ID'] <- ids[i]
  
  # Visit 1:
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  pdi[i,'v1.Score_dist'] <- sum(tmpv1$answer1)
  pdi[i,'v1.Score_time'] <- sum(tmpv1$answer2)
  pdi[i,'v1.Score_conf'] <- sum(tmpv1$answer3)
  pdi[i,'numR.v1'] <- dim(tmpv1)[1]
  pdi[i,'DATE.v1'] <- dates[1]
  
  # Visit 2:
  tmpv2 <- tmp[as.Date(tmp$date) == dates[2],]
  pdi[i,'v2.Score_dist'] <- sum(tmpv2$answer1)
  pdi[i,'v2.Score_time'] <- sum(tmpv2$answer2)
  pdi[i,'v2.Score_conf'] <- sum(tmpv2$answer3)
  pdi[i,'numR.v2'] <- dim(tmpv2)[1]
  pdi[i,'DATE.v2'] <- dates[2]
  pdi[i,'DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  
  if (length(dates)==3){
    # Visit 3:
    tmpv3 <- tmp[as.Date(tmp$date) == dates[3],]
    pdi[i,'v3.Score_dist'] <- sum(tmpv3$answer1)
    pdi[i,'v3.Score_time'] <- sum(tmpv3$answer2)
    pdi[i,'v3.Score_conf'] <- sum(tmpv3$answer3)
    pdi[i,'numR.v3'] <- dim(tmpv3)[1]
    pdi[i,'DATE.v3'] <- dates[3]
    pdi[i,'DATE-diff23'] <- as.numeric(dates[3]-dates[2])
    
  }
}
#pdi[is.na(pdi$DATE.v2),c('NumIt.v2')] <- NA
write.xlsx(pdi, paste(dir, 'summary/', 'pdi', '.xlsx', sep=''), row.names = F)
rm(pdi)
  

# LUCiD
qlucid <- read.csv2(paste(dir, 'questioner_lucid.csv', sep=''),sep=';')
qlucid <- qlucid[which(nchar(qlucid$id)>3),]
qlucid$question_nr <- qlucid$question_nr-1
ids <- names(table(qlucid$id))

lucid <- as.data.frame(matrix(NA, length(ids), 33))


colnames(lucid) <- c('ID',
  'v1.q0','v1.insight', 'v1.control', 'v1.thought', 'v1.realism', 'v1.memory', 'v1.dissociation', 'v1.NegEm', 'v1.PosEm',
  'v2.q0','v2.insight', 'v2.control', 'v2.thought', 'v2.realism', 'v2.memory', 'v2.dissociation', 'v2.NegEm', 'v2.PosEm',
  'v3.q0','v3.insight', 'v3.control', 'v3.thought', 'v3.realism', 'v3.memory', 'v3.dissociation', 'v3.NegEm', 'v3.PosEm',
  'DATE.v1', 'DATE.v2', 'DATE.v3','DATE-diff12', 'DATE-diff23')


for (i in 1:length(ids)) {
  tmp <- subset(qlucid, qlucid$id==ids[i])
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  
  lucid[i,'ID'] <- ids[i]
  
  # Visit 1:
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  lucid[i,'v1.q0'] <- tmpv1$answer[tmpv1$question_nr==0]
  lucid[i,'v1.insight'] <- tmpv1$answer[tmpv1$question_nr==1]+tmpv1$answer[tmpv1$question_nr==3]+tmpv1$answer[tmpv1$question_nr==8]+
    tmpv1$answer[tmpv1$question_nr==9]+tmpv1$answer[tmpv1$question_nr==16]+tmpv1$answer[tmpv1$question_nr==19]
  lucid[i,'v1.control'] <- tmpv1$answer[tmpv1$question_nr==4]+tmpv1$answer[tmpv1$question_nr==6]+tmpv1$answer[tmpv1$question_nr==10]+
    tmpv1$answer[tmpv1$question_nr==14]+tmpv1$answer[tmpv1$question_nr==23]
  lucid[i,'v1.thought'] <- tmpv1$answer[tmpv1$question_nr==5]+tmpv1$answer[tmpv1$question_nr==12]+tmpv1$answer[tmpv1$question_nr==22]
  lucid[i,'v1.realism'] <- tmpv1$answer[tmpv1$question_nr==7]+tmpv1$answer[tmpv1$question_nr==17]+tmpv1$answer[tmpv1$question_nr==20]
  lucid[i,'v1.memory'] <- tmpv1$answer[tmpv1$question_nr==2]+tmpv1$answer[tmpv1$question_nr==13]+tmpv1$answer[tmpv1$question_nr==18]+
    tmpv1$answer[tmpv1$question_nr==24]
  lucid[i,'v1.dissociation'] <- tmpv1$answer[tmpv1$question_nr==11]+tmpv1$answer[tmpv1$question_nr==15]+tmpv1$answer[tmpv1$question_nr==21]
  lucid[i,'v1.NegEm'] <- tmpv1$answer[tmpv1$question_nr==26]+tmpv1$answer[tmpv1$question_nr==28]
  lucid[i,'v1.PosEm'] <- tmpv1$answer[tmpv1$question_nr==25]+tmpv1$answer[tmpv1$question_nr==27]
  lucid[i,'numR.v1'] <- dim(tmpv1)[1]
  lucid[i,'DATE.v1'] <- dates[1]

  # Visit 2:
  tmpv2 <- tmp[as.Date(tmp$date) == dates[2],]
  lucid[i,'v2.q0'] <- tmpv2$answer[tmpv2$question_nr==0]
  lucid[i,'v2.insight'] <- tmpv2$answer[tmpv2$question_nr==1]+tmpv2$answer[tmpv2$question_nr==3]+tmpv2$answer[tmpv2$question_nr==8]+
    tmpv2$answer[tmpv2$question_nr==9]+tmpv2$answer[tmpv2$question_nr==16]+tmpv2$answer[tmpv2$question_nr==19]
  lucid[i,'v2.control'] <- tmpv2$answer[tmpv2$question_nr==4]+tmpv2$answer[tmpv2$question_nr==6]+tmpv2$answer[tmpv2$question_nr==10]+
    tmpv2$answer[tmpv2$question_nr==14]+tmpv2$answer[tmpv2$question_nr==23]
  lucid[i,'v2.thought'] <- tmpv2$answer[tmpv2$question_nr==5]+tmpv2$answer[tmpv2$question_nr==12]+tmpv2$answer[tmpv2$question_nr==22]
  lucid[i,'v2.realism'] <- tmpv2$answer[tmpv2$question_nr==7]+tmpv2$answer[tmpv2$question_nr==17]+tmpv2$answer[tmpv2$question_nr==20]
  lucid[i,'v2.memory'] <- tmpv2$answer[tmpv2$question_nr==2]+tmpv2$answer[tmpv2$question_nr==13]+tmpv2$answer[tmpv2$question_nr==18]+
    tmpv2$answer[tmpv2$question_nr==24]
  lucid[i,'v2.dissociation'] <- tmpv2$answer[tmpv2$question_nr==11]+tmpv2$answer[tmpv2$question_nr==15]+tmpv2$answer[tmpv2$question_nr==21]
  lucid[i,'v2.NegEm'] <- tmpv2$answer[tmpv2$question_nr==26]+tmpv2$answer[tmpv2$question_nr==28]
  lucid[i,'v2.PosEm'] <- tmpv2$answer[tmpv2$question_nr==25]+tmpv2$answer[tmpv2$question_nr==27]
  lucid[i,'numR.v2'] <- dim(tmpv2)[1]
  lucid[i,'DATE.v2'] <- dates[2]
  lucid[i,'DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  
  if (length(dates)==3){
    # Visit 3:
    tmpv3 <- tmp[as.Date(tmp$date) == dates[3],]
    lucid[i,'v3.q0'] <- tmpv3$answer[tmpv3$question_nr==0]
    lucid[i,'v3.insight'] <- tmpv3$answer[tmpv3$question_nr==1]+tmpv3$answer[tmpv3$question_nr==3]+tmpv3$answer[tmpv3$question_nr==8]+
      tmpv3$answer[tmpv3$question_nr==9]+tmpv3$answer[tmpv3$question_nr==16]+tmpv3$answer[tmpv3$question_nr==19]
    lucid[i,'v3.control'] <- tmpv3$answer[tmpv3$question_nr==4]+tmpv3$answer[tmpv3$question_nr==6]+tmpv3$answer[tmpv3$question_nr==10]+
      tmpv3$answer[tmpv3$question_nr==14]+tmpv3$answer[tmpv3$question_nr==23]
    lucid[i,'v3.thought'] <- tmpv3$answer[tmpv3$question_nr==5]+tmpv3$answer[tmpv3$question_nr==12]+tmpv3$answer[tmpv3$question_nr==22]
    lucid[i,'v3.realism'] <- tmpv3$answer[tmpv3$question_nr==7]+tmpv3$answer[tmpv3$question_nr==17]+tmpv3$answer[tmpv3$question_nr==20]
    lucid[i,'v3.memory'] <- tmpv3$answer[tmpv3$question_nr==2]+tmpv3$answer[tmpv3$question_nr==13]+tmpv3$answer[tmpv3$question_nr==18]+
      tmpv3$answer[tmpv3$question_nr==24]
    lucid[i,'v3.dissociation'] <- tmpv3$answer[tmpv3$question_nr==11]+tmpv3$answer[tmpv3$question_nr==15]+tmpv3$answer[tmpv3$question_nr==21]
    lucid[i,'v3.NegEm'] <- tmpv3$answer[tmpv3$question_nr==26]+tmpv3$answer[tmpv3$question_nr==28]
    lucid[i,'v3.PosEm'] <- tmpv3$answer[tmpv3$question_nr==25]+tmpv3$answer[tmpv3$question_nr==27]
    lucid[i,'numR.v3'] <- dim(tmpv3)[1]
    lucid[i,'DATE.v3'] <- dates[3]
    lucid[i,'DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  }
}
  
write.xlsx(lucid, paste(dir, 'summary/', 'lucid', '.xlsx', sep=''), row.names = F)
rm(lucid)

# MADRE
# [Check "Aningen/Inte så"]
qmadre <- read.csv2(paste(dir, 'questioner_madre.csv', sep=''),sep=';')
qmadre <- qmadre[which(nchar(qmadre$id)>3),]
qmadre$question_nr <- qmadre$question_nr-1
ids <- names(table(qmadre$id))
madre <- as.data.frame(matrix(NA, length(ids), 90))

colnames(madre) <- c('ID',
                   'v1.recall', 'v1.EmIntense', 'v1.EmTone', 
                   'v1.NightmaresFreq', 'v1.NightmaresDistress',
                   'v1.NightmaresSituation', 'v1.NightmaresRecurrPerc',
                   'v1.NightmaresChildhood', 'v1.NightmaresTopics',
                   'v1.LucidDreamsFreq', 'v1.LucidDreamsAgeOns',
                   'v1.DreamsAttitude=11','v1.DreamsAttitude=12','v1.DreamsAttitude=13',
                   'v1.DreamsAttitude=14','v1.DreamsAttitude=15','v1.DreamsAttitude=16',
                   'v1.DreamsAttitude=17','v1.DreamsAttitude=18',
                   'v1.DreamsAttitude',
                   'v1.DreamsShare', 'v1.DreamsRecord', 'v1.DreamsAffect',
                   'v1.DreamsCreative', 'v1.DreamsProbSolv', 'v1.DejaVu',
                   'v1.Read', 'v1.ReadHelp',
                   
                   'v2.recall', 'v2.EmIntense', 'v2.EmTone', 
                   'v2.NightmaresFreq', 'v2.NightmaresDistress',
                   'v2.NightmaresSituation', 'v2.NightmaresRecurrPerc',
                   'v2.NightmaresChildhood', 'v2.NightmaresTopics',
                   'v2.LucidDreamsFreq', 'v2.LucidDreamsAgeOns',
                   'v2.DreamsAttitude=11','v2.DreamsAttitude=12','v2.DreamsAttitude=13',
                   'v2.DreamsAttitude=14','v2.DreamsAttitude=15','v2.DreamsAttitude=16',
                   'v2.DreamsAttitude=17','v2.DreamsAttitude=18',
                   'v2.DreamsAttitude',
                   'v2.DreamsShare', 'v2.DreamsRecord', 'v2.DreamsAffect',
                   'v2.DreamsCreative', 'v2.DreamsProbSolv', 'v2.DejaVu',
                   'v2.Read', 'v2.ReadHelp',
                   
                   'v3.recall', 'v3.EmIntense', 'v3.EmTone', 
                   'v3.NightmaresFreq', 'v3.NightmaresDistress',
                   'v3.NightmaresSituation', 'v3.NightmaresRecurrPerc',
                   'v3.NightmaresChildhood', 'v3.NightmaresTopics',
                   'v3.LucidDreamsFreq', 'v3.LucidDreamsAgeOns',
                   'v3.DreamsAttitude=11','v3.DreamsAttitude=12','v3.DreamsAttitude=13',
                   'v3.DreamsAttitude=14','v3.DreamsAttitude=15','v3.DreamsAttitude=16',
                   'v3.DreamsAttitude=17','v3.DreamsAttitude=18',
                   'v3.DreamsAttitude',
                   'v3.DreamsShare', 'v3.DreamsRecord', 'v3.DreamsAffect',
                   'v3.DreamsCreative', 'v3.DreamsProbSolv', 'v3.DejaVu',
                   'v3.Read', 'v3.ReadHelp',
                   
                   'DATE.v1', 'DATE.v2', 'DATE.v3','DATE-diff12', 'DATE-diff23')
                   
                   
for(i in 1:length(ids)){
  tmp <- subset(qmadre, qmadre$id==ids[i])
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  
  madre[i,'ID'] <- ids[i]
  
  # Visit 1:
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  
  # sort(table(qmadre$answer[qmadre$question_nr==1]),T)[1:5]
  
  if(tmpv1$answer[tmpv1$question_nr==0] == 'Aldrig'){
    madre[i,'v1.recall'] <- 0
  } else if(tmpv1$answer[tmpv1$question_nr==0] == 'Mindre än en gång i månaden'){
    madre[i,'v1.recall'] <- 1
  } else if(tmpv1$answer[tmpv1$question_nr==0] == 'Ungefär en gång i månaden'){
    madre[i,'v1.recall'] <- 2
  } else if(tmpv1$answer[tmpv1$question_nr==0] == 'Två eller tre gånger i månaden'){
    madre[i,'v1.recall'] <- 3
  } else if(tmpv1$answer[tmpv1$question_nr==0] == 'Ungefär en gång i veckan  '){
    madre[i,'v1.recall'] <- 4
  } else if(tmpv1$answer[tmpv1$question_nr==0] == 'Flera gånger i veckan '){
    madre[i,'v1.recall'] <- 5
  } else if(tmpv1$answer[tmpv1$question_nr==0] == 'Nästan varje morgon'){
    madre[i,'v1.recall'] <- 6
  } else{
    madre[i,'v1.recall'] <- NA
  }
  
  if(tmpv1$answer[tmpv1$question_nr==1] == 'Inte alls intensiva'){
    madre[i,'v1.EmIntense'] <- 0
  } else if (tmpv1$answer[tmpv1$question_nr==1] == 'Inte så intensiva'){
    madre[i,'v1.EmIntense'] <- 1
  } else if(tmpv1$answer[tmpv1$question_nr==1] == 'Aningen intensiva'){
    madre[i,'v1.EmIntense'] <- 2
  } else if(tmpv1$answer[tmpv1$question_nr==1] == 'Ganska intensiva'){
    madre[i,'v1.EmIntense'] <- 3
  } else if(tmpv1$answer[tmpv1$question_nr==1] == 'Väldigt intensiva'){
    madre[i,'v1.EmIntense'] <- 4
  } else {
    madre[i,'v1.EmIntense'] <- NA
  }
  
  if(tmpv1$answer[tmpv1$question_nr==2] == 'Väldigt negativ') {
    madre[i,'v1.EmTone'] <- -2
  } else if(tmpv1$answer[tmpv1$question_nr==2] == 'Aningen negativ') {
    madre[i,'v1.EmTone'] <- -1
  } else if(tmpv1$answer[tmpv1$question_nr==2] == 'Neutral') {
    madre[i,'v1.EmTone'] <- 0
  } else if(tmpv1$answer[tmpv1$question_nr==2] == 'Aningen positiv'){
    madre[i,'v1.EmTone'] <- 1
  } else if(tmpv1$answer[tmpv1$question_nr==2] == 'Väldigt positiv'){
    madre[i,'v1.EmTone'] <- 2
  } else{
    madre[i,'v1.EmTone'] <- NA
  }
  
  if(tmpv1$answer[tmpv1$question_nr==3] == 'Aldrig'){
    madre[i,'v1.NightmaresFreq'] <- 0
  } else if(tmpv1$answer[tmpv1$question_nr==3] == 'Mindre än en gång per år'){
    madre[i,'v1.NightmaresFreq'] <- 1
  } else if(tmpv1$answer[tmpv1$question_nr==3] == 'Ungefär en gång per år'){
    madre[i,'v1.NightmaresFreq'] <- 2
  } else if(tmpv1$answer[tmpv1$question_nr==3] == 'Ungefär två till fyra gånger per år'){
    madre[i,'v1.NightmaresFreq'] <- 3
  } else if(tmpv1$answer[tmpv1$question_nr==3] == 'Ungefär en gång i månaden'){
    madre[i,'v1.NightmaresFreq'] <- 4
  } else if(tmpv1$answer[tmpv1$question_nr==3] == 'Två till tre gånger i månaden'){
    madre[i,'v1.NightmaresFreq'] <- 5
  } else if(tmpv1$answer[tmpv1$question_nr==3] == 'Ungefär en gång i veckan  '){
    madre[i,'v1.NightmaresFreq'] <- 6
  } else if(tmpv1$answer[tmpv1$question_nr==3] == 'Flera gånger i veckan '){
    madre[i,'v1.NightmaresFreq'] <- 7
  } else{
    madre[i,'v1.NightmaresFreq'] <- NA
  }
  
  if(tmpv1$answer[tmpv1$question_nr==4] == 'Inte alls jobbiga'){
    madre[i,'v1.NightmaresDistress'] <- 0
  } else if(tmpv1$answer[tmpv1$question_nr==4] == 'Inte så jobbiga'){
    madre[i,'v1.NightmaresDistress'] <- 1
  } else if(tmpv1$answer[tmpv1$question_nr==4] == 'Aningen jobbiga'){
    madre[i,'v1.NightmaresDistress'] <- 2
  } else if(tmpv1$answer[tmpv1$question_nr==4] == 'Ganska jobbiga'){
    madre[i,'v1.NightmaresDistress'] <- 3
  } else if(tmpv1$answer[tmpv1$question_nr==4] == 'Väldigt jobbiga'){
    madre[i,'v1.NightmaresDistress'] <- 4
  } else{
    madre[i,'v1.NightmaresDistress'] <- NA
  }
  
  if(tmpv1$answer[tmpv1$question_nr==5] == 'Nej'){
    madre[i,'v1.NightmaresSituation'] <- 0
  } else if(tmpv1$answer[tmpv1$question_nr==5] == 'Ja'){
    madre[i,'v1.NightmaresSituation'] <- 1
  } else{
    madre[i,'v1.NightmaresSituation'] <- NA
  }
  
  madre[i,'v1.NightmaresRecurrPerc'] <- as.numeric(as.vector(tmpv1$answer[tmpv1$question_nr==6]))
  
  if(tmpv1$answer[tmpv1$question_nr==7] == 'Aldrig'){
    madre[i,'v1.NightmaresChildhood'] <- 0
  } else if(tmpv1$answer[tmpv1$question_nr==7] == 'Mindre än en gång per år'){
    madre[i,'v1.NightmaresChildhood'] <- 1
  } else if(tmpv1$answer[tmpv1$question_nr==7] == 'Ungefär en gång per år'){
    madre[i,'v1.NightmaresChildhood'] <- 2
  } else if(tmpv1$answer[tmpv1$question_nr==7] == 'Ungefär två till fyra gånger per år'){
    madre[i,'v1.NightmaresChildhood'] <- 3
  } else if(tmpv1$answer[tmpv1$question_nr==7] == 'Ungefär en gång i månaden'){
    madre[i,'v1.NightmaresChildhood'] <- 4
  } else if(tmpv1$answer[tmpv1$question_nr==7] == 'Två till tre gånger i månaden'){
    madre[i,'v1.NightmaresChildhood'] <- 5
  } else if(tmpv1$answer[tmpv1$question_nr==7] == 'Ungefär en gång i veckan  '){
    madre[i,'v1.NightmaresChildhood'] <- 6
  } else if(tmpv1$answer[tmpv1$question_nr==7] == 'Flera gånger i veckan '){
    madre[i,'v1.NightmaresChildhood'] <- 7
  } else{
    madre[i,'v1.NightmaresChildhood'] <- NA
  }
  
  madre[i,'v1.NightmaresTopics'] <- as.vector(tmpv1$answer[tmpv1$question_nr==8])
  
  if(tmpv1$answer[tmpv1$question_nr==9] == 'Aldrig'){
    madre[i,'v1.LucidDreamsFreq'] <- 0
  } else if(tmpv1$answer[tmpv1$question_nr==9] == 'Mindre än en gång per år'){
    madre[i,'v1.LucidDreamsFreq'] <- 1
  } else if(tmpv1$answer[tmpv1$question_nr==9] == 'Ungefär en gång per år'){
    madre[i,'v1.LucidDreamsFreq'] <- 2
  } else if(tmpv1$answer[tmpv1$question_nr==9] == 'Ungefär två till fyra gånger per år'){
    madre[i,'v1.LucidDreamsFreq'] <- 3
  } else if(tmpv1$answer[tmpv1$question_nr==9] == 'Ungefär en gång i månaden'){
    madre[i,'v1.LucidDreamsFreq'] <- 4
  } else if(tmpv1$answer[tmpv1$question_nr==9] == 'Två till tre gånger i månaden'){
    madre[i,'v1.LucidDreamsFreq'] <- 5
  } else if(tmpv1$answer[tmpv1$question_nr==9] == 'Ungefär en gång i veckan  '){
    madre[i,'v1.LucidDreamsFreq'] <- 6
  } else if(tmpv1$answer[tmpv1$question_nr==9] == 'Flera gånger i veckan '){
    madre[i,'v1.LucidDreamsFreq'] <- 7
  } else{
    madre[i,'v1.LucidDreamsFreq'] <- NA
  }
  
  
  
  madre[i,'v1.LucidDreamsAgeOns'] <- as.numeric(as.vector(tmpv1$answer[tmpv1$question_nr==10]))
  
  if(tmpv1$answer[tmpv1$question_nr==11] == 'Inte alls'){
    madre[i,'v1.DreamsAttitude=11'] <- 0
  } else if(tmpv1$answer[tmpv1$question_nr==11] == 'Inte så mycket'){
    madre[i,'v1.DreamsAttitude=11'] <- 1
  } else if(tmpv1$answer[tmpv1$question_nr==11] == 'Delvis'){
    madre[i,'v1.DreamsAttitude=11'] <- 2
  } else if(tmpv1$answer[tmpv1$question_nr==11] == 'Ganska mycket'){
    madre[i,'v1.DreamsAttitude=11'] <- 3
  } else if(tmpv1$answer[tmpv1$question_nr==11] == 'Väldigt mycket'){
    madre[i,'v1.DreamsAttitude=11'] <- 4
  } else{
    madre[i,'v1.DreamsAttitude=11'] <- NA
  }
  
  if(tmpv1$answer[tmpv1$question_nr==12] == 'Inte alls'){
    madre[i,'v1.DreamsAttitude=12'] <- 0
  } else if(tmpv1$answer[tmpv1$question_nr==12] == 'Inte så mycket'){
    madre[i,'v1.DreamsAttitude=12'] <- 1
  } else if(tmpv1$answer[tmpv1$question_nr==12] == 'Delvis'){
    madre[i,'v1.DreamsAttitude=12'] <- 2
  } else if(tmpv1$answer[tmpv1$question_nr==12] == 'Ganska mycket'){
    madre[i,'v1.DreamsAttitude=12'] <- 3
  } else if(tmpv1$answer[tmpv1$question_nr==12] == 'Väldigt mycket'){
    madre[i,'v1.DreamsAttitude=12'] <- 4
  } else{
    madre[i,'v1.DreamsAttitude=12'] <- NA
  }
  
  if(tmpv1$answer[tmpv1$question_nr==13] == 'Inte alls'){
    madre[i,'v1.DreamsAttitude=13'] <- 0
  } else if(tmpv1$answer[tmpv1$question_nr==13] == 'Inte så mycket'){
    madre[i,'v1.DreamsAttitude=13'] <- 1
  } else if(tmpv1$answer[tmpv1$question_nr==13] == 'Delvis'){
    madre[i,'v1.DreamsAttitude=13'] <- 2
  } else if(tmpv1$answer[tmpv1$question_nr==13] == 'Ganska mycket'){
    madre[i,'v1.DreamsAttitude=13'] <- 3
  } else if(tmpv1$answer[tmpv1$question_nr==13] == 'Väldigt mycket'){
    madre[i,'v1.DreamsAttitude=13'] <- 4
  } else{
    madre[i,'v1.DreamsAttitude=13'] <- NA
  }
  
  if(tmpv1$answer[tmpv1$question_nr==14] == 'Inte alls'){
    madre[i,'v1.DreamsAttitude=14'] <- 0
  } else if(tmpv1$answer[tmpv1$question_nr==14] == 'Inte så mycket'){
    madre[i,'v1.DreamsAttitude=14'] <- 1
  } else if(tmpv1$answer[tmpv1$question_nr==14] == 'Delvis'){
    madre[i,'v1.DreamsAttitude=14'] <- 2
  } else if(tmpv1$answer[tmpv1$question_nr==14] == 'Ganska mycket'){
    madre[i,'v1.DreamsAttitude=14'] <- 3
  } else if(tmpv1$answer[tmpv1$question_nr==14] == 'Väldigt mycket'){
    madre[i,'v1.DreamsAttitude=14'] <- 4
  } else{
    madre[i,'v1.DreamsAttitude=14'] <- NA
  }
  
  if(tmpv1$answer[tmpv1$question_nr==15] == 'Inte alls'){
    madre[i,'v1.DreamsAttitude=15'] <- 0
  } else if(tmpv1$answer[tmpv1$question_nr==15] == 'Inte så mycket'){
    madre[i,'v1.DreamsAttitude=15'] <- 1
  } else if(tmpv1$answer[tmpv1$question_nr==15] == 'Delvis'){
    madre[i,'v1.DreamsAttitude=15'] <- 2
  } else if(tmpv1$answer[tmpv1$question_nr==15] == 'Ganska mycket'){
    madre[i,'v1.DreamsAttitude=15'] <- 3
  } else if(tmpv1$answer[tmpv1$question_nr==15] == 'Väldigt mycket'){
    madre[i,'v1.DreamsAttitude=15'] <- 4
  } else{
    madre[i,'v1.DreamsAttitude=15'] <- NA
  }
  
  if(tmpv1$answer[tmpv1$question_nr==16] == 'Inte alls'){
    madre[i,'v1.DreamsAttitude=16'] <- 0
  } else if(tmpv1$answer[tmpv1$question_nr==16] == 'Inte så mycket'){
    madre[i,'v1.DreamsAttitude=16'] <- 1
  } else if(tmpv1$answer[tmpv1$question_nr==16] == 'Delvis'){
    madre[i,'v1.DreamsAttitude=16'] <- 2
  } else if(tmpv1$answer[tmpv1$question_nr==16] == 'Ganska mycket'){
    madre[i,'v1.DreamsAttitude=16'] <- 3
  } else if(tmpv1$answer[tmpv1$question_nr==16] == 'Väldigt mycket'){
    madre[i,'v1.DreamsAttitude=16'] <- 4
  } else{
    madre[i,'v1.DreamsAttitude=16'] <- NA
  }
  
  if(tmpv1$answer[tmpv1$question_nr==17] == 'Inte alls'){
    madre[i,'v1.DreamsAttitude=17'] <- 0
  } else if(tmpv1$answer[tmpv1$question_nr==17] == 'Inte så mycket'){
    madre[i,'v1.DreamsAttitude=17'] <- 1
  } else if(tmpv1$answer[tmpv1$question_nr==17] == 'Delvis'){
    madre[i,'v1.DreamsAttitude=17'] <- 2
  } else if(tmpv1$answer[tmpv1$question_nr==17] == 'Ganska mycket'){
    madre[i,'v1.DreamsAttitude=17'] <- 3
  } else if(tmpv1$answer[tmpv1$question_nr==17] == 'Väldigt mycket'){
    madre[i,'v1.DreamsAttitude=17'] <- 4
  } else{
    madre[i,'v1.DreamsAttitude=17'] <- NA
  }
  
  if(tmpv1$answer[tmpv1$question_nr==18] == 'Inte alls'){
    madre[i,'v1.DreamsAttitude=18'] <- 0
  } else if(tmpv1$answer[tmpv1$question_nr==18] == 'Inte så mycket'){
    madre[i,'v1.DreamsAttitude=18'] <- 1
  } else if(tmpv1$answer[tmpv1$question_nr==18] == 'Delvis'){
    madre[i,'v1.DreamsAttitude=18'] <- 2
  } else if(tmpv1$answer[tmpv1$question_nr==18] == 'Ganska mycket'){
    madre[i,'v1.DreamsAttitude=18'] <- 3
  } else if(tmpv1$answer[tmpv1$question_nr==18] == 'Väldigt mycket'){
    madre[i,'v1.DreamsAttitude=18'] <- 4
  } else{
    madre[i,'v1.DreamsAttitude=18'] <- NA
  }
  
  madre[i,'v1.DreamsAttitude'] <- madre[i,'v1.DreamsAttitude=11']+madre[i,'v1.DreamsAttitude=12']+
    madre[i,'v1.DreamsAttitude=13']+madre[i,'v1.DreamsAttitude=14']+madre[i,'v1.DreamsAttitude=15']+
    madre[i,'v1.DreamsAttitude=16']+madre[i,'v1.DreamsAttitude=17']+madre[i,'v1.DreamsAttitude=18']
  
  if(tmpv1$answer[tmpv1$question_nr==19] == 'Aldrig'){
    madre[i,'v1.DreamsShare'] <- 0
  } else if(tmpv1$answer[tmpv1$question_nr==19] == 'Mindre än en gång per år'){
    madre[i,'v1.DreamsShare'] <- 1
  } else if(tmpv1$answer[tmpv1$question_nr==19] == 'Ungefär en gång per år'){
    madre[i,'v1.DreamsShare'] <- 2
  } else if(tmpv1$answer[tmpv1$question_nr==19] == 'Ungefär två till fyra gånger per år'){
    madre[i,'v1.DreamsShare'] <- 3
  } else if(tmpv1$answer[tmpv1$question_nr==19] == 'Ungefär en gång i månaden'){
    madre[i,'v1.DreamsShare'] <- 4
  } else if(tmpv1$answer[tmpv1$question_nr==19] == 'Två till tre gånger i månaden'){
    madre[i,'v1.DreamsShare'] <- 5
  } else if(tmpv1$answer[tmpv1$question_nr==19] == 'Ungefär en gång i veckan  '){
    madre[i,'v1.DreamsShare'] <- 6
  } else if(tmpv1$answer[tmpv1$question_nr==19] == 'Flera gånger i veckan '){
    madre[i,'v1.DreamsShare'] <- 7
  } else{
    madre[i,'v1.DreamsShare'] <- NA
  }
  
 
  if(tmpv1$answer[tmpv1$question_nr==20] == 'Aldrig'){
    madre[i,'v1.DreamsRecord'] <- 0
  } else if(tmpv1$answer[tmpv1$question_nr==20] == 'Mindre än en gång per år'){
    madre[i,'v1.DreamsRecord'] <- 1
  } else if(tmpv1$answer[tmpv1$question_nr==20] == 'Ungefär en gång per år'){
    madre[i,'v1.DreamsRecord'] <- 2
  } else if(tmpv1$answer[tmpv1$question_nr==20] == 'Ungefär två till fyra gånger per år'){
    madre[i,'v1.DreamsRecord'] <- 3
  } else if(tmpv1$answer[tmpv1$question_nr==20] == 'Ungefär en gång i månaden'){
    madre[i,'v1.DreamsRecord'] <- 4
  } else if(tmpv1$answer[tmpv1$question_nr==20] == 'Två till tre gånger i månaden'){
    madre[i,'v1.DreamsRecord'] <- 5
  } else if(tmpv1$answer[tmpv1$question_nr==20] == 'Ungefär en gång i veckan  '){
    madre[i,'v1.DreamsRecord'] <- 6
  } else if(tmpv1$answer[tmpv1$question_nr==20] == 'Flera gånger i veckan '){
    madre[i,'v1.DreamsRecord'] <- 7
  } else{
    madre[i,'v1.DreamsRecord'] <- NA
  }
  
  if(tmpv1$answer[tmpv1$question_nr==21] == 'Aldrig'){
    madre[i,'v1.DreamsAffect'] <- 0
  } else if(tmpv1$answer[tmpv1$question_nr==21] == 'Mindre än en gång per år'){
    madre[i,'v1.DreamsAffect'] <- 1
  } else if(tmpv1$answer[tmpv1$question_nr==21] == 'Ungefär en gång per år'){
    madre[i,'v1.DreamsAffect'] <- 2
  } else if(tmpv1$answer[tmpv1$question_nr==21] == 'Ungefär två till fyra gånger per år'){
    madre[i,'v1.DreamsAffect'] <- 3
  } else if(tmpv1$answer[tmpv1$question_nr==21] == 'Ungefär en gång i månaden'){
    madre[i,'v1.DreamsAffect'] <- 4
  } else if(tmpv1$answer[tmpv1$question_nr==21] == 'Två till tre gånger i månaden'){
    madre[i,'v1.DreamsAffect'] <- 5
  } else if(tmpv1$answer[tmpv1$question_nr==21] == 'Ungefär en gång i veckan  '){
    madre[i,'v1.DreamsAffect'] <- 6
  } else if(tmpv1$answer[tmpv1$question_nr==21] == 'Flera gånger i veckan '){
    madre[i,'v1.DreamsAffect'] <- 7
  } else{
    madre[i,'v1.DreamsAffect'] <- NA
  }
  
  if(tmpv1$answer[tmpv1$question_nr==22] == 'Aldrig'){
    madre[i,'v1.DreamsCreative'] <- 0
  } else if(tmpv1$answer[tmpv1$question_nr==22] == 'Mindre än en gång per år'){
    madre[i,'v1.DreamsCreative'] <- 1
  } else if(tmpv1$answer[tmpv1$question_nr==22] == 'Ungefär en gång per år'){
    madre[i,'v1.DreamsCreative'] <- 2
  } else if(tmpv1$answer[tmpv1$question_nr==22] == 'Ungefär två till fyra gånger per år'){
    madre[i,'v1.DreamsCreative'] <- 3
  } else if(tmpv1$answer[tmpv1$question_nr==22] == 'Ungefär en gång i månaden'){
    madre[i,'v1.DreamsCreative'] <- 4
  } else if(tmpv1$answer[tmpv1$question_nr==22] == 'Två till tre gånger i månaden'){
    madre[i,'v1.DreamsCreative'] <- 5
  } else if(tmpv1$answer[tmpv1$question_nr==22] == 'Ungefär en gång i veckan  '){
    madre[i,'v1.DreamsCreative'] <- 6
  } else if(tmpv1$answer[tmpv1$question_nr==22] == 'Flera gånger i veckan '){
    madre[i,'v1.DreamsCreative'] <- 7
  } else{
    madre[i,'v1.DreamsCreative'] <- NA
  }
    
  if(tmpv1$answer[tmpv1$question_nr==23] == 'Aldrig'){
    madre[i,'v1.DreamsProbSolv'] <- 0
  } else if(tmpv1$answer[tmpv1$question_nr==23] == 'Mindre än en gång per år'){
    madre[i,'v1.DreamsProbSolv'] <- 1
  } else if(tmpv1$answer[tmpv1$question_nr==23] == 'Ungefär en gång per år'){
    madre[i,'v1.DreamsProbSolv'] <- 2
  } else if(tmpv1$answer[tmpv1$question_nr==23] == 'Ungefär två till fyra gånger per år'){
    madre[i,'v1.DreamsProbSolv'] <- 3
  } else if(tmpv1$answer[tmpv1$question_nr==23] == 'Ungefär en gång i månaden'){
    madre[i,'v1.DreamsProbSolv'] <- 4
  } else if(tmpv1$answer[tmpv1$question_nr==23] == 'Två till tre gånger i månaden'){
    madre[i,'v1.DreamsProbSolv'] <- 5
  } else if(tmpv1$answer[tmpv1$question_nr==23] == 'Ungefär en gång i veckan  '){
    madre[i,'v1.DreamsProbSolv'] <- 6
  } else if(tmpv1$answer[tmpv1$question_nr==23] == 'Flera gånger i veckan '){
    madre[i,'v1.DreamsProbSolv'] <- 7
  } else{
    madre[i,'v1.DreamsProbSolv'] <- NA
  }
  
  if(tmpv1$answer[tmpv1$question_nr==24] == 'Aldrig'){
    madre[i,'v1.DejaVu'] <- 0
  } else if(tmpv1$answer[tmpv1$question_nr==24] == 'Mindre än en gång per år'){
    madre[i,'v1.DejaVu'] <- 1
  } else if(tmpv1$answer[tmpv1$question_nr==24] == 'Ungefär en gång per år'){
    madre[i,'v1.DejaVu'] <- 2
  } else if(tmpv1$answer[tmpv1$question_nr==24] == 'Ungefär två till fyra gånger per år'){
    madre[i,'v1.DejaVu'] <- 3
  } else if(tmpv1$answer[tmpv1$question_nr==24] == 'Ungefär en gång i månaden'){
    madre[i,'v1.DejaVu'] <- 4
  } else if(tmpv1$answer[tmpv1$question_nr==24] == 'Två till tre gånger i månaden'){
    madre[i,'v1.DejaVu'] <- 5
  } else if(tmpv1$answer[tmpv1$question_nr==24] == 'Ungefär en gång i veckan  '){
    madre[i,'v1.DejaVu'] <- 6
  } else if(tmpv1$answer[tmpv1$question_nr==24] == 'Flera gånger i veckan '){
    madre[i,'v1.DejaVu'] <- 7
  } else{
    madre[i,'v1.DejaVu'] <- NA
  }
  
  if(tmpv1$answer[tmpv1$question_nr==25] == 'Nej'){
    madre[i,'v1.Read'] <- 0
  } else if(tmpv1$answer[tmpv1$question_nr==25] == 'En till två gånger'){
    madre[i,'v1.Read'] <- 1
  } else if(tmpv1$answer[tmpv1$question_nr==25] == 'Flera gånger'){
    madre[i,'v1.Read'] <- 2
  }
  madre[i,'DATE.v1'] <- dates[1]
  
  # Visit 2:
  tmpv2 <- tmp[as.Date(tmp$date) == dates[2],]
  
  # sort(table(qmadre$answer[qmadre$question_nr==1]),T)[1:5]
  
  if(tmpv2$answer[tmpv2$question_nr==0] == 'Aldrig'){
    madre[i,'v2.recall'] <- 0
  } else if(tmpv2$answer[tmpv2$question_nr==0] == 'Mindre än en gång i månaden'){
    madre[i,'v2.recall'] <- 1
  } else if(tmpv2$answer[tmpv2$question_nr==0] == 'Ungefär en gång i månaden'){
    madre[i,'v2.recall'] <- 2
  } else if(tmpv2$answer[tmpv2$question_nr==0] == 'Två eller tre gånger i månaden'){
    madre[i,'v2.recall'] <- 3
  } else if(tmpv2$answer[tmpv2$question_nr==0] == 'Ungefär en gång i veckan  '){
    madre[i,'v2.recall'] <- 4
  } else if(tmpv2$answer[tmpv2$question_nr==0] == 'Flera gånger i veckan '){
    madre[i,'v2.recall'] <- 5
  } else if(tmpv2$answer[tmpv2$question_nr==0] == 'Nästan varje morgon'){
    madre[i,'v2.recall'] <- 6
  } else{
    madre[i,'v2.recall'] <- NA
  }
  
  if(tmpv2$answer[tmpv2$question_nr==1] == 'Inte alls intensiva'){
    madre[i,'v2.EmIntense'] <- 0
  } else if(tmpv2$answer[tmpv2$question_nr==1] == 'Inte så intensiva'){
    madre[i,'v2.EmIntense'] <- 1
  } else if(tmpv2$answer[tmpv2$question_nr==1] == 'Aningen intensiva'){
    madre[i,'v2.EmIntense'] <- 2
  } else if(tmpv2$answer[tmpv2$question_nr==1] == 'Ganska intensiva'){
    madre[i,'v2.EmIntense'] <- 3
  } else if(tmpv2$answer[tmpv2$question_nr==1] == 'Väldigt intensiva'){
    madre[i,'v2.EmIntense'] <- 4
  } else{
    madre[i,'v2.EmIntense'] <- NA
  }
  
  if(tmpv2$answer[tmpv2$question_nr==2] == 'Väldigt negativ'){
    madre[i,'v2.EmTone'] <- -2
  } else if(tmpv2$answer[tmpv2$question_nr==2] == 'Aningen negativ'){
    madre[i,'v2.EmTone'] <- -1
  } else if(tmpv2$answer[tmpv2$question_nr==2] == 'Neutral'){
    madre[i,'v2.EmTone'] <- 0
  } else if(tmpv2$answer[tmpv2$question_nr==2] == 'Aningen positiv'){
    madre[i,'v2.EmTone'] <- 1
  } else if(tmpv2$answer[tmpv2$question_nr==2] == 'Väldigt positiv'){
    madre[i,'v2.EmTone'] <- 2
  } else{
    madre[i,'v2.EmTone'] <- NA
  }
    
    if(tmpv2$answer[tmpv2$question_nr==3] == 'Aldrig'){
      madre[i,'v2.NightmaresFreq'] <- 0
    } else if(tmpv2$answer[tmpv2$question_nr==3] == 'Mindre än en gång per år'){
      madre[i,'v2.NightmaresFreq'] <- 1
    } else if(tmpv2$answer[tmpv2$question_nr==3] == 'Ungefär en gång per år'){
      madre[i,'v2.NightmaresFreq'] <- 2
    } else if(tmpv2$answer[tmpv2$question_nr==3] == 'Ungefär två till fyra gånger per år'){
      madre[i,'v2.NightmaresFreq'] <- 3
    } else if(tmpv2$answer[tmpv2$question_nr==3] == 'Ungefär en gång i månaden'){
      madre[i,'v2.NightmaresFreq'] <- 4
    } else if(tmpv2$answer[tmpv2$question_nr==3] == 'Två till tre gånger i månaden'){
      madre[i,'v2.NightmaresFreq'] <- 5
    } else if(tmpv2$answer[tmpv2$question_nr==3] == 'Ungefär en gång i veckan  '){
      madre[i,'v2.NightmaresFreq'] <- 6
    } else if(tmpv2$answer[tmpv2$question_nr==3] == 'Flera gånger i veckan '){
      madre[i,'v2.NightmaresFreq'] <- 7
    } else{
      madre[i,'v2.NightmaresFreq'] <- NA
    }
    
    if(tmpv2$answer[tmpv2$question_nr==4] == 'Inte alls jobbiga'){
      madre[i,'v2.NightmaresDistress'] <- 0
    } else if(tmpv2$answer[tmpv2$question_nr==4] == 'Inte så jobbiga'){
      madre[i,'v2.NightmaresDistress'] <- 1
    } else if(tmpv2$answer[tmpv2$question_nr==4] == 'Aningen jobbiga'){
      madre[i,'v2.NightmaresDistress'] <- 2
    } else if(tmpv2$answer[tmpv2$question_nr==4] == 'Ganska jobbiga'){
      madre[i,'v2.NightmaresDistress'] <- 3
    } else if(tmpv2$answer[tmpv2$question_nr==4] == 'Väldigt jobbiga'){
      madre[i,'v2.NightmaresDistress'] <- 4
    } else{
      madre[i,'v2.NightmaresDistress'] <- NA
    }
    
    if(tmpv2$answer[tmpv2$question_nr==5] == 'Nej'){
      madre[i,'v2.NightmaresSituation'] <- 0
    } else if(tmpv2$answer[tmpv2$question_nr==5] == 'Ja'){
      madre[i,'v2.NightmaresSituation'] <- 1
    } else{
      madre[i,'v2.NightmaresSituation'] <- NA
    }
    
    madre[i,'v2.NightmaresRecurrPerc'] <- as.numeric(as.vector(tmpv2$answer[tmpv2$question_nr==6]))
    
    if(tmpv2$answer[tmpv2$question_nr==7] == 'Aldrig'){
      madre[i,'v2.NightmaresChildhood'] <- 0
    } else if(tmpv2$answer[tmpv2$question_nr==7] == 'Mindre än en gång per år'){
      madre[i,'v2.NightmaresChildhood'] <- 1
    } else if(tmpv2$answer[tmpv2$question_nr==7] == 'Ungefär en gång per år'){
      madre[i,'v2.NightmaresChildhood'] <- 2
    } else if(tmpv2$answer[tmpv2$question_nr==7] == 'Ungefär två till fyra gånger per år'){
      madre[i,'v2.NightmaresChildhood'] <- 3
    } else if(tmpv2$answer[tmpv2$question_nr==7] == 'Ungefär en gång i månaden'){
      madre[i,'v2.NightmaresChildhood'] <- 4
    } else if(tmpv2$answer[tmpv2$question_nr==7] == 'Två till tre gånger i månaden'){
      madre[i,'v2.NightmaresChildhood'] <- 5
    } else if(tmpv2$answer[tmpv2$question_nr==7] == 'Ungefär en gång i veckan  '){
      madre[i,'v2.NightmaresChildhood'] <- 6
    } else if(tmpv2$answer[tmpv2$question_nr==7] == 'Flera gånger i veckan '){
      madre[i,'v2.NightmaresChildhood'] <- 7
    } else{
      madre[i,'v2.NightmaresChildhood'] <- NA
    }
    
    madre[i,'v2.NightmaresTopics'] <- as.vector(tmpv2$answer[tmpv2$question_nr==8])
    
    if(tmpv2$answer[tmpv2$question_nr==9] == 'Aldrig'){
      madre[i,'v2.LucidDreamsFreq'] <- 0
    } else if(tmpv2$answer[tmpv2$question_nr==9] == 'Mindre än en gång per år'){
      madre[i,'v2.LucidDreamsFreq'] <- 1
    } else if(tmpv2$answer[tmpv2$question_nr==9] == 'Ungefär en gång per år'){
      madre[i,'v2.LucidDreamsFreq'] <- 2
    } else if(tmpv2$answer[tmpv2$question_nr==9] == 'Ungefär två till fyra gånger per år'){
      madre[i,'v2.LucidDreamsFreq'] <- 3
    } else if(tmpv2$answer[tmpv2$question_nr==9] == 'Ungefär en gång i månaden'){
      madre[i,'v2.LucidDreamsFreq'] <- 4
    } else if(tmpv2$answer[tmpv2$question_nr==9] == 'Två till tre gånger i månaden'){
      madre[i,'v2.LucidDreamsFreq'] <- 5
    } else if(tmpv2$answer[tmpv2$question_nr==9] == 'Ungefär en gång i veckan  '){
      madre[i,'v2.LucidDreamsFreq'] <- 6
    } else if(tmpv2$answer[tmpv2$question_nr==9] == 'Flera gånger i veckan '){
      madre[i,'v2.LucidDreamsFreq'] <- 7
    } else{
      madre[i,'v2.LucidDreamsFreq'] <- NA
    }
    
    
    
    madre[i,'v2.LucidDreamsAgeOns'] <- as.numeric(as.vector(tmpv2$answer[tmpv2$question_nr==10]))
    
    if(tmpv2$answer[tmpv2$question_nr==11] == 'Inte alls'){
      madre[i,'v2.DreamsAttitude=11'] <- 0
    } else if(tmpv2$answer[tmpv2$question_nr==11] == 'Inte så mycket'){
      madre[i,'v2.DreamsAttitude=11'] <- 1
    } else if(tmpv2$answer[tmpv2$question_nr==11] == 'Delvis'){
      madre[i,'v2.DreamsAttitude=11'] <- 2
    } else if(tmpv2$answer[tmpv2$question_nr==11] == 'Ganska mycket'){
      madre[i,'v2.DreamsAttitude=11'] <- 3
    } else if(tmpv2$answer[tmpv2$question_nr==11] == 'Väldigt mycket'){
      madre[i,'v2.DreamsAttitude=11'] <- 4
    } else{
      madre[i,'v2.DreamsAttitude=11'] <- NA
    }
    
    if(tmpv2$answer[tmpv2$question_nr==12] == 'Inte alls'){
      madre[i,'v2.DreamsAttitude=12'] <- 0
    } else if(tmpv2$answer[tmpv2$question_nr==12] == 'Inte så mycket'){
      madre[i,'v2.DreamsAttitude=12'] <- 1
    } else if(tmpv2$answer[tmpv2$question_nr==12] == 'Delvis'){
      madre[i,'v2.DreamsAttitude=12'] <- 2
    } else if(tmpv2$answer[tmpv2$question_nr==12] == 'Ganska mycket'){
      madre[i,'v2.DreamsAttitude=12'] <- 3
    } else if(tmpv2$answer[tmpv2$question_nr==12] == 'Väldigt mycket'){
      madre[i,'v2.DreamsAttitude=12'] <- 4
    } else{
      madre[i,'v2.DreamsAttitude=12'] <- NA
    }
    
    if(tmpv2$answer[tmpv2$question_nr==13] == 'Inte alls'){
      madre[i,'v2.DreamsAttitude=13'] <- 0
    } else if(tmpv2$answer[tmpv2$question_nr==13] == 'Inte så mycket'){
      madre[i,'v2.DreamsAttitude=13'] <- 1
    } else if(tmpv2$answer[tmpv2$question_nr==13] == 'Delvis'){
      madre[i,'v2.DreamsAttitude=13'] <- 2
    } else if(tmpv2$answer[tmpv2$question_nr==13] == 'Ganska mycket'){
      madre[i,'v2.DreamsAttitude=13'] <- 3
    } else if(tmpv2$answer[tmpv2$question_nr==13] == 'Väldigt mycket'){
      madre[i,'v2.DreamsAttitude=13'] <- 4
    } else{
      madre[i,'v2.DreamsAttitude=13'] <- NA
    }
    
    if(tmpv2$answer[tmpv2$question_nr==14] == 'Inte alls'){
      madre[i,'v2.DreamsAttitude=14'] <- 0
    } else if(tmpv2$answer[tmpv2$question_nr==14] == 'Inte så mycket'){
      madre[i,'v2.DreamsAttitude=14'] <- 1
    } else if(tmpv2$answer[tmpv2$question_nr==14] == 'Delvis'){
      madre[i,'v2.DreamsAttitude=14'] <- 2
    } else if(tmpv2$answer[tmpv2$question_nr==14] == 'Ganska mycket'){
      madre[i,'v2.DreamsAttitude=14'] <- 3
    } else if(tmpv2$answer[tmpv2$question_nr==14] == 'Väldigt mycket'){
      madre[i,'v2.DreamsAttitude=14'] <- 4
    } else{
      madre[i,'v2.DreamsAttitude=14'] <- NA
    }
    
    if(tmpv2$answer[tmpv2$question_nr==15] == 'Inte alls'){
      madre[i,'v2.DreamsAttitude=15'] <- 0
    } else if(tmpv2$answer[tmpv2$question_nr==15] == 'Inte så mycket'){
      madre[i,'v2.DreamsAttitude=15'] <- 1
    } else if(tmpv2$answer[tmpv2$question_nr==15] == 'Delvis'){
      madre[i,'v2.DreamsAttitude=15'] <- 2
    } else if(tmpv2$answer[tmpv2$question_nr==15] == 'Ganska mycket'){
      madre[i,'v2.DreamsAttitude=15'] <- 3
    } else if(tmpv2$answer[tmpv2$question_nr==15] == 'Väldigt mycket'){
      madre[i,'v2.DreamsAttitude=15'] <- 4
    } else{
      madre[i,'v2.DreamsAttitude=15'] <- NA
    }
    
    if(tmpv2$answer[tmpv2$question_nr==16] == 'Inte alls'){
      madre[i,'v2.DreamsAttitude=16'] <- 0
    } else if(tmpv2$answer[tmpv2$question_nr==16] == 'Inte så mycket'){
      madre[i,'v2.DreamsAttitude=16'] <- 1
    } else if(tmpv2$answer[tmpv2$question_nr==16] == 'Delvis'){
      madre[i,'v2.DreamsAttitude=16'] <- 2
    } else if(tmpv2$answer[tmpv2$question_nr==16] == 'Ganska mycket'){
      madre[i,'v2.DreamsAttitude=16'] <- 3
    } else if(tmpv2$answer[tmpv2$question_nr==16] == 'Väldigt mycket'){
      madre[i,'v2.DreamsAttitude=16'] <- 4
    } else{
      madre[i,'v2.DreamsAttitude=16'] <- NA
    }
    
    if(tmpv2$answer[tmpv2$question_nr==17] == 'Inte alls'){
      madre[i,'v2.DreamsAttitude=17'] <- 0
    } else if(tmpv2$answer[tmpv2$question_nr==17] == 'Inte så mycket'){
      madre[i,'v2.DreamsAttitude=17'] <- 1
    } else if(tmpv2$answer[tmpv2$question_nr==17] == 'Delvis'){
      madre[i,'v2.DreamsAttitude=17'] <- 2
    } else if(tmpv2$answer[tmpv2$question_nr==17] == 'Ganska mycket'){
      madre[i,'v2.DreamsAttitude=17'] <- 3
    } else if(tmpv2$answer[tmpv2$question_nr==17] == 'Väldigt mycket'){
      madre[i,'v2.DreamsAttitude=17'] <- 4
    } else{
      madre[i,'v2.DreamsAttitude=17'] <- NA
    }
    
    if(tmpv2$answer[tmpv2$question_nr==18] == 'Inte alls'){
      madre[i,'v2.DreamsAttitude=18'] <- 0
    } else if(tmpv2$answer[tmpv2$question_nr==18] == 'Inte så mycket'){
      madre[i,'v2.DreamsAttitude=18'] <- 1
    } else if(tmpv2$answer[tmpv2$question_nr==18] == 'Delvis'){
      madre[i,'v2.DreamsAttitude=18'] <- 2
    } else if(tmpv2$answer[tmpv2$question_nr==18] == 'Ganska mycket'){
      madre[i,'v2.DreamsAttitude=18'] <- 3
    } else if(tmpv2$answer[tmpv2$question_nr==18] == 'Väldigt mycket'){
      madre[i,'v2.DreamsAttitude=18'] <- 4
    } else{
      madre[i,'v2.DreamsAttitude=18'] <- NA
    }
    
    madre[i,'v2.DreamsAttitude'] <- madre[i,'v2.DreamsAttitude=11']+madre[i,'v2.DreamsAttitude=12']+
      madre[i,'v2.DreamsAttitude=13']+madre[i,'v2.DreamsAttitude=14']+madre[i,'v2.DreamsAttitude=15']+
      madre[i,'v2.DreamsAttitude=16']+madre[i,'v2.DreamsAttitude=17']+madre[i,'v2.DreamsAttitude=18']
    
    if(tmpv2$answer[tmpv2$question_nr==19] == 'Aldrig'){
      madre[i,'v2.DreamsShare'] <- 0
    } else if(tmpv2$answer[tmpv2$question_nr==19] == 'Mindre än en gång per år'){
      madre[i,'v2.DreamsShare'] <- 1
    } else if(tmpv2$answer[tmpv2$question_nr==19] == 'Ungefär en gång per år'){
      madre[i,'v2.DreamsShare'] <- 2
    } else if(tmpv2$answer[tmpv2$question_nr==19] == 'Ungefär två till fyra gånger per år'){
      madre[i,'v2.DreamsShare'] <- 3
    } else if(tmpv2$answer[tmpv2$question_nr==19] == 'Ungefär en gång i månaden'){
      madre[i,'v2.DreamsShare'] <- 4
    } else if(tmpv2$answer[tmpv2$question_nr==19] == 'Två till tre gånger i månaden'){
      madre[i,'v2.DreamsShare'] <- 5
    } else if(tmpv2$answer[tmpv2$question_nr==19] == 'Ungefär en gång i veckan  '){
      madre[i,'v2.DreamsShare'] <- 6
    } else if(tmpv2$answer[tmpv2$question_nr==19] == 'Flera gånger i veckan '){
      madre[i,'v2.DreamsShare'] <- 7
    } else{
      madre[i,'v2.DreamsShare'] <- NA
    }
    
    
    if(tmpv2$answer[tmpv2$question_nr==20] == 'Aldrig'){
      madre[i,'v2.DreamsRecord'] <- 0
    } else if(tmpv2$answer[tmpv2$question_nr==20] == 'Mindre än en gång per år'){
      madre[i,'v2.DreamsRecord'] <- 1
    } else if(tmpv2$answer[tmpv2$question_nr==20] == 'Ungefär en gång per år'){
      madre[i,'v2.DreamsRecord'] <- 2
    } else if(tmpv2$answer[tmpv2$question_nr==20] == 'Ungefär två till fyra gånger per år'){
      madre[i,'v2.DreamsRecord'] <- 3
    } else if(tmpv2$answer[tmpv2$question_nr==20] == 'Ungefär en gång i månaden'){
      madre[i,'v2.DreamsRecord'] <- 4
    } else if(tmpv2$answer[tmpv2$question_nr==20] == 'Två till tre gånger i månaden'){
      madre[i,'v2.DreamsRecord'] <- 5
    } else if(tmpv2$answer[tmpv2$question_nr==20] == 'Ungefär en gång i veckan  '){
      madre[i,'v2.DreamsRecord'] <- 6
    } else if(tmpv2$answer[tmpv2$question_nr==20] == 'Flera gånger i veckan '){
      madre[i,'v2.DreamsRecord'] <- 7
    } else{
      madre[i,'v2.DreamsRecord'] <- NA
    }
    
    if(tmpv2$answer[tmpv2$question_nr==21] == 'Aldrig'){
      madre[i,'v2.DreamsAffect'] <- 0
    } else if(tmpv2$answer[tmpv2$question_nr==21] == 'Mindre än en gång per år'){
      madre[i,'v2.DreamsAffect'] <- 1
    } else if(tmpv2$answer[tmpv2$question_nr==21] == 'Ungefär en gång per år'){
      madre[i,'v2.DreamsAffect'] <- 2
    } else if(tmpv2$answer[tmpv2$question_nr==21] == 'Ungefär två till fyra gånger per år'){
      madre[i,'v2.DreamsAffect'] <- 3
    } else if(tmpv2$answer[tmpv2$question_nr==21] == 'Ungefär en gång i månaden'){
      madre[i,'v2.DreamsAffect'] <- 4
    } else if(tmpv2$answer[tmpv2$question_nr==21] == 'Två till tre gånger i månaden'){
      madre[i,'v2.DreamsAffect'] <- 5
    } else if(tmpv2$answer[tmpv2$question_nr==21] == 'Ungefär en gång i veckan  '){
      madre[i,'v2.DreamsAffect'] <- 6
    } else if(tmpv2$answer[tmpv2$question_nr==21] == 'Flera gånger i veckan '){
      madre[i,'v2.DreamsAffect'] <- 7
    } else{
      madre[i,'v2.DreamsAffect'] <- NA
    }
    
    if(tmpv2$answer[tmpv2$question_nr==22] == 'Aldrig'){
      madre[i,'v2.DreamsCreative'] <- 0
    } else if(tmpv2$answer[tmpv2$question_nr==22] == 'Mindre än en gång per år'){
      madre[i,'v2.DreamsCreative'] <- 1
    } else if(tmpv2$answer[tmpv2$question_nr==22] == 'Ungefär en gång per år'){
      madre[i,'v2.DreamsCreative'] <- 2
    } else if(tmpv2$answer[tmpv2$question_nr==22] == 'Ungefär två till fyra gånger per år'){
      madre[i,'v2.DreamsCreative'] <- 3
    } else if(tmpv2$answer[tmpv2$question_nr==22] == 'Ungefär en gång i månaden'){
      madre[i,'v2.DreamsCreative'] <- 4
    } else if(tmpv2$answer[tmpv2$question_nr==22] == 'Två till tre gånger i månaden'){
      madre[i,'v2.DreamsCreative'] <- 5
    } else if(tmpv2$answer[tmpv2$question_nr==22] == 'Ungefär en gång i veckan  '){
      madre[i,'v2.DreamsCreative'] <- 6
    } else if(tmpv2$answer[tmpv2$question_nr==22] == 'Flera gånger i veckan '){
      madre[i,'v2.DreamsCreative'] <- 7
    } else{
      madre[i,'v2.DreamsCreative'] <- NA
    }
    
    if(tmpv2$answer[tmpv2$question_nr==23] == 'Aldrig'){
      madre[i,'v2.DreamsProbSolv'] <- 0
    } else if(tmpv2$answer[tmpv2$question_nr==23] == 'Mindre än en gång per år'){
      madre[i,'v2.DreamsProbSolv'] <- 1
    } else if(tmpv2$answer[tmpv2$question_nr==23] == 'Ungefär en gång per år'){
      madre[i,'v2.DreamsProbSolv'] <- 2
    } else if(tmpv2$answer[tmpv2$question_nr==23] == 'Ungefär två till fyra gånger per år'){
      madre[i,'v2.DreamsProbSolv'] <- 3
    } else if(tmpv2$answer[tmpv2$question_nr==23] == 'Ungefär en gång i månaden'){
      madre[i,'v2.DreamsProbSolv'] <- 4
    } else if(tmpv2$answer[tmpv2$question_nr==23] == 'Två till tre gånger i månaden'){
      madre[i,'v2.DreamsProbSolv'] <- 5
    } else if(tmpv2$answer[tmpv2$question_nr==23] == 'Ungefär en gång i veckan  '){
      madre[i,'v2.DreamsProbSolv'] <- 6
    } else if(tmpv2$answer[tmpv2$question_nr==23] == 'Flera gånger i veckan '){
      madre[i,'v2.DreamsProbSolv'] <- 7
    } else{
      madre[i,'v2.DreamsProbSolv'] <- NA
    }
    
    if(tmpv2$answer[tmpv2$question_nr==24] == 'Aldrig'){
      madre[i,'v2.DejaVu'] <- 0
    } else if(tmpv2$answer[tmpv2$question_nr==24] == 'Mindre än en gång per år'){
      madre[i,'v2.DejaVu'] <- 1
    } else if(tmpv2$answer[tmpv2$question_nr==24] == 'Ungefär en gång per år'){
      madre[i,'v2.DejaVu'] <- 2
    } else if(tmpv2$answer[tmpv2$question_nr==24] == 'Ungefär två till fyra gånger per år'){
      madre[i,'v2.DejaVu'] <- 3
    } else if(tmpv2$answer[tmpv2$question_nr==24] == 'Ungefär en gång i månaden'){
      madre[i,'v2.DejaVu'] <- 4
    } else if(tmpv2$answer[tmpv2$question_nr==24] == 'Två till tre gånger i månaden'){
      madre[i,'v2.DejaVu'] <- 5
    } else if(tmpv2$answer[tmpv2$question_nr==24] == 'Ungefär en gång i veckan  '){
      madre[i,'v2.DejaVu'] <- 6
    } else if(tmpv2$answer[tmpv2$question_nr==24] == 'Flera gånger i veckan '){
      madre[i,'v2.DejaVu'] <- 7
    } else{
      madre[i,'v2.DejaVu'] <- NA
    }
    
    if(tmpv2$answer[tmpv2$question_nr==25] == 'Nej'){
      madre[i,'v2.Read'] <- 0
    } else if(tmpv2$answer[tmpv2$question_nr==25] == 'En till två gånger'){
      madre[i,'v2.Read'] <- 1
    } else if(tmpv2$answer[tmpv2$question_nr==25] == 'Flera gånger'){
      madre[i,'v2.Read'] <- 2
    }
  
  madre[i,'DATE.v2'] <- dates[2]
  madre[i,'DATE-diff12'] <- as.numeric(dates[2]-dates[1])
  
  if (length(dates)==3){
    # Visit 3:
    tmpv3 <- tmp[as.Date(tmp$date) == dates[3],]
    
    # sort(table(qmadre$answer[qmadre$question_nr==1]),T)[1:5]
    
    if(tmpv3$answer[tmpv3$question_nr==0] == 'Aldrig'){
      madre[i,'v3.recall'] <- 0
    } else if(tmpv3$answer[tmpv3$question_nr==0] == 'Mindre än en gång i månaden'){
      madre[i,'v3.recall'] <- 1
    } else if(tmpv3$answer[tmpv3$question_nr==0] == 'Ungefär en gång i månaden'){
      madre[i,'v3.recall'] <- 2
    } else if(tmpv3$answer[tmpv3$question_nr==0] == 'Två eller tre gånger i månaden'){
      madre[i,'v3.recall'] <- 3
    } else if(tmpv3$answer[tmpv3$question_nr==0] == 'Ungefär en gång i veckan  '){
      madre[i,'v3.recall'] <- 4
    } else if(tmpv3$answer[tmpv3$question_nr==0] == 'Flera gånger i veckan '){
      madre[i,'v3.recall'] <- 5
    } else if(tmpv3$answer[tmpv3$question_nr==0] == 'Nästan varje morgon'){
      madre[i,'v3.recall'] <- 6
    } else{
      madre[i,'v3.recall'] <- NA
    }
    
    if(tmpv3$answer[tmpv3$question_nr==1] == 'Inte alls intensiva'){
      madre[i,'v3.EmIntense'] <- 0
    } else if(tmpv3$answer[tmpv3$question_nr==1] == 'Inte så intensiva'){
      madre[i,'v3.EmIntense'] <- 1
    } else if(tmpv3$answer[tmpv3$question_nr==1] == 'Aningen intensiva'){
      madre[i,'v3.EmIntense'] <- 2
    } else if(tmpv3$answer[tmpv3$question_nr==1] == 'Ganska intensiva'){
      madre[i,'v3.EmIntense'] <- 3
    } else if(tmpv3$answer[tmpv3$question_nr==1] == 'Väldigt intensiva'){
      madre[i,'v3.EmIntense'] <- 4
    } else{
      madre[i,'v3.EmIntense'] <- NA
    }
    
    if(tmpv3$answer[tmpv3$question_nr==2] == 'Väldigt negativ'){
      madre[i,'v3.EmTone'] <- -2
    } else if(tmpv3$answer[tmpv3$question_nr==2] == 'Aningen negativ'){
      madre[i,'v3.EmTone'] <- -1
    } else if(tmpv3$answer[tmpv3$question_nr==2] == 'Neutral'){
      madre[i,'v3.EmTone'] <- 0
    } else if(tmpv3$answer[tmpv3$question_nr==2] == 'Aningen positiv'){
      madre[i,'v3.EmTone'] <- 1
    } else if(tmpv3$answer[tmpv3$question_nr==2] == 'Väldigt positiv'){
      madre[i,'v3.EmTone'] <- 2
    } else{
      madre[i,'v3.EmTone'] <- NA
    }
      
      if(tmpv3$answer[tmpv3$question_nr==3] == 'Aldrig'){
        madre[i,'v3.NightmaresFreq'] <- 0
      } else if(tmpv3$answer[tmpv3$question_nr==3] == 'Mindre än en gång per år'){
        madre[i,'v3.NightmaresFreq'] <- 1
      } else if(tmpv3$answer[tmpv3$question_nr==3] == 'Ungefär en gång per år'){
        madre[i,'v3.NightmaresFreq'] <- 2
      } else if(tmpv3$answer[tmpv3$question_nr==3] == 'Ungefär två till fyra gånger per år'){
        madre[i,'v3.NightmaresFreq'] <- 3
      } else if(tmpv3$answer[tmpv3$question_nr==3] == 'Ungefär en gång i månaden'){
        madre[i,'v3.NightmaresFreq'] <- 4
      } else if(tmpv3$answer[tmpv3$question_nr==3] == 'Två till tre gånger i månaden'){
        madre[i,'v3.NightmaresFreq'] <- 5
      } else if(tmpv3$answer[tmpv3$question_nr==3] == 'Ungefär en gång i veckan  '){
        madre[i,'v3.NightmaresFreq'] <- 6
      } else if(tmpv3$answer[tmpv3$question_nr==3] == 'Flera gånger i veckan '){
        madre[i,'v3.NightmaresFreq'] <- 7
      } else{
        madre[i,'v3.NightmaresFreq'] <- NA
      }
      
      if(tmpv3$answer[tmpv3$question_nr==4] == 'Inte alls jobbiga'){
        madre[i,'v3.NightmaresDistress'] <- 0
      } else if(tmpv3$answer[tmpv3$question_nr==4] == 'Inte så jobbiga'){
        madre[i,'v3.NightmaresDistress'] <- 1
      } else if(tmpv3$answer[tmpv3$question_nr==4] == 'Aningen jobbiga'){
        madre[i,'v3.NightmaresDistress'] <- 2
      } else if(tmpv3$answer[tmpv3$question_nr==4] == 'Ganska jobbiga'){
        madre[i,'v3.NightmaresDistress'] <- 3
      } else if(tmpv3$answer[tmpv3$question_nr==4] == 'Väldigt jobbiga'){
        madre[i,'v3.NightmaresDistress'] <- 4
      } else{
        madre[i,'v3.NightmaresDistress'] <- NA
      }
      
      if(tmpv3$answer[tmpv3$question_nr==5] == 'Nej'){
        madre[i,'v3.NightmaresSituation'] <- 0
      } else if(tmpv3$answer[tmpv3$question_nr==5] == 'Ja'){
        madre[i,'v3.NightmaresSituation'] <- 1
      } else{
        madre[i,'v3.NightmaresSituation'] <- NA
      }
      
      madre[i,'v3.NightmaresRecurrPerc'] <- as.numeric(as.vector(tmpv3$answer[tmpv3$question_nr==6]))
      
      if(tmpv3$answer[tmpv3$question_nr==7] == 'Aldrig'){
        madre[i,'v3.NightmaresChildhood'] <- 0
      } else if(tmpv3$answer[tmpv3$question_nr==7] == 'Mindre än en gång per år'){
        madre[i,'v3.NightmaresChildhood'] <- 1
      } else if(tmpv3$answer[tmpv3$question_nr==7] == 'Ungefär en gång per år'){
        madre[i,'v3.NightmaresChildhood'] <- 2
      } else if(tmpv3$answer[tmpv3$question_nr==7] == 'Ungefär två till fyra gånger per år'){
        madre[i,'v3.NightmaresChildhood'] <- 3
      } else if(tmpv3$answer[tmpv3$question_nr==7] == 'Ungefär en gång i månaden'){
        madre[i,'v3.NightmaresChildhood'] <- 4
      } else if(tmpv3$answer[tmpv3$question_nr==7] == 'Två till tre gånger i månaden'){
        madre[i,'v3.NightmaresChildhood'] <- 5
      } else if(tmpv3$answer[tmpv3$question_nr==7] == 'Ungefär en gång i veckan  '){
        madre[i,'v3.NightmaresChildhood'] <- 6
      } else if(tmpv3$answer[tmpv3$question_nr==7] == 'Flera gånger i veckan '){
        madre[i,'v3.NightmaresChildhood'] <- 7
      } else{
        madre[i,'v3.NightmaresChildhood'] <- NA
      }
      
      madre[i,'v3.NightmaresTopics'] <- as.vector(tmpv3$answer[tmpv3$question_nr==8])
      
      if(tmpv3$answer[tmpv3$question_nr==9] == 'Aldrig'){
        madre[i,'v3.LucidDreamsFreq'] <- 0
      } else if(tmpv3$answer[tmpv3$question_nr==9] == 'Mindre än en gång per år'){
        madre[i,'v3.LucidDreamsFreq'] <- 1
      } else if(tmpv3$answer[tmpv3$question_nr==9] == 'Ungefär en gång per år'){
        madre[i,'v3.LucidDreamsFreq'] <- 2
      } else if(tmpv3$answer[tmpv3$question_nr==9] == 'Ungefär två till fyra gånger per år'){
        madre[i,'v3.LucidDreamsFreq'] <- 3
      } else if(tmpv3$answer[tmpv3$question_nr==9] == 'Ungefär en gång i månaden'){
        madre[i,'v3.LucidDreamsFreq'] <- 4
      } else if(tmpv3$answer[tmpv3$question_nr==9] == 'Två till tre gånger i månaden'){
        madre[i,'v3.LucidDreamsFreq'] <- 5
      } else if(tmpv3$answer[tmpv3$question_nr==9] == 'Ungefär en gång i veckan  '){
        madre[i,'v3.LucidDreamsFreq'] <- 6
      } else if(tmpv3$answer[tmpv3$question_nr==9] == 'Flera gånger i veckan '){
        madre[i,'v3.LucidDreamsFreq'] <- 7
      } else{
        madre[i,'v3.LucidDreamsFreq'] <- NA
      }
      
      
      
      madre[i,'v3.LucidDreamsAgeOns'] <- as.numeric(as.vector(tmpv3$answer[tmpv3$question_nr==10]))
      
      if(tmpv3$answer[tmpv3$question_nr==11] == 'Inte alls'){
        madre[i,'v3.DreamsAttitude=11'] <- 0
      } else if(tmpv3$answer[tmpv3$question_nr==11] == 'Inte så mycket'){
        madre[i,'v3.DreamsAttitude=11'] <- 1
      } else if(tmpv3$answer[tmpv3$question_nr==11] == 'Delvis'){
        madre[i,'v3.DreamsAttitude=11'] <- 2
      } else if(tmpv3$answer[tmpv3$question_nr==11] == 'Ganska mycket'){
        madre[i,'v3.DreamsAttitude=11'] <- 3
      } else if(tmpv3$answer[tmpv3$question_nr==11] == 'Väldigt mycket'){
        madre[i,'v3.DreamsAttitude=11'] <- 4
      } else{
        madre[i,'v3.DreamsAttitude=11'] <- NA
      }
      
      if(tmpv3$answer[tmpv3$question_nr==12] == 'Inte alls'){
        madre[i,'v3.DreamsAttitude=12'] <- 0
      } else if(tmpv3$answer[tmpv3$question_nr==12] == 'Inte så mycket'){
        madre[i,'v3.DreamsAttitude=12'] <- 1
      } else if(tmpv3$answer[tmpv3$question_nr==12] == 'Delvis'){
        madre[i,'v3.DreamsAttitude=12'] <- 2
      } else if(tmpv3$answer[tmpv3$question_nr==12] == 'Ganska mycket'){
        madre[i,'v3.DreamsAttitude=12'] <- 3
      } else if(tmpv3$answer[tmpv3$question_nr==12] == 'Väldigt mycket'){
        madre[i,'v3.DreamsAttitude=12'] <- 4
      } else{
        madre[i,'v3.DreamsAttitude=12'] <- NA
      }
      
      if(tmpv3$answer[tmpv3$question_nr==13] == 'Inte alls'){
        madre[i,'v3.DreamsAttitude=13'] <- 0
      } else if(tmpv3$answer[tmpv3$question_nr==13] == 'Inte så mycket'){
        madre[i,'v3.DreamsAttitude=13'] <- 1
      } else if(tmpv3$answer[tmpv3$question_nr==13] == 'Delvis'){
        madre[i,'v3.DreamsAttitude=13'] <- 2
      } else if(tmpv3$answer[tmpv3$question_nr==13] == 'Ganska mycket'){
        madre[i,'v3.DreamsAttitude=13'] <- 3
      } else if(tmpv3$answer[tmpv3$question_nr==13] == 'Väldigt mycket'){
        madre[i,'v3.DreamsAttitude=13'] <- 4
      } else{
        madre[i,'v3.DreamsAttitude=13'] <- NA
      }
      
      if(tmpv3$answer[tmpv3$question_nr==14] == 'Inte alls'){
        madre[i,'v3.DreamsAttitude=14'] <- 0
      } else if(tmpv3$answer[tmpv3$question_nr==14] == 'Inte så mycket'){
        madre[i,'v3.DreamsAttitude=14'] <- 1
      } else if(tmpv3$answer[tmpv3$question_nr==14] == 'Delvis'){
        madre[i,'v3.DreamsAttitude=14'] <- 2
      } else if(tmpv3$answer[tmpv3$question_nr==14] == 'Ganska mycket'){
        madre[i,'v3.DreamsAttitude=14'] <- 3
      } else if(tmpv3$answer[tmpv3$question_nr==14] == 'Väldigt mycket'){
        madre[i,'v3.DreamsAttitude=14'] <- 4
      } else{
        madre[i,'v3.DreamsAttitude=14'] <- NA
      }
      
      if(tmpv3$answer[tmpv3$question_nr==15] == 'Inte alls'){
        madre[i,'v3.DreamsAttitude=15'] <- 0
      } else if(tmpv3$answer[tmpv3$question_nr==15] == 'Inte så mycket'){
        madre[i,'v3.DreamsAttitude=15'] <- 1
      } else if(tmpv3$answer[tmpv3$question_nr==15] == 'Delvis'){
        madre[i,'v3.DreamsAttitude=15'] <- 2
      } else if(tmpv3$answer[tmpv3$question_nr==15] == 'Ganska mycket'){
        madre[i,'v3.DreamsAttitude=15'] <- 3
      } else if(tmpv3$answer[tmpv3$question_nr==15] == 'Väldigt mycket'){
        madre[i,'v3.DreamsAttitude=15'] <- 4
      } else{
        madre[i,'v3.DreamsAttitude=15'] <- NA
      }
      
      if(tmpv3$answer[tmpv3$question_nr==16] == 'Inte alls'){
        madre[i,'v3.DreamsAttitude=16'] <- 0
      } else if(tmpv3$answer[tmpv3$question_nr==16] == 'Inte så mycket'){
        madre[i,'v3.DreamsAttitude=16'] <- 1
      } else if(tmpv3$answer[tmpv3$question_nr==16] == 'Delvis'){
        madre[i,'v3.DreamsAttitude=16'] <- 2
      } else if(tmpv3$answer[tmpv3$question_nr==16] == 'Ganska mycket'){
        madre[i,'v3.DreamsAttitude=16'] <- 3
      } else if(tmpv3$answer[tmpv3$question_nr==16] == 'Väldigt mycket'){
        madre[i,'v3.DreamsAttitude=16'] <- 4
      } else{
        madre[i,'v3.DreamsAttitude=16'] <- NA
      }
      
      if(tmpv3$answer[tmpv3$question_nr==17] == 'Inte alls'){
        madre[i,'v3.DreamsAttitude=17'] <- 0
      } else if(tmpv3$answer[tmpv3$question_nr==17] == 'Inte så mycket'){
        madre[i,'v3.DreamsAttitude=17'] <- 1
      } else if(tmpv3$answer[tmpv3$question_nr==17] == 'Delvis'){
        madre[i,'v3.DreamsAttitude=17'] <- 2
      } else if(tmpv3$answer[tmpv3$question_nr==17] == 'Ganska mycket'){
        madre[i,'v3.DreamsAttitude=17'] <- 3
      } else if(tmpv3$answer[tmpv3$question_nr==17] == 'Väldigt mycket'){
        madre[i,'v3.DreamsAttitude=17'] <- 4
      } else{
        madre[i,'v3.DreamsAttitude=17'] <- NA
      }
      
      if(tmpv3$answer[tmpv3$question_nr==18] == 'Inte alls'){
        madre[i,'v3.DreamsAttitude=18'] <- 0
      } else if(tmpv3$answer[tmpv3$question_nr==18] == 'Inte så mycket'){
        madre[i,'v3.DreamsAttitude=18'] <- 1
      } else if(tmpv3$answer[tmpv3$question_nr==18] == 'Delvis'){
        madre[i,'v3.DreamsAttitude=18'] <- 2
      } else if(tmpv3$answer[tmpv3$question_nr==18] == 'Ganska mycket'){
        madre[i,'v3.DreamsAttitude=18'] <- 3
      } else if(tmpv3$answer[tmpv3$question_nr==18] == 'Väldigt mycket'){
        madre[i,'v3.DreamsAttitude=18'] <- 4
      } else{
        madre[i,'v3.DreamsAttitude=18'] <- NA
      }
      
      madre[i,'v3.DreamsAttitude'] <- madre[i,'v3.DreamsAttitude=11']+madre[i,'v3.DreamsAttitude=12']+
        madre[i,'v3.DreamsAttitude=13']+madre[i,'v3.DreamsAttitude=14']+madre[i,'v3.DreamsAttitude=15']+
        madre[i,'v3.DreamsAttitude=16']+madre[i,'v3.DreamsAttitude=17']+madre[i,'v3.DreamsAttitude=18']
      
      if(tmpv3$answer[tmpv3$question_nr==19] == 'Aldrig'){
        madre[i,'v3.DreamsShare'] <- 0
      } else if(tmpv3$answer[tmpv3$question_nr==19] == 'Mindre än en gång per år'){
        madre[i,'v3.DreamsShare'] <- 1
      } else if(tmpv3$answer[tmpv3$question_nr==19] == 'Ungefär en gång per år'){
        madre[i,'v3.DreamsShare'] <- 2
      } else if(tmpv3$answer[tmpv3$question_nr==19] == 'Ungefär två till fyra gånger per år'){
        madre[i,'v3.DreamsShare'] <- 3
      } else if(tmpv3$answer[tmpv3$question_nr==19] == 'Ungefär en gång i månaden'){
        madre[i,'v3.DreamsShare'] <- 4
      } else if(tmpv3$answer[tmpv3$question_nr==19] == 'Två till tre gånger i månaden'){
        madre[i,'v3.DreamsShare'] <- 5
      } else if(tmpv3$answer[tmpv3$question_nr==19] == 'Ungefär en gång i veckan  '){
        madre[i,'v3.DreamsShare'] <- 6
      } else if(tmpv3$answer[tmpv3$question_nr==19] == 'Flera gånger i veckan '){
        madre[i,'v3.DreamsShare'] <- 7
      } else{
        madre[i,'v3.DreamsShare'] <- NA
      }
      
      
      if(tmpv3$answer[tmpv3$question_nr==20] == 'Aldrig'){
        madre[i,'v3.DreamsRecord'] <- 0
      } else if(tmpv3$answer[tmpv3$question_nr==20] == 'Mindre än en gång per år'){
        madre[i,'v3.DreamsRecord'] <- 1
      } else if(tmpv3$answer[tmpv3$question_nr==20] == 'Ungefär en gång per år'){
        madre[i,'v3.DreamsRecord'] <- 2
      } else if(tmpv3$answer[tmpv3$question_nr==20] == 'Ungefär två till fyra gånger per år'){
        madre[i,'v3.DreamsRecord'] <- 3
      } else if(tmpv3$answer[tmpv3$question_nr==20] == 'Ungefär en gång i månaden'){
        madre[i,'v3.DreamsRecord'] <- 4
      } else if(tmpv3$answer[tmpv3$question_nr==20] == 'Två till tre gånger i månaden'){
        madre[i,'v3.DreamsRecord'] <- 5
      } else if(tmpv3$answer[tmpv3$question_nr==20] == 'Ungefär en gång i veckan  '){
        madre[i,'v3.DreamsRecord'] <- 6
      } else if(tmpv3$answer[tmpv3$question_nr==20] == 'Flera gånger i veckan '){
        madre[i,'v3.DreamsRecord'] <- 7
      } else{
        madre[i,'v3.DreamsRecord'] <- NA
      }
      
      if(tmpv3$answer[tmpv3$question_nr==21] == 'Aldrig'){
        madre[i,'v3.DreamsAffect'] <- 0
      } else if(tmpv3$answer[tmpv3$question_nr==21] == 'Mindre än en gång per år'){
        madre[i,'v3.DreamsAffect'] <- 1
      } else if(tmpv3$answer[tmpv3$question_nr==21] == 'Ungefär en gång per år'){
        madre[i,'v3.DreamsAffect'] <- 2
      } else if(tmpv3$answer[tmpv3$question_nr==21] == 'Ungefär två till fyra gånger per år'){
        madre[i,'v3.DreamsAffect'] <- 3
      } else if(tmpv3$answer[tmpv3$question_nr==21] == 'Ungefär en gång i månaden'){
        madre[i,'v3.DreamsAffect'] <- 4
      } else if(tmpv3$answer[tmpv3$question_nr==21] == 'Två till tre gånger i månaden'){
        madre[i,'v3.DreamsAffect'] <- 5
      } else if(tmpv3$answer[tmpv3$question_nr==21] == 'Ungefär en gång i veckan  '){
        madre[i,'v3.DreamsAffect'] <- 6
      } else if(tmpv3$answer[tmpv3$question_nr==21] == 'Flera gånger i veckan '){
        madre[i,'v3.DreamsAffect'] <- 7
      } else{
        madre[i,'v3.DreamsAffect'] <- NA
      }
      
      if(tmpv3$answer[tmpv3$question_nr==22] == 'Aldrig'){
        madre[i,'v3.DreamsCreative'] <- 0
      } else if(tmpv3$answer[tmpv3$question_nr==22] == 'Mindre än en gång per år'){
        madre[i,'v3.DreamsCreative'] <- 1
      } else if(tmpv3$answer[tmpv3$question_nr==22] == 'Ungefär en gång per år'){
        madre[i,'v3.DreamsCreative'] <- 2
      } else if(tmpv3$answer[tmpv3$question_nr==22] == 'Ungefär två till fyra gånger per år'){
        madre[i,'v3.DreamsCreative'] <- 3
      } else if(tmpv3$answer[tmpv3$question_nr==22] == 'Ungefär en gång i månaden'){
        madre[i,'v3.DreamsCreative'] <- 4
      } else if(tmpv3$answer[tmpv3$question_nr==22] == 'Två till tre gånger i månaden'){
        madre[i,'v3.DreamsCreative'] <- 5
      } else if(tmpv3$answer[tmpv3$question_nr==22] == 'Ungefär en gång i veckan  '){
        madre[i,'v3.DreamsCreative'] <- 6
      } else if(tmpv3$answer[tmpv3$question_nr==22] == 'Flera gånger i veckan '){
        madre[i,'v3.DreamsCreative'] <- 7
      } else{
        madre[i,'v3.DreamsCreative'] <- NA
      }
      
      if(tmpv3$answer[tmpv3$question_nr==23] == 'Aldrig'){
        madre[i,'v3.DreamsProbSolv'] <- 0
      } else if(tmpv3$answer[tmpv3$question_nr==23] == 'Mindre än en gång per år'){
        madre[i,'v3.DreamsProbSolv'] <- 1
      } else if(tmpv3$answer[tmpv3$question_nr==23] == 'Ungefär en gång per år'){
        madre[i,'v3.DreamsProbSolv'] <- 2
      } else if(tmpv3$answer[tmpv3$question_nr==23] == 'Ungefär två till fyra gånger per år'){
        madre[i,'v3.DreamsProbSolv'] <- 3
      } else if(tmpv3$answer[tmpv3$question_nr==23] == 'Ungefär en gång i månaden'){
        madre[i,'v3.DreamsProbSolv'] <- 4
      } else if(tmpv3$answer[tmpv3$question_nr==23] == 'Två till tre gånger i månaden'){
        madre[i,'v3.DreamsProbSolv'] <- 5
      } else if(tmpv3$answer[tmpv3$question_nr==23] == 'Ungefär en gång i veckan  '){
        madre[i,'v3.DreamsProbSolv'] <- 6
      } else if(tmpv3$answer[tmpv3$question_nr==23] == 'Flera gånger i veckan '){
        madre[i,'v3.DreamsProbSolv'] <- 7
      } else{
        madre[i,'v3.DreamsProbSolv'] <- NA
      }
      
      if(tmpv3$answer[tmpv3$question_nr==24] == 'Aldrig'){
        madre[i,'v3.DejaVu'] <- 0
      } else if(tmpv3$answer[tmpv3$question_nr==24] == 'Mindre än en gång per år'){
        madre[i,'v3.DejaVu'] <- 1
      } else if(tmpv3$answer[tmpv3$question_nr==24] == 'Ungefär en gång per år'){
        madre[i,'v3.DejaVu'] <- 2
      } else if(tmpv3$answer[tmpv3$question_nr==24] == 'Ungefär två till fyra gånger per år'){
        madre[i,'v3.DejaVu'] <- 3
      } else if(tmpv3$answer[tmpv3$question_nr==24] == 'Ungefär en gång i månaden'){
        madre[i,'v3.DejaVu'] <- 4
      } else if(tmpv3$answer[tmpv3$question_nr==24] == 'Två till tre gånger i månaden'){
        madre[i,'v3.DejaVu'] <- 5
      } else if(tmpv3$answer[tmpv3$question_nr==24] == 'Ungefär en gång i veckan  '){
        madre[i,'v3.DejaVu'] <- 6
      } else if(tmpv3$answer[tmpv3$question_nr==24] == 'Flera gånger i veckan '){
        madre[i,'v3.DejaVu'] <- 7
      } else{
        madre[i,'v3.DejaVu'] <- NA
      }
      
      if(tmpv3$answer[tmpv3$question_nr==25] == 'Nej'){
        madre[i,'v3.Read'] <- 0
      } else if(tmpv3$answer[tmpv3$question_nr==25] == 'En till två gånger'){
        madre[i,'v3.Read'] <- 1
      } else if(tmpv3$answer[tmpv3$question_nr==25] == 'Flera gånger'){
        madre[i,'v3.Read'] <- 2
      }
    madre[i,'DATE.v3'] <- dates[3]
    madre[i,'DATE-diff23'] <- as.numeric(dates[3]-dates[2])
  }
}

write.xlsx(madre, paste(dir, 'summary/', 'madre', '.xlsx', sep=''), row.names = F)
rm(madre)

# CEQ:
qceq <- read.csv2(paste(dir, 'questioner_ceq.csv', sep=''),sep=';')
qceq <- qceq[which(nchar(qceq$id)>3),]
qceq$question_nr <- qceq$question_nr-1
ids <- names(table(qceq$id))
ceq <- as.data.frame(matrix(NA, length(ids), 9))

colnames(ceq) <- c('ID',
                   'v1.score','v2.score','v3.score', 
                   'DATE.v1', 'DATE.v2', 'DATE.v3','DATE-diff12', 'DATE-diff23')

  


for (i in 1:length(ids)) {
  tmp <- subset(qceq, qceq$id==ids[i])
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  
  ceq[i,'ID'] <- ids[i]
  
  # Visit 1:
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  ceq[i,'v1.score'] <- sum(tmpv1$answer==1)
  ceq[i,'numR.v1'] <- dim(tmpv1)[1]
  ceq[i,'DATE.v1'] <- dates[1]
  
  # Visit 2:
  tmpv2 <- tmp[as.Date(tmp$date) == dates[2],]
  ceq[i,'v2.score'] <- sum(tmpv2$answer==1)
  ceq[i,'numR.v2'] <- dim(tmpv2)[1]
  ceq[i,'DATE.v2'] <- dates[2]
  
  if (length(dates)==3){
    # Visit 3:
    # Visit 1:
    tmpv3 <- tmp[as.Date(tmp$date) == dates[3],]
    ceq[i,'v3.score'] <- sum(tmpv3$answer==1)
    ceq[i,'numR.v3'] <- dim(tmpv3)[1]
    ceq[i,'DATE.v3'] <- dates[3]
  }
}
write.xlsx(ceq, paste(dir, 'summary/', 'CEQ', '.xlsx', sep=''), row.names = F)
rm(ceq)

### END ###
