##############################
### RBTII_bdataPrepare6m.R ###
##############################

# Author: Alexander V. Lebedev
# Date: 2018-05-25

# DESCRIPTION:
# The present R-script reads behavioural output from REBOOTII 6-month follow-up
# tests and generates individual score summaries for data analysis for
# Visuospatial Reasoning End-Point

# Load libraries:
library(xlsx)
library(R.matlab)
library(zoo)

# Set working directory:
dir <- '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2018-05-17FollowUp/'



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

################
# Summarizing: #
################



# Ia. Spatial Intelligence:
rav <- read.xlsx2(paste(dir, 'summary/transfer_raven.xlsx', sep=''),1)[,c('ID','v1.Score')]
beta <- read.xlsx2(paste(dir, 'summary/transfer_beta.xlsx', sep=''),1)[,c('ID','v1.Acc')]
wasi <- read.xlsx2(paste(dir, 'summary/transfer_wasi.xlsx', sep=''),1)[,c('ID','v1.Acc')]
anl <- read.xlsx2(paste(dir, 'summary/transfer_analogies.xlsx', sep=''),1)[,c('ID','v1.Acc')]
syll <- read.xlsx2(paste(dir, 'summary/transfer_syllogism.xlsx', sep=''),1)[,c('ID','v1.Acc')]
vinf <- read.xlsx2(paste(dir, 'summary/transfer_verbal_inference.xlsx', sep=''),1)[,c('ID','v1.Acc')]



cogdat6m <- merge(rav,beta, by='ID')
cogdat6m <- merge(cogdat6m,wasi, by='ID')
cogdat6m <- merge(cogdat6m,anl, by='ID')
cogdat6m <- merge(cogdat6m,syll, by='ID')
cogdat6m <- merge(cogdat6m,vinf, by='ID')

colnames(cogdat6m)<-c('ID', 'v3.rav', 'v3.beta','v3.wasi','v3.anl', 'v3.syll', 'v3.vinf')
cogdat6m[,c('v3.rav', 'v3.beta','v3.wasi','v3.anl', 'v3.syll', 'v3.vinf')]<- apply(cogdat6m[,c('v3.rav', 'v3.beta','v3.wasi','v3.anl', 'v3.syll', 'v3.vinf')],2, as.numeric)

# Modify incorrectly entered IDs ('https://docs.google.com/spreadsheets/d/1sTR8-dVXjTYIAGrnJnC4qFQZhz3lFoMUDMBs5Xbe6rY/edit#gid=0')
cogdat6m$ID <- as.vector(cogdat6m$ID)
cogdat6m$ID[cogdat6m$ID=='1101'] <- '4132'
cogdat6m$ID[cogdat6m$ID=='1102'] <- '4228'
cogdat6m$ID[cogdat6m$ID=='1103'] <- '4100'

cogdat6m$ID <- as.factor(cogdat6m$ID)

save(cogdat6m, file=paste(dir, 'summary/', 'cogdat6m.rda', sep=''))

# <<<<<<<<<<<<<<<
# <<< THE END <<<
# <<<<<<<<<<<<<<<

