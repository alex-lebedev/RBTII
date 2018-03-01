# Prepare a list of files:
nbfolder <- '/Volumes/REBOOTII/RBTII/inScannerTasks_2017-11-25/nback-cleaned-2017-11-25/'


nblist <- list.files(path=nbfolder, pattern="*.csv") 
nblist_v1 <- nblist[-grep('*_v2_*',nblist)]
nblist_v2 <- nblist[grep('*_v2_*',nblist)]



# Visit1:
nbScanner <- matrix(NA,length(nblist_v1), 45)

for (i in 1:dim(nbScanner)[1]){
  # for Vis2:
  # 1:71, 73:dim(nbScanner)[1]
  
  
  subjF <- read.csv(paste(nbfolder,nblist_v1[i], sep=''))
  
  ## n1, blocks 1-3:
  subj11 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n1'& subjF$block=='b1')
  sens11 <- table(subj11$cr, subj11$resp.keys)['1','1']/table(subj11$cr)['1']
  spec11 <- table(subj11$cr, subj11$resp.keys)['None','None']/table(subj11$cr)['None']
  SimpAcc11 <- table(subj11$cr==subj11$resp.keys)['TRUE']/sum(table(subj11$cr==subj11$resp.keys))
  
  subj12 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n1'& subjF$block=='b2')
  sens12 <- table(subj12$cr, subj12$resp.keys)['1','1']/table(subj12$cr)['1']
  spec12 <- table(subj12$cr, subj12$resp.keys)['None','None']/table(subj12$cr)['None']
  SimpAcc12 <- table(subj12$cr==subj12$resp.keys)['TRUE']/sum(table(subj12$cr==subj12$resp.keys))
  
  subj13 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n1'& subjF$block=='b3')
  sens13 <- table(subj13$cr, subj13$resp.keys)['1','1']/table(subj13$cr)['1']
  spec13 <- table(subj13$cr, subj13$resp.keys)['None','None']/table(subj13$cr)['None']
  SimpAcc13 <- table(subj13$cr==subj13$resp.keys)['TRUE']/sum(table(subj13$cr==subj13$resp.keys))
  
  ## n2:
  subj21 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n2'& subjF$block=='b1')
  sens21 <- table(subj21$cr, subj21$resp.keys)['1','1']/table(subj21$cr)['1']
  spec21 <- table(subj21$cr, subj21$resp.keys)['None','None']/table(subj21$cr)['None']
  SimpAcc21 <- table(subj21$cr==subj21$resp.keys)['TRUE']/sum(table(subj21$cr==subj21$resp.keys))
  
  subj22 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n2'& subjF$block=='b2')
  sens22 <- table(subj22$cr, subj22$resp.keys)['1','1']/table(subj22$cr)['1']
  spec22 <- table(subj22$cr, subj22$resp.keys)['None','None']/table(subj22$cr)['None']
  SimpAcc22 <- table(subj22$cr==subj22$resp.keys)['TRUE']/sum(table(subj22$cr==subj22$resp.keys))
  
  subj23 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n2'& subjF$block=='b3')
  sens23 <- table(subj23$cr, subj23$resp.keys)['1','1']/table(subj23$cr)['1']
  spec23 <- table(subj23$cr, subj23$resp.keys)['None','None']/table(subj23$cr)['None']
  SimpAcc23 <- table(subj23$cr==subj23$resp.keys)['TRUE']/sum(table(subj23$cr==subj23$resp.keys))
  
  ## n3:
  subj31 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n3'& subjF$block=='b1')
  sens31 <- table(subj31$cr, subj31$resp.keys)['1','1']/table(subj31$cr)['1']
  spec31 <- table(subj31$cr, subj31$resp.keys)['None','None']/table(subj31$cr)['None']
  SimpAcc31 <- table(subj31$cr==subj31$resp.keys)['TRUE']/sum(table(subj31$cr==subj31$resp.keys))
  
  subj32 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n3'& subjF$block=='b2')
  sens32 <- table(subj32$cr, subj32$resp.keys)['1','1']/table(subj32$cr)['1']
  spec32 <- table(subj32$cr, subj32$resp.keys)['None','None']/table(subj32$cr)['None']
  SimpAcc32 <- table(subj32$cr==subj32$resp.keys)['TRUE']/sum(table(subj32$cr==subj32$resp.keys))
  
  subj33 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n3'& subjF$block=='b3')
  sens33 <- table(subj33$cr, subj33$resp.keys)['1','1']/table(subj33$cr)['1']
  spec33 <- table(subj33$cr, subj33$resp.keys)['None','None']/table(subj33$cr)['None']
  SimpAcc33 <- table(subj33$cr==subj33$resp.keys)['TRUE']/sum(table(subj33$cr==subj33$resp.keys))
  
  # RTs per N (3 blocks combined):
  subjn1 <- subset(subjF[,c('cr', 'resp.keys', 'resp.rt')], subjF$n=='n1')
  rtn1m <- mean(subset(subjn1$resp.rt, subjn1$cr==subjn1$resp.keys & subjn1$cr==1))
  rtn1sd <- sd(subset(subjn1$resp.rt, subjn1$cr==subjn1$resp.keys & subjn1$cr==1))
  subjn2 <- subset(subjF[,c('cr', 'resp.keys', 'resp.rt')], subjF$n=='n2')
  rtn2m <- mean(subset(subjn2$resp.rt, subjn2$cr==subjn2$resp.keys & subjn2$cr==1))
  rtn2sd <- sd(subset(subjn2$resp.rt, subjn2$cr==subjn2$resp.keys & subjn2$cr==1))
  subjn3 <- subset(subjF[,c('cr', 'resp.keys', 'resp.rt')], subjF$n=='n3')
  rtn3m <- mean(subset(subjn3$resp.rt, subjn3$cr==subjn3$resp.keys & subjn3$cr==1))
  rtn3sd <- sd(subset(subjn3$resp.rt, subjn3$cr==subjn3$resp.keys & subjn3$cr==1))
  
  # Overall accuracy:
  oa11 <- mean(c(sens11,spec11))
  oa12 <- mean(c(sens12,spec12))
  oa13 <- mean(c(sens13,spec13))
  
  oa21 <- mean(c(sens21,spec21))
  oa22 <- mean(c(sens22,spec22))
  oa23 <- mean(c(sens23,spec23))
  
  oa31 <- mean(c(sens31,spec31))
  oa32 <- mean(c(sens32,spec32))
  oa33 <- mean(c(sens33,spec33))
  
  oa1 <- mean(c(oa11,oa12,oa12))
  oa2 <- mean(c(oa21,oa22,oa23))
  oa3 <- mean(c(oa31,oa32,oa33))
  
  nbScanner[i,] <-  c(oa1, oa2, oa3, oa11, oa12, oa13, oa21, oa22, oa23, oa31, oa32, oa33,
                      sens11, sens12, sens13, spec11, spec12, spec13, SimpAcc11, SimpAcc12, SimpAcc13,
                      sens21, sens22, sens23, spec21, spec22, spec23, SimpAcc21, SimpAcc22, SimpAcc23,
                      sens31, sens32, sens33, spec31, spec32, spec33, SimpAcc31, SimpAcc32, SimpAcc33,
                      rtn1m, rtn1sd, rtn2m, rtn2sd, rtn2m,rtn2sd)
}



nbScanner <- cbind(as.numeric(strtrim(nblist_v1,4)),nbScanner)
colnames(nbScanner) <- c('StudyID','oa1', 'oa2', 'oa3', 'oa11', 'oa12', 'oa13', 'oa21', 'oa22', 'oa23', 'oa31', 'oa32', 'oa33',
                         'sens11','sens12','sens13','spec11','spec12','spec13','SimpAcc11',
                         'SimpAcc12','SimpAcc13','sens21','sens22','sens23','spec21','spec22',
                         'spec23','SimpAcc21','SimpAcc22','SimpAcc23','sens31','sens32','sens33',
                         'spec31','spec32','spec33','SimpAcc31','SimpAcc32','SimpAcc33','rtn1m',
                         'rtn1sd','rtn2m','rtn2sd','rtn2m','rtn2sd')


nbScannerV1 <- nbScanner




### N-BACK: VIS2
nbScanner <- matrix(NA,length(nblist_v2), 45)

for (i in 1:71) {
  # for Vis2: 1:71, 73:dim(nbScanner)[1]
  
  
  subjF <- read.csv(paste(nbfolder,nblist_v2[i], sep=''))
  
  ## n1, blocks 1-3:
  subj11 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n1'& subjF$block=='b1')
  sens11 <- table(subj11$cr, subj11$resp.keys)['1','1']/table(subj11$cr)['1']
  spec11 <- table(subj11$cr, subj11$resp.keys)['None','None']/table(subj11$cr)['None']
  SimpAcc11 <- table(subj11$cr==subj11$resp.keys)['TRUE']/sum(table(subj11$cr==subj11$resp.keys))
  
  subj12 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n1'& subjF$block=='b2')
  sens12 <- table(subj12$cr, subj12$resp.keys)['1','1']/table(subj12$cr)['1']
  spec12 <- table(subj12$cr, subj12$resp.keys)['None','None']/table(subj12$cr)['None']
  SimpAcc12 <- table(subj12$cr==subj12$resp.keys)['TRUE']/sum(table(subj12$cr==subj12$resp.keys))
  
  subj13 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n1'& subjF$block=='b3')
  sens13 <- table(subj13$cr, subj13$resp.keys)['1','1']/table(subj13$cr)['1']
  spec13 <- table(subj13$cr, subj13$resp.keys)['None','None']/table(subj13$cr)['None']
  SimpAcc13 <- table(subj13$cr==subj13$resp.keys)['TRUE']/sum(table(subj13$cr==subj13$resp.keys))
  
  ## n2:
  subj21 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n2'& subjF$block=='b1')
  sens21 <- table(subj21$cr, subj21$resp.keys)['1','1']/table(subj21$cr)['1']
  spec21 <- table(subj21$cr, subj21$resp.keys)['None','None']/table(subj21$cr)['None']
  SimpAcc21 <- table(subj21$cr==subj21$resp.keys)['TRUE']/sum(table(subj21$cr==subj21$resp.keys))
  
  subj22 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n2'& subjF$block=='b2')
  sens22 <- table(subj22$cr, subj22$resp.keys)['1','1']/table(subj22$cr)['1']
  spec22 <- table(subj22$cr, subj22$resp.keys)['None','None']/table(subj22$cr)['None']
  SimpAcc22 <- table(subj22$cr==subj22$resp.keys)['TRUE']/sum(table(subj22$cr==subj22$resp.keys))
  
  subj23 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n2'& subjF$block=='b3')
  sens23 <- table(subj23$cr, subj23$resp.keys)['1','1']/table(subj23$cr)['1']
  spec23 <- table(subj23$cr, subj23$resp.keys)['None','None']/table(subj23$cr)['None']
  SimpAcc23 <- table(subj23$cr==subj23$resp.keys)['TRUE']/sum(table(subj23$cr==subj23$resp.keys))
  
  ## n3:
  subj31 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n3'& subjF$block=='b1')
  sens31 <- table(subj31$cr, subj31$resp.keys)['1','1']/table(subj31$cr)['1']
  spec31 <- table(subj31$cr, subj31$resp.keys)['None','None']/table(subj31$cr)['None']
  SimpAcc31 <- table(subj31$cr==subj31$resp.keys)['TRUE']/sum(table(subj31$cr==subj31$resp.keys))
  
  subj32 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n3'& subjF$block=='b2')
  sens32 <- table(subj32$cr, subj32$resp.keys)['1','1']/table(subj32$cr)['1']
  spec32 <- table(subj32$cr, subj32$resp.keys)['None','None']/table(subj32$cr)['None']
  SimpAcc32 <- table(subj32$cr==subj32$resp.keys)['TRUE']/sum(table(subj32$cr==subj32$resp.keys))
  
  subj33 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n3'& subjF$block=='b3')
  sens33 <- table(subj33$cr, subj33$resp.keys)['1','1']/table(subj33$cr)['1']
  spec33 <- table(subj33$cr, subj33$resp.keys)['None','None']/table(subj33$cr)['None']
  SimpAcc33 <- table(subj33$cr==subj33$resp.keys)['TRUE']/sum(table(subj33$cr==subj33$resp.keys))
  
  # RTs per N (3 blocks combined):
  subjn1 <- subset(subjF[,c('cr', 'resp.keys', 'resp.rt')], subjF$n=='n1')
  rtn1m <- mean(subset(subjn1$resp.rt, subjn1$cr==subjn1$resp.keys & subjn1$cr==1))
  rtn1sd <- sd(subset(subjn1$resp.rt, subjn1$cr==subjn1$resp.keys & subjn1$cr==1))
  subjn2 <- subset(subjF[,c('cr', 'resp.keys', 'resp.rt')], subjF$n=='n2')
  rtn2m <- mean(subset(subjn2$resp.rt, subjn2$cr==subjn2$resp.keys & subjn2$cr==1))
  rtn2sd <- sd(subset(subjn2$resp.rt, subjn2$cr==subjn2$resp.keys & subjn2$cr==1))
  subjn3 <- subset(subjF[,c('cr', 'resp.keys', 'resp.rt')], subjF$n=='n3')
  rtn3m <- mean(subset(subjn3$resp.rt, subjn3$cr==subjn3$resp.keys & subjn3$cr==1))
  rtn3sd <- sd(subset(subjn3$resp.rt, subjn3$cr==subjn3$resp.keys & subjn3$cr==1))
  
  # Overall accuracy:
  oa11 <- mean(c(sens11,spec11))
  oa12 <- mean(c(sens12,spec12))
  oa13 <- mean(c(sens13,spec13))
  
  oa21 <- mean(c(sens21,spec21))
  oa22 <- mean(c(sens22,spec22))
  oa23 <- mean(c(sens23,spec23))
  
  oa31 <- mean(c(sens31,spec31))
  oa32 <- mean(c(sens32,spec32))
  oa33 <- mean(c(sens33,spec33))
  
  oa1 <- mean(c(oa11,oa12,oa12))
  oa2 <- mean(c(oa21,oa22,oa23))
  oa3 <- mean(c(oa31,oa32,oa33))
  
  nbScanner[i,] <-  c(oa1, oa2, oa3, oa11, oa12, oa13, oa21, oa22, oa23, oa31, oa32, oa33,
                      sens11, sens12, sens13, spec11, spec12, spec13, SimpAcc11, SimpAcc12, SimpAcc13,
                      sens21, sens22, sens23, spec21, spec22, spec23, SimpAcc21, SimpAcc22, SimpAcc23,
                      sens31, sens32, sens33, spec31, spec32, spec33, SimpAcc31, SimpAcc32, SimpAcc33,
                      rtn1m, rtn1sd, rtn2m, rtn2sd, rtn2m,rtn2sd)
}


colnames(nbScanner) <- c('oa1', 'oa2', 'oa3', 'oa11', 'oa12', 'oa13', 'oa21', 'oa22', 'oa23', 'oa31', 'oa32', 'oa33',
                         'sens11','sens12','sens13','spec11','spec12','spec13','SimpAcc11',
                         'SimpAcc12','SimpAcc13','sens21','sens22','sens23','spec21','spec22',
                         'spec23','SimpAcc21','SimpAcc22','SimpAcc23','sens31','sens32','sens33',
                         'spec31','spec32','spec33','SimpAcc31','SimpAcc32','SimpAcc33','rtn1m',
                         'rtn1sd','rtn2m','rtn2sd','rtn2m','rtn2sd')


for (i in 73:dim(nbScanner)[1]) {
  # for Vis2: 1:71, 73:dim(nbScanner)[1]
  
  
  subjF <- read.csv(paste(nbfolder,nblist_v2[i], sep=''))
  
  ## n1, blocks 1-3:
  subj11 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n1'& subjF$block=='b1')
  sens11 <- table(subj11$cr, subj11$resp.keys)['1','1']/table(subj11$cr)['1']
  spec11 <- table(subj11$cr, subj11$resp.keys)['None','None']/table(subj11$cr)['None']
  SimpAcc11 <- table(subj11$cr==subj11$resp.keys)['TRUE']/sum(table(subj11$cr==subj11$resp.keys))
  
  subj12 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n1'& subjF$block=='b2')
  sens12 <- table(subj12$cr, subj12$resp.keys)['1','1']/table(subj12$cr)['1']
  spec12 <- table(subj12$cr, subj12$resp.keys)['None','None']/table(subj12$cr)['None']
  SimpAcc12 <- table(subj12$cr==subj12$resp.keys)['TRUE']/sum(table(subj12$cr==subj12$resp.keys))
  
  subj13 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n1'& subjF$block=='b3')
  sens13 <- table(subj13$cr, subj13$resp.keys)['1','1']/table(subj13$cr)['1']
  spec13 <- table(subj13$cr, subj13$resp.keys)['None','None']/table(subj13$cr)['None']
  SimpAcc13 <- table(subj13$cr==subj13$resp.keys)['TRUE']/sum(table(subj13$cr==subj13$resp.keys))
  
  ## n2:
  subj21 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n2'& subjF$block=='b1')
  sens21 <- table(subj21$cr, subj21$resp.keys)['1','1']/table(subj21$cr)['1']
  spec21 <- table(subj21$cr, subj21$resp.keys)['None','None']/table(subj21$cr)['None']
  SimpAcc21 <- table(subj21$cr==subj21$resp.keys)['TRUE']/sum(table(subj21$cr==subj21$resp.keys))
  
  subj22 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n2'& subjF$block=='b2')
  sens22 <- table(subj22$cr, subj22$resp.keys)['1','1']/table(subj22$cr)['1']
  spec22 <- table(subj22$cr, subj22$resp.keys)['None','None']/table(subj22$cr)['None']
  SimpAcc22 <- table(subj22$cr==subj22$resp.keys)['TRUE']/sum(table(subj22$cr==subj22$resp.keys))
  
  subj23 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n2'& subjF$block=='b3')
  sens23 <- table(subj23$cr, subj23$resp.keys)['1','1']/table(subj23$cr)['1']
  spec23 <- table(subj23$cr, subj23$resp.keys)['None','None']/table(subj23$cr)['None']
  SimpAcc23 <- table(subj23$cr==subj23$resp.keys)['TRUE']/sum(table(subj23$cr==subj23$resp.keys))
  
  ## n3:
  subj31 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n3'& subjF$block=='b1')
  sens31 <- table(subj31$cr, subj31$resp.keys)['1','1']/table(subj31$cr)['1']
  spec31 <- table(subj31$cr, subj31$resp.keys)['None','None']/table(subj31$cr)['None']
  SimpAcc31 <- table(subj31$cr==subj31$resp.keys)['TRUE']/sum(table(subj31$cr==subj31$resp.keys))
  
  subj32 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n3'& subjF$block=='b2')
  sens32 <- table(subj32$cr, subj32$resp.keys)['1','1']/table(subj32$cr)['1']
  spec32 <- table(subj32$cr, subj32$resp.keys)['None','None']/table(subj32$cr)['None']
  SimpAcc32 <- table(subj32$cr==subj32$resp.keys)['TRUE']/sum(table(subj32$cr==subj32$resp.keys))
  
  subj33 <- subset(subjF[,c('cr', 'resp.keys')], subjF$n=='n3'& subjF$block=='b3')
  sens33 <- table(subj33$cr, subj33$resp.keys)['1','1']/table(subj33$cr)['1']
  spec33 <- table(subj33$cr, subj33$resp.keys)['None','None']/table(subj33$cr)['None']
  SimpAcc33 <- table(subj33$cr==subj33$resp.keys)['TRUE']/sum(table(subj33$cr==subj33$resp.keys))
  
  # RTs per N (3 blocks combined):
  subjn1 <- subset(subjF[,c('cr', 'resp.keys', 'resp.rt')], subjF$n=='n1')
  rtn1m <- mean(subset(subjn1$resp.rt, subjn1$cr==subjn1$resp.keys & subjn1$cr==1))
  rtn1sd <- sd(subset(subjn1$resp.rt, subjn1$cr==subjn1$resp.keys & subjn1$cr==1))
  subjn2 <- subset(subjF[,c('cr', 'resp.keys', 'resp.rt')], subjF$n=='n2')
  rtn2m <- mean(subset(subjn2$resp.rt, subjn2$cr==subjn2$resp.keys & subjn2$cr==1))
  rtn2sd <- sd(subset(subjn2$resp.rt, subjn2$cr==subjn2$resp.keys & subjn2$cr==1))
  subjn3 <- subset(subjF[,c('cr', 'resp.keys', 'resp.rt')], subjF$n=='n3')
  rtn3m <- mean(subset(subjn3$resp.rt, subjn3$cr==subjn3$resp.keys & subjn3$cr==1))
  rtn3sd <- sd(subset(subjn3$resp.rt, subjn3$cr==subjn3$resp.keys & subjn3$cr==1))
  
  # Overall accuracy:
  oa11 <- mean(c(sens11,spec11))
  oa12 <- mean(c(sens12,spec12))
  oa13 <- mean(c(sens13,spec13))
  
  oa21 <- mean(c(sens21,spec21))
  oa22 <- mean(c(sens22,spec22))
  oa23 <- mean(c(sens23,spec23))
  
  oa31 <- mean(c(sens31,spec31))
  oa32 <- mean(c(sens32,spec32))
  oa33 <- mean(c(sens33,spec33))
  
  oa1 <- mean(c(oa11,oa12,oa12))
  oa2 <- mean(c(oa21,oa22,oa23))
  oa3 <- mean(c(oa31,oa32,oa33))
  
  nbScanner[i,] <-  c(oa1, oa2, oa3, oa11, oa12, oa13, oa21, oa22, oa23, oa31, oa32, oa33,
                      sens11, sens12, sens13, spec11, spec12, spec13, SimpAcc11, SimpAcc12, SimpAcc13,
                      sens21, sens22, sens23, spec21, spec22, spec23, SimpAcc21, SimpAcc22, SimpAcc23,
                      sens31, sens32, sens33, spec31, spec32, spec33, SimpAcc31, SimpAcc32, SimpAcc33,
                      rtn1m, rtn1sd, rtn2m, rtn2sd, rtn2m,rtn2sd)
}



nbScanner <- cbind(as.numeric(strtrim(nblist_v2,4)),nbScanner)
colnames(nbScanner) <- c('StudyID','oa1', 'oa2', 'oa3', 'oa11', 'oa12', 'oa13', 'oa21', 'oa22', 'oa23', 'oa31', 'oa32', 'oa33',
                         'sens11','sens12','sens13','spec11','spec12','spec13','SimpAcc11',
                         'SimpAcc12','SimpAcc13','sens21','sens22','sens23','spec21','spec22',
                         'spec23','SimpAcc21','SimpAcc22','SimpAcc23','sens31','sens32','sens33',
                         'spec31','spec32','spec33','SimpAcc31','SimpAcc32','SimpAcc33','rtn1m',
                         'rtn1sd','rtn2m','rtn2sd','rtn2m','rtn2sd')


nbScannerV2 <- nbScanner


# MERGE:
nbScanner <- merge(nbScannerV1[,1:4], nbScannerV2[,1:4], by='StudyID')

colnames(nbScanner)[1] <- 'ID'
dd <- as.data.frame(scogdat[c('ID', 'group')]);  dat <- merge(nbScanner, dd, by='ID')
t.test(c(dat$oa3.y[dat$group=='con']-dat$oa3.x[dat$group=='con']),c(dat$oa3.y[dat$group=='act']-dat$oa3.x[dat$group=='act']))
t.test(c(dat$oa2.y[dat$group=='con']-dat$oa2.x[dat$group=='con']),c(dat$oa2.y[dat$group=='act']-dat$oa2.x[dat$group=='act']))
t.test(c(dat$oa1.y[dat$group=='con']-dat$oa1.x[dat$group=='con']),c(dat$oa1.y[dat$group=='act']-dat$oa1.x[dat$group=='act']))

