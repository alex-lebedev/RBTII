# Prepare a list of files:
nbfolder <- '/Volumes/REBOOTII/RBTII/nback-cleaned-2017-11-25/'


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
colnames(nbScanner) <- c('ID','oa1', 'oa2', 'oa3', 'oa11', 'oa12', 'oa13', 'oa21', 'oa22', 'oa23', 'oa31', 'oa32', 'oa33',
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
colnames(nbScanner) <- c('ID','oa1', 'oa2', 'oa3', 'oa11', 'oa12', 'oa13', 'oa21', 'oa22', 'oa23', 'oa31', 'oa32', 'oa33',
                         'sens11','sens12','sens13','spec11','spec12','spec13','SimpAcc11',
                         'SimpAcc12','SimpAcc13','sens21','sens22','sens23','spec21','spec22',
                         'spec23','SimpAcc21','SimpAcc22','SimpAcc23','sens31','sens32','sens33',
                         'spec31','spec32','spec33','SimpAcc31','SimpAcc32','SimpAcc33','rtn1m',
                         'rtn1sd','rtn2m','rtn2sd','rtn2m','rtn2sd')


nbScannerV2 <- nbScanner


# MERGE:
nbScanner <- merge(nbScannerV1[,1:4], nbScannerV2[,1:4], by='ID')
#save()



# ANALYSIS:
# REASONING:
# Load cleaned source data:
load('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-11-29/summary/cogdat_cleaned.rda')
scogdat <- cogdat_cleaned
dat <- scogdat[,c('group','v1.rav', 'v2.rav', 'v1.beta', 'v2.beta', 'v1.wasi', 'v2.wasi', 'ID')]
x1 <- as.numeric(as.vector(dat[,2]))
x2 <- as.numeric(as.vector(dat[,3]))
y1 <- as.numeric(as.vector(dat[,4]))
y2 <- as.numeric(as.vector(dat[,5]))
z1 <- as.numeric(as.vector(dat[,6]))
z2 <- as.numeric(as.vector(dat[,7]))
x1s <- (x1-mean(x1, na.rm=T))/sd(x1, na.rm=T)
x2s <- (x2-mean(x1, na.rm=T))/sd(x1, na.rm=T)
y1s <- (y1-mean(y1, na.rm=T))/sd(y1, na.rm=T)
y2s <- (y2-mean(y1, na.rm=T))/sd(y1, na.rm=T)
z1s <- (z1-mean(z1, na.rm=T))/sd(z1, na.rm=T)
z2s <- (z2-mean(z1, na.rm=T))/sd(z1, na.rm=T)
tmp1 <- cbind(x1s,y1s,z1s)
tmp2 <- cbind(x2s,y2s,z2s)
comp1 <- apply(tmp1, 1,mean)
comp2 <- apply(tmp2, 1,mean)


dat$SI1<- comp1
dat$SI2<- comp2
dat <- dat[,c('ID','group', 'SI1', 'SI2')]


ddd <- merge(dat, nbScanner, by='ID')


cor(ddd[complete.cases(ddd),3:10])

t.test(c(ddd$oa3.y[ddd$group=='con']-ddd$oa3.x[ddd$group=='con']),
       c(ddd$oa3.y[ddd$group=='act']-ddd$oa3.x[ddd$group=='act']))

t.test(c(ddd$oa2.y[ddd$group=='con']-ddd$oa2.x[ddd$group=='con']),
       c(ddd$oa2.y[ddd$group=='act']-ddd$oa2.x[ddd$group=='act']))

t.test(c(ddd$oa1.y[ddd$group=='con']-ddd$oa1.x[ddd$group=='con']),
       c(ddd$oa1.y[ddd$group=='act']-ddd$oa1.x[ddd$group=='act']))


d <- data.frame(ID = rep(ddd$ID, 6), group=rep(ddd$group,6),
                visit=c(rep('V1', length(ddd$oa1.x)*3),rep('V2', length(ddd$oa1.x)*3)),
                load = rep(c(rep(1,length(ddd$oa1.x)),rep(2,length(ddd$oa1.x)),rep(3,length(ddd$oa1.x))),2),
  perf = c(ddd$oa1.x,ddd$oa2.x, ddd$oa3.x,ddd$oa1.y,ddd$oa2.y, ddd$oa3.y))
                

nback_scanner = d

save(nback_scanner, file='/Volumes/REBOOTII/RBTII/nback_scanner-2017-11-25.rda')

summary(glm(perf~group*load*visit, data=d))

modME <- lme(perf~group*load*visit,data=d, random=~1|ID)
summary(modME)

