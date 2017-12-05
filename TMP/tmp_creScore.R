#############################
# RBT-II creativity ratings #
#############################

# Laod libraries:
library(xlsx)

#creDat <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/RBTII_creativity_Filip_May5.xlsx',1)
#creDat <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/RBTII_creativity_William_May.xlsx',1)

creDat <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/RBTII_creativity_Filip_May5.xlsx',1)



CreORIG <- cbind(creDat[,1:2],creDat[,grepl("ORIG.subj", names(creDat))])
CreFLU <- cbind(creDat[,1:2],creDat[,grepl("FLU", names(creDat))])
CreFLEX <- cbind(creDat[,1:2],creDat[,grepl("FLEX", names(creDat))])

CreORIG[is.na(CreORIG)] <- 0
CreFLEX[is.na(CreFLEX)] <- 0
CreFLU[is.na(CreFLU)] <- 0


ids <- CreORIG[,1][!duplicated(CreORIG[,1])]

creO <- as.data.frame(matrix(NA, length(ids),dim(CreORIG)[2]))
creFx <- creO
creFl <- creO

for (i in 1:length(ids)){
  tmpO <- subset(CreORIG, is.element(CreORIG$StudyID, ids[i]))
  tmpFx <- subset(CreFLEX, is.element(CreFLEX$StudyID, ids[i]))
  tmpFl <- subset(CreFLU, is.element(CreFLU$StudyID, ids[i]))
  
  creO[i,1] <- tmpO$StudyID[1]
  creO[i,2] <- as.vector(tmpO$ListVer[1])
  creFx[i,1] <- tmpFx$StudyID[1]
  creFx[i,2] <- as.vector(tmpFx$ListVer[1])
  creFl[i,1] <- tmpFl$StudyID[1]
  creFl[i,2] <- as.vector(tmpFl$ListVer[1])
  
  
  for (n in c(3:dim(tmpO)[2])) {
    creO[i,n] <- mean(tmpO[,n])
    creFx[i,n] <- mean(tmpFx[,n])
    creFl[i,n] <- mean(tmpFl[,n])
  }
}


colnames(creO) <- c('StudyID', 'ListVer', strsplit(names(CreORIG[,c(3:dim(CreORIG)[2])]),split='.ORIG*.*'))
colnames(creFx) <- colnames(creO)
colnames(creFl) <- colnames(creO)


cO1 <- creO[is.element(creO$ListVer,c('L1', 'L3', 'L6', 'L7')), c(c('ttct.lines', 'au.paperclip', 'au.brick', 'au.newspaper',
                                                                    'inst.locomotion',	'inst.round', 'inst.loud_noise'), 
                            c('ttct.circles', 'au.däck', 'au.flaska', 'au.kniv', 'inst.legs', 'inst.strong', 'inst.storage'))]
cO2 <- creO[!is.element(creO$ListVer,c('L1', 'L3', 'L6', 'L7')), c(c('ttct.circles', 'au.däck', 'au.flaska', 'au.kniv', 
                                                                     'inst.legs', 'inst.strong', 'inst.storage'),
                            c('ttct.lines', 'au.paperclip', 'au.brick', 'au.newspaper', 'inst.locomotion',	'inst.round', 'inst.loud_noise'))]        
cO <- rbind(cO1,cO2)

colnames(cO) <- c('ttct.v1', 'au1.v1','au2.v1','au3.v1','inst1.v1','inst2.v1','inst3.v1',
                  
                  'ttct.v2', 'au1.v2','au2.v2','au3.v2','inst1.v2','inst2.v2','inst3.v2')


t.test(apply(cO[,1:7],1,mean),apply(cO[,8:14],1,mean), paired=T)

cO$CompTOT.v1 <- apply(cO[,1:7],1,mean)
cO$CompTOT.v2 <- apply(cO[,8:14],1,mean)
cO <- cbind(as.factor(creO$StudyID),cO)
colnames(cO)[1] <- 'stuID'

# cO_F <- cO
# cFx_F <- cO

#corrplot(cor(cbind(cO_F$CompTOT.v1,cO_F$CompTOT.v2,cO_W$CompTOT.v1,cO_W$CompTOT.v2)), 'shade', col=c('lightblue','blue', 'green', 'black','grey','red', 'yellow'), addCoef.col='black')



write.xlsx(cO_F, '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/cO_F.xlsx',row.names=F)


for (n in 2:dim(cO)[2]){
  cO[,n] <- apply(cbind(cO_F[,n],cO_W[,n]),1,mean)
  cFx[,n] <- apply(cbind(cFx_F[,n],cFx_W[,n]),1,mean)
  cFl[,n] <-apply(cbind(cFl_F[,n],cFl_W[,n]),1,mean)
}




###

rand1 <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/randomization/RandomizedWave1.xlsx',1)
rand2 <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/randomization/RandomizedWave2.xlsx',1)
rand <- rbind(rand1,rand2)
#creD <- merge(rand, cO, by='stuID')
#write.xlsx(creD, '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/creD.xlsx',row.names=F)
#t.test(c(creD$CreTOT.v2-creD$CreTOT.v1)[creD$group=='con'],c(creD$CreTOT.v2-creD$CreTOT.v1)[creD$group=='act'])

#qq <- merge(creD, q, by='stuID')
#cor(qq$CreTOT.v1, qq$v1.Score_dist)
#dd <- cbind(c(qq$CreTOT.v2-qq$CreTOT.v1), c(qq$tnb3.OA.v2-qq$tnb3.OA.v1))
# WM z-scoring:
#dat <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/Filip_source.xlsx',8)
#dem <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/Filip_source.xlsx',1)

dat <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/WM_w12.xlsx',1)

datz <- as.data.frame(cbind(dat$ID,
                (dat$tnb2.OA.v1-mean(dat$tnb2.OA.v1))/sd(dat$tnb2.OA.v1),
                (dat$tnb2.OA.v2-mean(dat$tnb2.OA.v1))/sd(dat$tnb2.OA.v1),
                
                (dat$tnb3.OA.v1-mean(dat$tnb3.OA.v1))/sd(dat$tnb3.OA.v1),
                (dat$tnb3.OA.v2-mean(dat$tnb3.OA.v1))/sd(dat$tnb3.OA.v1),
                
                (dat$v1.CorrCount.lvl2-mean(dat$v1.CorrCount.lvl2))/sd(dat$v1.CorrCount.lvl2),
                (dat$v2.CorrCount.lvl2-mean(dat$v1.CorrCount.lvl2))/sd(dat$v1.CorrCount.lvl2),
                
                (dat$v1.CorrCount.lvl4-mean(dat$v1.CorrCount.lvl4))/sd(dat$v1.CorrCount.lvl4),
                (dat$v2.CorrCount.lvl4-mean(dat$v1.CorrCount.lvl4))/sd(dat$v1.CorrCount.lvl4),
                
                ((dat$v1.RT.sw1-mean(dat$v1.RT.sw1))*(-1))/sd(dat$v1.RT.sw1), 
                ((dat$v2.RT.sw1-mean(dat$v1.RT.sw1))*(-1))/sd(dat$v1.RT.sw1)))

datz$V1 <- as.factor(as.vector(dat$ID))

colnames(datz) <- colnames(dat)
datz$wmz1 <- apply(datz[,c("tnb2.OA.v1", "tnb3.OA.v1","v1.CorrCount.lvl2","v1.CorrCount.lvl4","v1.RT.sw1")],1,mean)
datz$wmz2 <- apply(datz[,c("tnb2.OA.v2", "tnb3.OA.v2","v2.CorrCount.lvl2","v2.CorrCount.lvl4","v2.RT.sw1")],1,mean)

write.xlsx(datz, '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/Filip_datz.xlsx', row.names=F)
    
                

cre <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/cO_avg.xlsx',1)
wm <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/Filip_datz.xlsx',1)
#wm <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/Filip_WM.xlsx',1)
reas <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/reas_Filip.xlsx',1)
crecomp <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/CRECOMP.xlsx',1)
pers <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/personalityCORR.xlsx',1)
dem <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/Filip_source.xlsx',1)
#imp <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-05-05/summary/baratt.xlsx',1)

names(cre)[1] <- 'ID'
names(dem)[1] <- 'ID'

ddat <- merge(wm, cre, by='ID')
ddat <- merge(ddat, dem, by='ID')

ddat <- merge(ddat, pers, by='ID')
ddat <- merge(ddat, reas, by='ID')
#ddat <- merge(ddat, imp, by='ID')
ddat <- cbind(ddat, crecomp)


# Change-change:
cor(ddat$CompTOT.v2-ddat$CompTOT.v1, ddat$wmz2-ddat$wmz1)

dd1 <- as.data.frame(ddat[,c('ID', 'group', 'wmz1', 'cr1', 'CEQ.v1.score', 'PDI.v1.Score_dist', 'v1.O', 'v1.N', 'v1.C', 'CompTOT.v1', 'reasComp.v1')])
dd2 <- as.data.frame(ddat[,c('ID', 'group', 'wmz2', 'cr2', 'CEQ.v2.score', 'PDI.v2.Score_dist', 'v2.O', 'v2.N', 'v2.C', 'CompTOT.v2', 'reasComp.v2')])

colnames(dd1) <- c('ID', 'group', 'wmz', 'cr', 'CEQ', 'PDI', 'O', 'N', 'C', 'Cre', 'reasComp')
colnames(dd2) <- c('ID', 'group', 'wmz', 'cr', 'CEQ', 'PDI', 'O', 'N', 'C', 'Cre', 'reasComp')
dd <- rbind(dd1, dd2)
dd$time <- as.factor(c(rep(1,dim(ddat)[1]),rep(2, dim(ddat)[1])))

pdiSQ <- sqrt(dd1$PDI)
ceqSQ <- sqrt(dd1$CEQ)
psy <- (((pdiSQ-mean(pdiSQ))/sd(pdiSQ))+((ceqSQ-mean(ceqSQ))/sd(ceqSQ)))/2

dd$PDI_bl <- rep(pdiSQ,2)
dd$CEQ_bl <- rep(ceqSQ,2)
dd$O_bl <- rep(dd1$O,2)
dd$N_bl <- rep(dd1$N,2)
dd$C_bl <- rep(dd1$C,2)
dd$sch <- rep(psy,2)




summary(glm(cr~group*time, data=dd))
summary.aov(lm(cr~group*time, data=dd))

summary.aov(lm(cr~group*time, data=dd))
####


colnames(tnb2)[1] <- 'ID'
colnames(tnb3)[1] <- 'ID'
dat <- merge(tnb2, tnb3, by='ID')
dat <- merge(dat, tnup, by='ID')
dat <- merge(dat, ttsw, by='ID')



datz <- as.data.frame(cbind(dat$ID,
                            (dat$tnb2.OA.v1-mean(dat$tnb2.OA.v1))/sd(dat$tnb2.OA.v1),
                            (dat$tnb2.OA.v2-mean(dat$tnb2.OA.v1))/sd(dat$tnb2.OA.v1),
                            
                            (dat$tnb3.OA.v1-mean(dat$tnb3.OA.v1))/sd(dat$tnb3.OA.v1),
                            (dat$tnb3.OA.v2-mean(dat$tnb3.OA.v1))/sd(dat$tnb3.OA.v1),
                            
                            (dat$v1.CorrCount.lvl2-mean(dat$v1.CorrCount.lvl2))/sd(dat$v1.CorrCount.lvl2),
                            (dat$v2.CorrCount.lvl2-mean(dat$v1.CorrCount.lvl2))/sd(dat$v1.CorrCount.lvl2),
                            
                            (dat$v1.CorrCount.lvl4-mean(dat$v1.CorrCount.lvl4))/sd(dat$v1.CorrCount.lvl4),
                            (dat$v2.CorrCount.lvl4-mean(dat$v1.CorrCount.lvl4))/sd(dat$v1.CorrCount.lvl4),
                            
                            ((dat$v1.swC-mean(dat$v1.RT.sw1))*(-1))/sd(dat$v1.swC), 
                            ((dat$v2.swC-mean(dat$v1.RT.sw1))*(-1))/sd(dat$v1.swC)))

datz$V1 <- as.factor(as.vector(dat$ID))

colnames(datz) <- c('ID', 'tnb2.OA.v1', 'tnb2.OA.v2', 'tnb3.OA.v1', 'tnb3.OA.v2', 
                    'v1.CorrCount.lvl2', 'v2.CorrCount.lvl2', 'v1.CorrCount.lvl4', 'v2.CorrCount.lvl4',
                    'v1.swC', 'v2.swC')
datz <- datz[1:25,]
datz <- as.data.frame(apply(datz,2,as.numeric, as.vector))

datz$wmz1 <- apply(datz[,c("tnb2.OA.v1", "tnb3.OA.v1","v1.CorrCount.lvl2","v1.CorrCount.lvl4","v1.swC")],1,mean)
datz$wmz2 <- apply(datz[,c("tnb2.OA.v2", "tnb3.OA.v2","v2.CorrCount.lvl2","v2.CorrCount.lvl4","v2.swC")],1,mean)

dd <- merge(rand, datz, by='ID')
mean(dd$wmz2[dd$group=='act']-datz$wmz1[dd$group=='act'])



