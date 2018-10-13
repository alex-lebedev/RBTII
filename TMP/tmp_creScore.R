#############################
# RBT-II creativity ratings #
#############################

# Laod libraries:
library(xlsx)

#creDat <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/RBTII_Creativity_ALL_Filip.xlsx',1)
#creDat <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/RBTII_Creativity_ALL_William.xlsx',1)

creDat <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/RBTII_Creativity_ALL_Filip.xlsx',1)
CreORIG <- cbind(creDat[,1:2],creDat[,grepl("ORIG.subj", names(creDat))])
CreFLU <- cbind(creDat[,1:2],creDat[,grepl("FLU", names(creDat))])
CreFLEX <- cbind(creDat[,1:2],creDat[,grepl("FLEX", names(creDat))])
#CreORIG[is.na(CreORIG)] <- 0
#CreFLEX[is.na(CreFLEX)] <- 0
#CreFLU[is.na(CreFLU)] <- 0
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
    creO[i,n] <- mean(tmpO[,n],na.rm=T)
    creFx[i,n] <- mean(tmpFx[,n],na.rm=T)
    creFl[i,n] <- mean(tmpFl[,n],na.rm=T)
  }
}


colnames(creO) <- c('StudyID', 'ListVer', strsplit(names(CreORIG[,c(3:dim(CreORIG)[2])]),split='.ORIG*.*'))
colnames(creFx) <- colnames(creO)
colnames(creFl) <- colnames(creO)


# change creO to -> creFx, creFl and rerun:
cO1 <- creO[is.element(creO$ListVer,c('L1', 'L3', 'L6', 'L7')), c(c('ttct.lines', 'au.paperclip', 'au.brick', 'au.newspaper',
                                                                    'inst.locomotion',	'inst.round', 'inst.loud_noise'), 
                            c('ttct.circles', 'au.däck', 'au.flaska', 'au.kniv', 'inst.legs', 'inst.strong', 'inst.storage'))]
cO2 <- creO[!is.element(creO$ListVer,c('L1', 'L3', 'L6', 'L7')), c(c('ttct.circles', 'au.däck', 'au.flaska', 'au.kniv', 
                                                                     'inst.legs', 'inst.strong', 'inst.storage'),
                            c('ttct.lines', 'au.paperclip', 'au.brick', 'au.newspaper', 'inst.locomotion',	'inst.round', 'inst.loud_noise'))]        
cO <- rbind(cO1,cO2)

colnames(cO) <- c('ttct.v1', 'au1.v1','au2.v1','au3.v1','inst1.v1','inst2.v1','inst3.v1',
                  
                  'ttct.v2', 'au1.v2','au2.v2','au3.v2','inst1.v2','inst2.v2','inst3.v2')

#t.test(apply(cO[,1:7],1,mean),apply(cO[,8:14],1,mean), paired=T)
cO$CompTOT.v1 <- apply(cO[,1:7],1,mean, na.rm=T)
cO$CompTOT.v2 <- apply(cO[,8:14],1,mean, na.rm=T)
cO <- cbind(as.factor(creO$StudyID),cO)
colnames(cO)[1] <- 'stuID'

# cO_F <- cO
# cFx_F <- cO

#corrplot(cor(cbind(cO_F$CompTOT.v1,cO_F$CompTOT.v2,cO_W$CompTOT.v1,cO_W$CompTOT.v2)), 'shade', col=c('lightblue','blue', 'green', 'black','grey','red', 'yellow'), addCoef.col='black')



write.xlsx(cO_F, '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/cO_F.xlsx',row.names=F)
write.xlsx(cFl_F, '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/cFl_F.xlsx',row.names=F)
write.xlsx(cFx_F, '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/cFx_F.xlsx',row.names=F)
write.xlsx(cO_W, '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/cO_W.xlsx',row.names=F)
write.xlsx(cFl_W, '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/cFl_W.xlsx',row.names=F)
write.xlsx(cFx_W, '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/cFx_W.xlsx',row.names=F)

cO <- cO
cFx <- cO
cFl <- cO
for (n in 2:dim(cO)[2]){
  cO[,n] <- apply(cbind(cO_F[,n],cO_W[,n]),1,mean)
  cFx[,n] <- apply(cbind(cFx_F[,n],cFx_W[,n]),1,mean)
  cFl[,n] <-apply(cbind(cFl_F[,n],cFl_W[,n]),1,mean)
}

colnames(cO)[1] <- 'ID'
colnames(cFx)[1] <- 'ID'
colnames(cFl)[1] <- 'ID'
write.xlsx(cO, '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/cO_final.xlsx',row.names=F)
write.xlsx(cFl, '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/cFl_final.xlsx',row.names=F)
write.xlsx(cFx, '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/cFx_final.xlsx',row.names=F)


# Plotting Results:
library(ggplot2)


ids <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/cO_F.xlsx',1)$stuID
load('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-11-29/summary/cogdat_cleaned.rda')


fl_v1 <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/cFl_final.xlsx',1)$CompTOT.v1
fl_v2 <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/cFl_final.xlsx',1)$CompTOT.v2
fx_v1 <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/cFx_final.xlsx',1)$CompTOT.v1
fx_v2 <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/cFx_final.xlsx',1)$CompTOT.v2
o_v1 <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/cO_final.xlsx',1)$CompTOT.v1
o_v2 <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/cO_final.xlsx',1)$CompTOT.v2

v <- as.data.frame(cbind(o_v1,o_v2))
colnames(v) <- c('beh1', 'beh2')
v$ID <- ids
ddat <- merge(cogdat_cleaned, v, by= 'ID')

dat_long <- data.frame(ID=as.factor(rep(ids, 2)),
                       group=as.factor(rep(ddat$group, 2)),
                       visit=as.factor(c(rep('V1',dim(ddat)[1]),rep('V2',dim(ddat)[1]))),
                       beh=c(ddat$beh1, ddat$beh2))
p <- ggplot(data = dat_long, aes(x = visit, y = beh, group = ID))
p + geom_line(aes(linetype= group))  + stat_smooth(aes(group = group, linetype= group), color='black', size=3) +
  stat_summary(aes(group = group, shape = group),geom = "point", fun.y = mean, size = 6)+theme_bw() +
  theme(axis.text=element_text(size=20), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank())  +
  ylab("COG")



t.test(fl_v1,fl_v2, paired=T)

# Run GMV part from SN*, then:
ddd_CRE <- data.frame(ID=as.factor(rep(ddat$ID,2)), visit=as.factor(c(rep('V1',dim(ddat)[1]),rep('V2',dim(ddat)[1]))), var=as.numeric(c(ddat$beh1,ddat$beh2)))
ddd_tot <- merge(ddd_CRE, gmvdat, by=c('ID', 'visit'))


d <- cbind(c(ddd_tot$res[ddd_tot$visit=='V2']-ddd_tot$res[ddd_tot$visit=='V1']),c(ddd_tot$var[ddd_tot$visit=='V2']-ddd_tot$var[ddd_tot$visit=='V1']))
dC <- cbind(c(ddd_tot$res[ddd_tot$visit=='V2'&ddd_tot$group=='con']-ddd_tot$res[ddd_tot$visit=='V1'&ddd_tot$group=='con']),c(ddd_tot$var[ddd_tot$visit=='V2'&ddd_tot$group=='con']-ddd_tot$var[ddd_tot$visit=='V1'&ddd_tot$group=='con']))
dA <- cbind(c(ddd_tot$res[ddd_tot$visit=='V2'&ddd_tot$group=='act']-ddd_tot$res[ddd_tot$visit=='V1'&ddd_tot$group=='act']),c(ddd_tot$var[ddd_tot$visit=='V2'&ddd_tot$group=='act']-ddd_tot$var[ddd_tot$visit=='V1'&ddd_tot$group=='act']))

cor(d)
cor(dA)
cor(dC)

### Tracts:

v <- as.data.frame(cbind(fl_v1,fl_v2,fx_v1,fx_v2,o_v1,o_v2))
colnames(v) <- c('fl1','fl2','fx1','fx2','o1', 'o2')

v$ID <- ids
ddat <- merge(cogdat_cleaned, v, by= 'ID')


# Set working directory:
dir = '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-11-29/summary/'
#dir = '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-11-23/summary/'

# Load data:
load(paste(dir, 'cogdat_cleaned.rda', sep=''))
excluded <- c('3035', '5037')


# Uncomment for outlier removal:
scogdat <- subset(cogdat_cleaned, is.element(cogdat_cleaned$ID, excluded)==F & cogdat_cleaned$PCAoutlier == 'No')
scogdat$G[scogdat$group=='con'] <- 0
scogdat$G[scogdat$group=='act'] <- 1
scogdat$G <- as.factor(scogdat$G)

# Calculate averaged updating performance:
scogdat$v1.NearUPD <- apply(scogdat[,c('v1.nearUpd.count.lvl2', 'v1.nearUpd.count.lvl4')],1, mean)
scogdat$v2.NearUPD <- apply(scogdat[,c('v2.nearUpd.count.lvl2', 'v2.nearUpd.count.lvl4')],1, mean)
scogdat$v1.TrainedUPD <- apply(scogdat[,c('v1.trainedUpd.count.lvl2', 'v1.trainedUpd.count.lvl4')],1, mean)
scogdat$v2.TrainedUPD <- apply(scogdat[,c('v2.trainedUpd.count.lvl2', 'v2.trainedUpd.count.lvl4')],1, mean)
# REASONING:
dat <- scogdat[,c('G','v1.rav', 'v2.rav', 'v1.beta', 'v2.beta', 'v1.wasi', 'v2.wasi', 'ID')]
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

scogdat$v1.reasComp <- comp1
scogdat$v2.reasComp <- comp2

CRE <- merge(scogdat, cO[,c('ID', 'CompTOT.v1','CompTOT.v2')], by='ID')
CRE <- merge(CRE, cFx[,c('ID', 'CompTOT.v1','CompTOT.v2')], by='ID')
CRE <- merge(CRE, cFl[,c('ID', 'CompTOT.v1','CompTOT.v2')], by='ID')



colnames(CRE)[is.element(colnames(CRE), c('CompTOT.v1.x','CompTOT.v2.x'))] <- c('CompTOT.v1.O','CompTOT.v2.O')
colnames(CRE)[is.element(colnames(CRE), c('CompTOT.v1.y','CompTOT.v2.y'))] <- c('CompTOT.v1.Fx','CompTOT.v2.Fx')
colnames(CRE)[is.element(colnames(CRE), c('CompTOT.v1','CompTOT.v2'))] <- c('CompTOT.v1.Fl','CompTOT.v2.Fl')

CRE$O.diff <- CRE$CompTOT.v2.O-CRE$CompTOT.v1.O
CRE$Fl.diff <- CRE$CompTOT.v2.Fl-CRE$CompTOT.v1.Fl
CRE$Fx.diff <- CRE$CompTOT.v2.Fx-CRE$CompTOT.v1.Fx
CRE$TrainedUPD.diff <- CRE$v2.TrainedUPD-CRE$v1.TrainedUPD
CRE$NearUPD.diff <- CRE$v2.NearUPD-CRE$v1.NearUPD
CRE$reasComp.diff <- CRE$v2.reasComp-CRE$v1.reasComp
CRE$near3back.diff <- CRE$near3back.OA.v2-CRE$near3back.OA.v1
CRE$trained3back.diff <- CRE$trained3back.OA.v2-CRE$trained3back.OA.v1
CRE$trained2back.diff <- CRE$trained2back.OA.v2-CRE$trained2back.OA.v1

CRE$trainedNback.diff <- apply(CRE[,c('trained2back.diff','trained3back.diff')],1,mean)
cor(CRE$O.diff,CRE$trained3back.diff, use='complete.obs')








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



