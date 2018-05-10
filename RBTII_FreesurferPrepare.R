# Prepare *.dat file for longitudinal analysis in Freesurfer

library(lavaan)
library(xlsx)
library(ggplot2)

dir = '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-11-29/summary/'

load(paste(dir, 'cogdat_cleaned.rda', sep=''))

scogdat <- cogdat_cleaned
scogdat$G[scogdat$group=='con'] <- 0
scogdat$G[scogdat$group=='act'] <- 1
scogdat$G <- as.factor(scogdat$G)

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


dat$SI1<- comp1
dat$SI2<- comp2


fs<- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/main_analysis/freesurfer/FS_data.xlsx',1)[,-3]
colnames(fs)[4] <- 'ID'
ffs <- as.data.frame(merge(fs,dat, by='ID'))

ffs[which(ffs$visit==1), 'SI'] <- ffs$SI1[which(ffs$visit==1)]
ffs[which(ffs$visit==2), 'SI'] <- ffs$SI2[which(ffs$visit==2)]

ffs$years <- as.numeric(rep(c(0,0.077),57))

ffs$group[ffs$G==1]<-'act'
ffs$group[ffs$G==0]<-'con'


fsdat <- ffs[,c('fsid', 'fsid.base', 'group', 'visit', 'years','SI','ID')]

colnames(fsdat)[c(2,7)]<-c('fsid-base','numid')
rownames(fsdat)<-c()

write.table(fsdat,file='/Volumes/REBOOTII/RBTII/PROCESSED/FSDIR/RBTII_long.dat', quote=F, sep=' ', row.names = F)

