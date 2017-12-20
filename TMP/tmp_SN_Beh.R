# SNgmv~Beh correlation

library(R.matlab)
library(RColorBrewer)

# Generate nice palette:
color <- brewer.pal(n = 8, "Dark2")

SN <- readMat('/Volumes/REBOOTII/RBTII/ANALYSIS/GMV/FullFact/SNextracted.mat')$SN
gmv <- SN$y.struct[,,1]$Y


group <- c(rep(rep('con',27),2),rep(rep('act',28),2))
visit <- c(rep('V1',27),rep('V2',27),rep('V1',28),rep('V2',28))

gmvdat <- data.frame(gmv=gmv, group=as.factor(group), visit=as.factor(visit))

gmvdat$subj <- c(rep(paste('s', 1:27, sep=''),2),rep(paste('s', 28:55, sep=''),2))

modME <- lme(gmv~group*visit,data=gmvdat, random=~1|subj)
summary(modME)

boxplot(subset(gmvdat$gmv, gmvdat$group=='con' & gmvdat$visit=='V1'),
        subset(gmvdat$gmv, gmvdat$group=='con' & gmvdat$visit=='V2'),
        subset(gmvdat$gmv, gmvdat$group=='act' & gmvdat$visit=='V1'),
        subset(gmvdat$gmv, gmvdat$group=='act' & gmvdat$visit=='V2'),
        col=c(3,3,2,2)) 

#summary(glm(gmv~group*visit,data=gmvdat))

modME <- lme(gmv~rand,data=gmvdat, random=~1|subj)
gmvdat$res <- residuals(modME)
boxplot(subset(gmvdat$res, gmvdat$group=='con' & gmvdat$visit=='V1'),
        subset(gmvdat$res, gmvdat$group=='con' & gmvdat$visit=='V2'),
        subset(gmvdat$res, gmvdat$group=='act' & gmvdat$visit=='V1'),
        subset(gmvdat$res, gmvdat$group=='act' & gmvdat$visit=='V2'),
        col=c(3,3,2,2)) 



gmvdat$ID <- as.factor(rep(c('1049', '1065', '1081', '1113', '1129', '2050', '2066', '2114', '2130', '2178', '3003', '3019', '3051', '3131', '3147', '4004', '4036', '4084', '4116', '4148', '4180', '5069', '5117', '5133', '5149', '5165', '5181'),2),
rep(c('1001', '1017', '1097', '1145', '1161', '2002', '2034', '2098', '2194', '2210', '2226', '3067', '3083', '3099', '3115', '3163', '4052', '4068', '4100', '4132', '4164', '4228', '5005', '5053', '5085', '5101', '5197', '5213'),2))




# Set working directory:
dir = '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-11-29/summary/'
#dir = '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-11-23/summary/'

# Load data:
load(paste(dir, 'cogdat_cleaned.rda', sep=''))


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
dat <- scogdat[,c('ID','G','v1.rav', 'v2.rav', 'v1.beta', 'v2.beta', 'v1.wasi', 'v2.wasi')]
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

ddd_SR <- data.frame(ID=as.factor(rep(scogdat$ID,2)), group=as.factor(rep(dat$G,2)), visit=as.factor(c(rep('V1',62),rep('V2',62))), var=as.numeric(c(comp1,comp2)))

ddd_tot <- merge(ddd_SR, gmvdat, by=c('ID', 'visit'))[,c('ID', 'visit', 'var', 'gmv', 'group.y')]
colnames(ddd_tot)[3:5] <- c('SR', 'SNvol', 'group')



# TRAINED TASK-SWITCHING:
dat <- scogdat[,c('ID','G','v1.trainedTSW.lvl23.cost', 'v2.trainedTSW.lvl23.cost', 'v1.trainedTSW.lvl456.cost', 'v2.trainedTSW.lvl456.cost', 'v1.trainedTSW.lvl789.cost', 'v2.trainedTSW.lvl789.cost')]
x1 <- as.numeric(as.vector(dat[,2]))
x2 <- as.numeric(as.vector(dat[,3]))
y1 <- as.numeric(as.vector(dat[,4]))
y2 <- as.numeric(as.vector(dat[,5]))
z1 <- as.numeric(as.vector(dat[,6]))
z2 <- as.numeric(as.vector(dat[,7]))
x2 <- (x2-min(na.exclude(x2)))^1/2
x1 <- (x1-min(na.exclude(x1)))^1/2
y2 <- (y2-min(na.exclude(y2)))^1/2
y1 <- (y1-min(na.exclude(y1)))^1/2
z2 <- (z2-min(na.exclude(z2)))^1/2
z1 <- (z1-min(na.exclude(z1)))^1/2
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
ddd_TSW <- data.frame(ID=as.factor(rep(scogdat$ID,2)), group=as.factor(rep(dat$G,2)), visit=as.factor(c(rep('V1',62),rep('V2',62))), var=as.numeric(c(comp1,comp2)))


ddd_tot <- merge(ddd_tot, ddd_TSW, by=c('ID', 'visit'))[,c('ID', 'visit', 'var', 'SR', 'SNvol', 'group.x')]
colnames(ddd_tot)[c(3,6)] <- c('tTSW', 'group')

s1 <- subset(ddd_tot, ddd_tot$group=='con')

# SR~tTSW
cor(c(s1$tTSW[s1$visit=='V2']-s1$tTSW[s1$visit=='V1']),c(s1$SR[s1$visit=='V2']-s1$SR[s1$visit=='V1']), use='complete')


# SR~SNvol
cor(c(s1$SNvol[s1$visit=='V2']-s1$SNvol[s1$visit=='V1']),c(s1$SR[s1$visit=='V2']-s1$SR[s1$visit=='V1']), use='complete')

# tTSW~SNvol
cor(c(s1$SNvol[s1$visit=='V2']-s1$SNvol[s1$visit=='V1']),c(s1$tTSW[s1$visit=='V2']-s1$tTSW[s1$visit=='V1']), use='complete')

# Plot:
s1 <- subset(ddd_tot, ddd_tot$group=='con')
SNvolume.change <- s1$SNvol[s1$visit=='V2']-s1$SNvol[s1$visit=='V1']
Performance.change <- s1$SR[s1$visit=='V2']-s1$SR[s1$visit=='V1']
plot(SNvolume.change,Performance.change, col=color[1], pch=17, cex=1.5)
abline(summary(glm(Performance.change~SNvolume.change)),col=color[1], lwd=5)

s1 <- subset(ddd_tot, ddd_tot$group=='act')
SNvolume.change <- s1$SNvol[s1$visit=='V2']-s1$SNvol[s1$visit=='V1']
Performance.change <- s1$SR[s1$visit=='V2']-s1$SR[s1$visit=='V1']
points(SNvolume.change,Performance.change, col=color[2], pch=16, cex=1.5, add=T)
abline(summary(glm(Performance.change~SNvolume.change)),col=color[2], lwd=5, add=T)
legend(0.022,4, legend=c('con', 'act'), pch=c(17,16),col=c(color[1], color[2]), cex=1.5)



# Plot between-group difference in change:

var1 <- 'v1.rav'
var2 <- 'v2.rav'

dat_long <- data.frame(ID=as.factor(rep(scogdat$ID, 2)),
                       G=as.factor(rep(scogdat$group, 2)),
                       visit=as.factor(c(rep('V1',dim(scogdat)[1]),rep('V2',dim(scogdat)[1]))),
                       beh=c(scogdat[,var1],scogdat[,var2]))
p <- ggplot(data = dat_long, aes(x = visit, y = beh, group = ID))
p + geom_line(aes(color= G)) + stat_smooth(aes(group = G)) + stat_summary(aes(group = G, color=G, shape = G), 
                                                                          geom = "point", fun.y = mean, size = 5)
