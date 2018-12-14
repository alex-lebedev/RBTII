# >>>>>>>>>>>>>>>>>>>>>>>>>>>
# >>> RBTII_3visitsPlot.R >>>
# >>>>>>>>>>>>>>>>>>>>>>>>>>>


# Authors: Alexander V. Lebedev & Martin Lovden
# Date: 2018-06-14
# Study: REBOOT-II (OSF: https://osf.io/aam9u/)
# Version for 3 indicators

'
Analysis of the primary outcomes in the REBOOT-II RCT (6-months follow-up data)
'

# Authors: Alexander V. Lebedev & Martin Lovden
# Date: 2018-06-14
# Version for 3 indicators

# Load Libraries:
library(lavaan)
library(xlsx)
library(ggplot2)
library(nlme)  

# Set working directory:
dir = '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-11-29/summary/'

# Load data:
load(paste(dir, 'cogdat_cleaned.rda', sep=''))

scogdat <- cogdat_cleaned
scogdat$G[scogdat$group=='con'] <- 0
scogdat$G[scogdat$group=='act'] <- 1
scogdat$G <- as.factor(scogdat$G)


# Primary End-Points (VISITS 1,2)

# Visuospatial Reasoning
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
dat$SR1 <- apply(tmp1, 1,mean)
dat$SR2 <- apply(tmp2, 1,mean)
datSR12 <- dat

# Verbal Reasoning:
dat <- scogdat[,c('G','v1.anl.transf.3root', 'v2.anl.transf.3root', 'v1.syll', 'v2.syll', 'v1.vinf', 'v2.vinf', 'ID')] # Verbal Reasoning
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

dat$VR1 <- apply(tmp1, 1,mean)
dat$VR2 <- apply(tmp2, 1,mean)
datVR12 <- dat



### VISIT 3 ###
# Set working directory:
dir = '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-11-29/summary/'

# Load data:
load(paste(dir, 'cogdat_cleaned.rda', sep='')) # main dataset
load('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2018-05-17FollowUp/summary/cogdat6m.rda') # 6-month follow-up

scogdat <- cogdat_cleaned
scogdat$G[scogdat$group=='con'] <- 0
scogdat$G[scogdat$group=='act'] <- 1
scogdat$G <- as.factor(scogdat$G)
scogdat <- merge(scogdat, cogdat6m, by='ID')


# Visuospatial Reasoning:
dat <- scogdat[,c('G','v1.rav', 'v3.rav', 'v1.beta', 'v3.beta', 'v1.wasi', 'v3.wasi', 'ID')] 
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
dat$SR1 <- apply(tmp1, 1,mean)
dat$SR3 <- apply(tmp2, 1,mean)
datSR13 <- dat


# Verbal Reasoning:
dat <- scogdat[,c('G','v1.anl', 'v3.anl', 'v1.syll', 'v3.syll', 'v1.vinf', 'v3.vinf', 'ID')] 
x1 <- as.numeric(as.vector(dat[,2]))
x2 <- as.numeric(as.vector(dat[,3]))
x1 <- x1^(1/3)
x2 <- x2^(1/3)
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

dat$VR1 <- apply(tmp1, 1,mean)
dat$VR3 <- apply(tmp2, 1,mean)
datVR13 <- dat


dat12 <- merge(datSR12[,c('ID', 'SR1', 'SR2', 'G')],datVR12[,c('ID', 'VR1', 'VR2', 'G')], by =c('ID','G'))
dat13 <- merge(datSR13[,c('ID', 'SR1', 'SR3', 'G')],datVR13[,c('ID', 'VR1', 'VR3', 'G')], by =c('ID','G'))

dat <- merge(dat12[,c('ID', 'SR1', 'SR2', 'VR1', 'VR2', 'G')],dat13[,c('ID', 'SR3', 'VR3', 'G')], by=c('ID','G'))

dat <- dat[!duplicated((dat$ID)),]
rownames(dat) <- c()
dat$G <- as.factor(1-as.numeric(as.vector(dat$G)))

var1 <- 'SR1'
var2 <- 'SR2'
var3 <- 'SR3'


dat_long <- data.frame(ID=as.factor(rep(dat$ID, 3)),
                       G=as.factor(rep(dat$G, 3)),
                       visit=as.factor(c(rep('V1',dim(dat)[1]),rep('V2',dim(dat)[1]),rep('V3',dim(dat)[1]))),
                       beh=c(dat[,var1],dat[,var2],dat[,var3]))

p <- ggplot(data = dat_long, aes(x = visit, y = beh, group = ID))
p + geom_line(aes(color= G, linetype=G),size=0.5)  + stat_summary(aes(group = G, color=G, shape = G), 
                                                                  geom = "point", fun.y = mean, size = 5)+
  stat_smooth(aes(group = G, linetype=G, colour=G, fill=G),size=3) +
  theme_bw() + theme(axis.text=element_text(size=20), axis.title.x=element_blank(),
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     panel.border = element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) + ylim(c(-3,3))



# Individual tests
load('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-11-29/summary/cogdat_cleaned.rda')
load('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-12-18-FollowUp/summary/cogdat_cleaned_fu.rda')

dat <- merge(cogdat_cleaned, cogdat_cleaned_fu, by=c('ID', 'group'))
  
  
var1 <- 'v1.rav.x'
var2 <- 'v2.rav.x'
var3 <- 'v1.rav.y'


dat_long <- data.frame(ID=as.factor(rep(dat$ID, 3)),
                       G=as.factor(rep(dat$group, 3)),
                       visit=as.factor(c(rep('V1',dim(dat)[1]),rep('V2',dim(dat)[1]),rep('V3',dim(dat)[1]))),
                       beh=c(dat[,var1],dat[,var2],dat[,var3]))
p <- ggplot(data = dat_long, aes(x = visit, y = beh, group = ID))
p + geom_line(aes(color= G)) + stat_smooth(aes(group = G)) + stat_summary(aes(group = G, color=G, shape = G), 
                                                                          geom = "point", fun.y = mean, size = 5)

# <<<<<<<<<<<<<<<
# <<< THE END <<<
# <<<<<<<<<<<<<<<