# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# >>> RBTII_DemogrCompliance.R >>>
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Authors: Alexander V. Lebedev & Martin Lovden
# Date: 2018-04-20
# Study: REBOOT-II (OSF: https://osf.io/aam9u/)

'
Demographics, descriptive statistics, BMI and blood pressure.
'

# Load Libraries:
library(xlsx)
library(ggplot2)
library(nlme)

####################################
### I. Demographics & Compliance ###
####################################

# Number of completed sessions:
# Load libraries:
library(ggplot2)
library(xlsx)

# Read progress data:
nb <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/main_analysis/demographics/progress_data/progress_nb.xlsx',1)
tsw <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/main_analysis/demographics/progress_data/progress_tsw.xlsx',1)
upd <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/main_analysis/demographics/progress_data/progress_upd.xlsx',1)
nb[,3:dim(nb)[2]] <- apply(nb[,3:dim(nb)[2]],2,as.numeric)
tsw[,3:dim(tsw)[2]] <- apply(tsw[,3:dim(tsw)[2]],2,as.numeric)
upd[,3:dim(upd)[2]] <- apply(upd[,3:dim(upd)[2]],2,as.numeric)
rand <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/main_analysis/demographics/rand.xlsx',1)

for(i in 1:dim(nb)[1]){
  nb[i,'maxlvl']<- max(nb[i,3:dim(nb)[2]], na.rm=T)
  nb[i,'nsess'] <- length(nb[i,3:dim(nb)[2]][!is.na(nb[i,3:dim(nb)[2]])])-1
}
dd <- merge(rand, nb[,c('ID', 'nsess')], by='ID')

mean(dd$nsess);sd(dd$nsess)
mean(dd$nsess[dd$group=='con']);sd(dd$nsess[dd$group=='con'])
mean(dd$nsess[dd$group=='act']);sd(dd$nsess[dd$group=='act'])

# MMSE and Education:
rand <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/main_analysis/demographics/rand.xlsx',1)
education <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/main_analysis/demographics/education_MMSE.xlsx',1)
mmse <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/main_analysis/demographics/education_MMSE.xlsx',2)


dd <- merge(rand, education, by='ID')
dd <- merge(dd, mmse, by='ID')

colnames(dd)[7:9] <- c('education', 'MMSE1', 'MMSE2')

dd[,7:9] <- apply(dd[,7:9], 2, as.numeric, as.vector)


median(dd$education[dd$group=='con'], na.rm=T);range(dd$education[dd$group=='con'], na.rm=T)
median(dd$education[dd$group=='act'], na.rm=T);range(dd$education[dd$group=='act'], na.rm=T)
wilcox.test(dd$education[dd$group=='con'],dd$education[dd$group=='act'])


median(dd$MMSE2[dd$group=='con'], na.rm=T);range(dd$MMSE2[dd$group=='con'], na.rm=T)
median(dd$MMSE2[dd$group=='act'], na.rm=T);range(dd$MMSE2[dd$group=='act'], na.rm=T)
wilcox.test(dd$MMSE2[dd$group=='con'],dd$MMSE2[dd$group=='act'])

#####################
### II. COGNITION ###
#####################

# Set working directory:
dir = '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-11-29/summary/'

# Load data:
load(paste(dir, 'cogdat_cleaned.rda', sep=''))
scogdat <- cogdat_cleaned

# Uncomment for outlier removal:
scogdat$G[scogdat$group=='con'] <- 0
scogdat$G[scogdat$group=='act'] <- 1
scogdat$G <- as.factor(scogdat$G)

# Calculate averaged updating performance:
scogdat$v1.NearUPD <- apply(scogdat[,c('v1.nearUpd.count.lvl2', 'v1.nearUpd.count.lvl4')],1, mean)
scogdat$v2.NearUPD <- apply(scogdat[,c('v2.nearUpd.count.lvl2', 'v2.nearUpd.count.lvl4')],1, mean)
scogdat$v1.TrainedUPD <- apply(scogdat[,c('v1.trainedUpd.count.lvl2', 'v1.trainedUpd.count.lvl4')],1, mean)
scogdat$v2.TrainedUPD <- apply(scogdat[,c('v2.trainedUpd.count.lvl2', 'v2.trainedUpd.count.lvl4')],1, mean)


# Prepare:
# REASONING:
dat <- scogdat[,c('G','v1.rav', 'v2.rav', 'v1.beta', 'v2.beta', 'v1.wasi', 'v2.wasi')]
#dat <- scogdat[,c('G','v1.anl.transf.3root', 'v2.anl.transf.3root', 'v1.syll', 'v2.syll', 'v1.vinf', 'v2.vinf')]

x1 <- as.numeric(as.vector(dat[,2]))
y1 <- as.numeric(as.vector(dat[,4]))
z1 <- as.numeric(as.vector(dat[,6]))


# TRAINED TASK-SWITCHING:
#dat <- scogdat[,c('G','v1.trainedTSW.lvl23.cost', 'v2.trainedTSW.lvl23.cost', 'v1.trainedTSW.lvl456.cost', 'v2.trainedTSW.lvl456.cost', 'v1.trainedTSW.lvl789.cost', 'v2.trainedTSW.lvl789.cost')]
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

# NEAR TASK-SWITCHING:
#dat <- scogdat[,c('G','v1.nearTSW.lvl23.cost', 'v2.nearTSW.lvl23.cost', 'v1.nearTSW.lvl456.cost', 'v2.nearTSW.lvl456.cost', 'v1.nearTSW.lvl789.cost', 'v2.nearTSW.lvl789.cost')]
x1 <- as.numeric(as.vector(dat[,2]))
x2 <- as.numeric(as.vector(dat[,3]))
y1 <- as.numeric(as.vector(dat[,4]))
y2 <- as.numeric(as.vector(dat[,5]))
z1 <- as.numeric(as.vector(dat[,6]))
z2 <- as.numeric(as.vector(dat[,7]))
x2 <- log1p(x2-min(na.exclude(x2)))
x1 <- log1p(x1-min(na.exclude(x1)))
y2 <- log1p(y2-min(na.exclude(y2)))
y1 <- log1p(y1-min(na.exclude(y1)))
z2 <- log1p(z2-min(na.exclude(z2)))
z1 <- log1p(z1-min(na.exclude(z1)))

###############
### COMPARE ###
###############

x1s <- (x1)/sd(x1, na.rm=T)
y1s <- (y1)/sd(y1, na.rm=T)
z1s <- (z1)/sd(z1, na.rm=T)

comp1 <- apply(cbind(x1s,y1s,z1s), 1,mean)

t.test(comp1[dat$G==0],comp1[dat$G==1])
sd(comp1[dat$G==0], na.rm=T)
sd(comp1[dat$G==1], na.rm=T)

########################
# BMI & Blood Pressure #
########################

library(xlsx)
library(ggplot2)


BMI = function(height,weight){
  return(weight/(height)^2)
}

biodat <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/main_analysis/CRFdata/BMIandBP.xlsx',1)
load('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-11-29/summary/demogr.rda')
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


ddd <- merge(dat, biodat, by='ID')


ddd[,c("SI1", "SI2", "Systolic.Blood.Pressure..mmHg.",
       "Diastolic.Blood.Pressure..mmHg.", "Height..cm.",
       "Weight..kg.")] <- apply(apply(ddd[,c("SI1", "SI2", "Systolic.Blood.Pressure..mmHg.",
                                             "Diastolic.Blood.Pressure..mmHg.", "Height..cm.",
                                             "Weight..kg.")], 2, as.vector),2,as.numeric)


# Blood pressure:
t.test(ddd$Systolic.Blood.Pressure..mmHg.[ddd$group=='con'],ddd$Systolic.Blood.Pressure..mmHg.[ddd$group=='act'])
t.test(ddd$Diastolic.Blood.Pressure..mmHg.[ddd$group=='con'],ddd$Diastolic.Blood.Pressure..mmHg.[ddd$group=='act'])




for (i in 1:dim(ddd)[1]){
  ddd$bmi[i] <- BMI(c(ddd$Height..cm.[i]/100),ddd$Weight..kg.[i])
}

d <- data.frame(ID = as.factor(rep(ddd$ID,2)), group = as.factor(rep(ddd$group,2)),
                SR = c(ddd$SI1,ddd$SI2), sbp = rep(ddd$Systolic.Blood.Pressure..mmHg.,2),
                dbp = rep(ddd$Diastolic.Blood.Pressure..mmHg.,2),
                bmi = rep(ddd$bmi,2), visit = c(rep(1, length(ddd$ID)),rep(2, length(ddd$ID))))


# Borderline-significant (p=0.07) groupcon:visit:bmi within-subject effect:
modME <- lme(SR~group*visit*bmi,data=d, random=~1|ID)
summary(modME)

# <<<<<<<<<<<<<<<
# <<< THE END <<<
# <<<<<<<<<<<<<<<

