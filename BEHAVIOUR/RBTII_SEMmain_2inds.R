library(lavaan)
library(xlsx)
library(ggplot2)

dir = '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-11-29/summary/'
#dir = '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-11-23/summary/'

load(paste(dir, 'cogdat_cleaned.rda', sep=''))


# CHECK:
#cor(scogdat[,c('v1.fl1.cost.3root','v1.fl2.cost', 'v1.nearRSW1.cost', 'v1.nearRSW2.cost', 'v1.nearTSW.cost')], use='complete')

#excluded <- c('3035', '5037')
#excluded <- c('3035', '5037', '5069')
#scogdat <- subset(cogdat_cleaned, is.element(cogdat_cleaned$ID, excluded)==F & cogdat_cleaned$PCAoutlier == 'No')
#scogdat <- subset(cogdat_cleaned, is.element(cogdat_cleaned$ID, excluded)==F)
scogdat <- cogdat_cleaned
scogdat$v1.NBACKtrained <- apply(scogdat[,c('trained2back.OA.v1', 'trained3back.OA.v1')],1, mean)
scogdat$v2.NBACKtrained <- apply(scogdat[,c('trained2back.OA.v2', 'trained3back.OA.v2')],1, mean)
scogdat$v1.NBACKnear <- apply(scogdat[,c('near2back.OA.v1', 'near3back.OA.v1')],1, mean)
scogdat$v2.NBACKnear <- apply(scogdat[,c('near2back.OA.v2', 'near3back.OA.v2')],1, mean)
scogdat <- cogdat_cleaned
scogdat$G[scogdat$group=='con'] <- 0
scogdat$G[scogdat$group=='act'] <- 1
scogdat$G <- as.factor(scogdat$G)

# Calculate averaged updating performance:
scogdat$v1.NearUPD <- apply(scogdat[,c('v1.nearUpd.count.lvl2', 'v1.nearUpd.count.lvl4')],1, mean)
scogdat$v2.NearUPD <- apply(scogdat[,c('v2.nearUpd.count.lvl2', 'v2.nearUpd.count.lvl4')],1, mean)
scogdat$v1.TrainedUPD <- apply(scogdat[,c('v1.trainedUpd.count.lvl2', 'v1.trainedUpd.count.lvl4')],1, mean)
scogdat$v2.TrainedUPD <- apply(scogdat[,c('v2.trainedUpd.count.lvl2', 'v2.trainedUpd.count.lvl4')],1, mean)


# UPDATING:
#dat <- scogdat[,c('G','v1.NearUPD', 'v2.NearUPD', 'near3back.OA.v1', 'near3back.OA.v2')]
#dat <- scogdat[,c('G','v1.TrainedUPD', 'v2.TrainedUPD', 'trained3back.OA.v1', 'trained3back.OA.v2')]
#dat <- scogdat[,c('G','v1.srec', 'v2.srec', 'v1.vrec', 'v2.vrec')]
#x1 <- log1p(as.numeric(as.vector(dat[,2])))
#x2 <- log1p(as.numeric(as.vector(dat[,3])))
#y1 <- as.numeric(as.vector(dat[,4]))
#y2 <- as.numeric(as.vector(dat[,5]))

# No transform:
x1 <- as.numeric(as.vector(dat[,2]))
x2 <- as.numeric(as.vector(dat[,3]))
y1 <- as.numeric(as.vector(dat[,4]))
y2 <- as.numeric(as.vector(dat[,5]))

# RULE-SWITCHING:
dat <- scogdat[,c('G','v1.nearRSW1.cost', 'v2.nearRSW1.cost', 'v1.nearRSW2.cost', 'v2.nearRSW2.cost')]
x1 <- as.numeric(as.vector(dat[,2]))
x2 <- as.numeric(as.vector(dat[,3]))
y1 <- as.numeric(as.vector(dat[,4]))
y2 <- as.numeric(as.vector(dat[,5]))

x2 <- log1p(x2-min(na.exclude(x2)))
x1 <- log1p(x1-min(na.exclude(x1)))
y2 <- log1p(y2-min(na.exclude(y2)))
y1 <- log1p(y1-min(na.exclude(y1)))

# Default:
#x1 <- as.numeric(as.vector(dat[,2]))
#x2 <- as.numeric(as.vector(dat[,3]))
#y1 <- as.numeric(as.vector(dat[,4]))
#y2 <- as.numeric(as.vector(dat[,5]))



x1s <- (x1-mean(x1, na.rm=T))/sd(x1, na.rm=T)
x2s <- (x2-mean(x1, na.rm=T))/sd(x1, na.rm=T)
y1s <- (y1-mean(y1, na.rm=T))/sd(y1, na.rm=T)
y2s <- (y2-mean(y1, na.rm=T))/sd(y1, na.rm=T)


tmp1 <- cbind(x1s,y1s)
tmp2 <- cbind(x2s,y2s)

comp1 <- apply(tmp1, 1,mean)
comp2 <- apply(tmp2, 1,mean)

t.test(comp1[dat$G==0],comp1[dat$G==1])
t.test(comp2[dat$G==0]-comp1[dat$G==0],comp2[dat$G==1]-comp1[dat$G==1])

ddd <- data.frame(ID=as.factor(rep(scogdat$ID,2)), group=as.factor(rep(dat$G,2)), visit=as.factor(c(rep('vis1',62),rep('vis2',62))), var=as.numeric(c(comp1,comp2)))
ddd <- na.exclude(ddd)
modME <- lme(var~group*visit,data=ddd, random=~1|ID)
summary(modME)


############################
### Prepare data for SEM ###
############################

x1 <- as.numeric(as.vector(dat[,2]))
x2 <- as.numeric(as.vector(dat[,3]))
y1 <- as.numeric(as.vector(dat[,4]))
y2 <- as.numeric(as.vector(dat[,5]))

G <- dat$G
work <- as.data.frame(cbind(x1,y1,x2,y2,G))
work$G <- as.factor(work$G)

# Scale the data for standardized effect-sizes:
work$x2 <- (work$x2-mean(work$x1, na.rm=T))/sd(work$x1, na.rm=T)
work$x1 <- (work$x1-mean(work$x1, na.rm=T))/sd(work$x1, na.rm=T)
work$y2 <- (work$y2-mean(work$y1, na.rm=T))/sd(work$y1, na.rm=T)
work$y1 <- (work$y1-mean(work$y1, na.rm=T))/sd(work$y1, na.rm=T)


# I. Model Specification:

# Model 1 (free)

m1 <-  '
#latent variables
t1 =~ x1 + y1
t2 =~ x2 + y2
delta =~ 1*t2

#Regression
t2 ~ 1*t1

#Variances and Covariances
t1 ~~ delta
x1 ~~ x2
y1 ~~ y2
t2 ~~ 0*t2

#Define intercept and means
t1 ~ 1
delta ~ 1
t2 ~ 0*1
x1 ~ 0*1
x2 ~ 0*1
y1 ~ 1
y2 ~ 1
'

# Model 2 (weak asumption)
m2 <-  '
#latent variables
t1 =~ x1 + l1*y1
t2 =~ x2 + l1*y2
delta =~ 1*t2

#Regression
t2 ~ 1*t1

#Variances and Covariances
t1 ~~ delta
x1 ~~ x2
y1 ~~ y2
t2 ~~ 0*t2 

#Define intercept and means
t1 ~ 1
delta ~ 1
t2 ~ 0*1
x1 ~ 0*1
x2 ~ 0*1
y1 ~ 1
y2 ~ 1
'

# Model 3 (strong asumption)
m3 <-  '
#latent variables
t1 =~ x1 + l1*y1
t2 =~ x2 + l1*y2
delta =~ 1*t2

#Regression
t2 ~ 1*t1

#Variances and Covariances
t1 ~~ delta
x1 ~~ x2
y1 ~~ y2
t2 ~~ 0*t2 

#Define intercept and means
t1 ~ 1
delta ~ 1
t2 ~ 0*1
x1 ~ 0*1
x2 ~ 0*1
y1 ~ i1*1
y2 ~ i1*1
'

# Model 4 (strict asumption)
m4 <-  '
#latent variables
t1 =~ x1 + l1*y1
t2 =~ x2 + l1*y2
delta =~ 1*t2

#Regression
t2 ~ 1*t1

#Variances and Covariances
t1 ~~ delta
x1 ~~ x2
y1 ~~ y2
t2 ~~ 0*t2 
x1 ~~ e1*x1
x2 ~~ e1*x2 
y1 ~~ e2*y1 
y2 ~~ e2*y2 


#Define intercept and means
t1 ~ 1
delta ~ 1
t2 ~ 0*1
x1 ~ 0*1
x2 ~ 0*1
y1 ~ i1*1
y2 ~ i1*1
'
# WITH GROUP
# Strict assmption is met + G
m4g <-  '
#latent variables
t1 =~ x1 + l1*y1
t2 =~ x2 + l1*y2
delta =~ 1*t2

#Regression
t2 ~ 1*t1
delta ~ G # add

#Variances and Covariances
t1 ~~ delta
x1 ~~ x2
y1 ~~ y2
t2 ~~ 0*t2 
x1 ~~ e1*x1
x2 ~~ e1*x2 
y1 ~~ e2*y1 
y2 ~~ e2*y2 
G ~~ G # add

#Define intercept and means
t1 ~ 1
delta ~ 1
t2 ~ 0*1
x1 ~ 0*1
x2 ~ 0*1
y1 ~ i1*1
y2 ~ i1*1
G ~ 1 # add
'

# Model 3 (strong asumption) + G
m3g <-  '
#latent variables
t1 =~ x1 + l1*y1
t2 =~ x2 + l1*y2
delta =~ 1*t2

#Regression
t2 ~ 1*t1
delta ~ G # add

#Variances and Covariances
t1 ~~ delta
x1 ~~ x2
y1 ~~ y2
t2 ~~ 0*t2 
G ~~ G

#Define intercept and means
t1 ~ 1
delta ~ 1
t2 ~ 0*1
x1 ~ 0*1
x2 ~ 0*1
y1 ~ i1*1
y2 ~ i1*1
G ~ 1
'


# Model2 weak assumption is met + G 

m2g <-  '
#latent variables
t1 =~ x1 + l1*y1
t2 =~ x2 + l1*y2
delta =~ 1*t2

#Regression
t2 ~ 1*t1
delta ~ G # add

#Variances and Covariances
t1 ~~ delta
x1 ~~ x2
y1 ~~ y2
t2 ~~ 0*t2 
G ~~ G

#Define intercept and means
t1 ~ 1
delta ~ 1
t2 ~ 0*1
x1 ~ 0*1
x2 ~ 0*1
y1 ~ 1
y2 ~ 1
G ~ 1 # add
'


fm1 <- sem(m1, data=work, estimator='ml', missing='FIML')
fm2 <- sem(m2, data=work, estimator='ml', missing='FIML')
fm3 <- sem(m3, data=work, estimator='ml', missing='FIML')
fm4 <- sem(m4, data=work, estimator='ml', missing='FIML')

#summary(fm1, fit.measures=TRUE, standardized=TRUE)
#summary(fm2, fit.measures=TRUE, standardized=TRUE)
#summary(fm3, fit.measures=TRUE, standardized=TRUE)
#summary(fm4, fit.measures=TRUE, standardized=TRUE)


#To run chi2 diff tests
anova(fm1,fm2,fm3,fm4) # based on Chisq

# Run appropriate model:
fm4g <- sem(m4g, data=work, estimator='ml', missing='FIML')
summary(fm4g, fit.measures=TRUE, standardized=TRUE)
