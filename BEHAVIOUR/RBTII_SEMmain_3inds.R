#############################
## SEM for REBOOTII data: ###
#############################

# Authors: Alexander V. Lebedev & Martin Lovden
# Date: 2017-12-04
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

# Calculate averaged N-back performance:
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


# Primary End-Points: reasoning abilities (aka Fluid Intelligence):
dat <- scogdat[,c('G','v1.rav', 'v2.rav', 'v1.beta', 'v2.beta', 'v1.wasi', 'v2.wasi')] # Visuospatial Reasoning
#dat <- scogdat[,c('G','v1.anl.transf.3root', 'v2.anl.transf.3root', 'v1.syll', 'v2.syll', 'v1.vinf', 'v2.vinf')] # Verbal Reasoning
# For secondary time-points refer to Line xxx

############################
### Prepare data for SEM ###
############################

x1 <- as.numeric(as.vector(dat[,2]))
x2 <- as.numeric(as.vector(dat[,3]))
y1 <- as.numeric(as.vector(dat[,4]))
y2 <- as.numeric(as.vector(dat[,5]))
z1 <- as.numeric(as.vector(dat[,6]))
z2 <- as.numeric(as.vector(dat[,7]))
G <- dat$G
work <- as.data.frame(cbind(x1,y1,z1,x2,y2,z2,G))
work$G <- as.factor(work$G)

# Scale the data for standardized effect-sizes:
work$x2 <- (work$x2-mean(work$x1, na.rm=T))/sd(work$x1, na.rm=T)
work$x1 <- (work$x1-mean(work$x1, na.rm=T))/sd(work$x1, na.rm=T)
work$y2 <- (work$y2-mean(work$y1, na.rm=T))/sd(work$y1, na.rm=T)
work$y1 <- (work$y1-mean(work$y1, na.rm=T))/sd(work$y1, na.rm=T)
work$z2 <- (work$z2-mean(work$z1, na.rm=T))/sd(work$z1, na.rm=T)
work$z1 <- (work$z1-mean(work$z1, na.rm=T))/sd(work$z1, na.rm=T)


# I. Model Specification:

# Model 1 (free)
m1 <-  '
#latent variables
t1 =~ x1 + y1 + z1 
t2 =~ x2 + y2 + z2
delta =~ 1*t2

#Regression
t2 ~ 1*t1

#Variances and Covariances
t1 ~~ delta
x1 ~~ x2
y1 ~~ y2
z1 ~~ z2
t2 ~~ 0*t2

#Define intercept and means
t1 ~ 1
delta ~ 1
t2 ~ 0*1
x1 ~ 0*1
x2 ~ 0*1
y1 ~ 1
y2 ~ 1
z1 ~ 1
z2 ~ 1
'

# Model 2 (weak asumption)
m2 <-  '
#latent variables
t1 =~ x1 + l1*y1 + l2*z1 
t2 =~ x2 + l1*y2 + l2*z2
delta =~ 1*t2

#Regression
t2 ~ 1*t1

#Variances and Covariances
t1 ~~ delta
x1 ~~ x2
y1 ~~ y2
z1 ~~ z2
t2 ~~ 0*t2 

#Define intercept and means
t1 ~ 1
delta ~ 1
t2 ~ 0*1
x1 ~ 0*1
x2 ~ 0*1
y1 ~ 1
y2 ~ 1
z1 ~ 1
z2 ~ 1
'

# Model 3 (strong asumption)
m3 <-  '
#latent variables
t1 =~ x1 + l1*y1 + l2*z1 
t2 =~ x2 + l1*y2 + l2*z2
delta =~ 1*t2

#Regression
t2 ~ 1*t1

#Variances and Covariances
t1 ~~ delta
x1 ~~ x2
y1 ~~ y2
z1 ~~ z2
t2 ~~ 0*t2 

#Define intercept and means
t1 ~ 1
delta ~ 1
t2 ~ 0*1
x1 ~ 0*1
x2 ~ 0*1
y1 ~ i1*1
y2 ~ i1*1
z1 ~ i2*1
z2 ~ i2*1
'

# Model 4 (strict asumption)
m4 <-  '
#latent variables
t1 =~ x1 + l1*y1 + l2*z1 
t2 =~ x2 + l1*y2 + l2*z2
delta =~ 1*t2

#Regression
t2 ~ 1*t1

#Variances and Covariances
t1 ~~ delta
x1 ~~ x2
y1 ~~ y2
z1 ~~ z2
t2 ~~ 0*t2 
x1 ~~ e1*x1
x2 ~~ e1*x2 
y1 ~~ e2*y1 
y2 ~~ e2*y2 
z1 ~~ e3*z1 
z2 ~~ e3*z2 

#Define intercept and means
t1 ~ 1
delta ~ 1
t2 ~ 0*1
x1 ~ 0*1
x2 ~ 0*1
y1 ~ i1*1
y2 ~ i1*1
z1 ~ i2*1
z2 ~ i2*1
'
# WITH GROUP
# Strict assmption is met + G
m4g <-  '
#latent variables
t1 =~ x1 + l1*y1 + l2*z1 
t2 =~ x2 + l1*y2 + l2*z2
delta =~ 1*t2

#Regression
t2 ~ 1*t1
delta ~ G # add

#Variances and Covariances
t1 ~~ delta
x1 ~~ x2
y1 ~~ y2
z1 ~~ z2
t2 ~~ 0*t2 
x1 ~~ e1*x1
x2 ~~ e1*x2 
y1 ~~ e2*y1 
y2 ~~ e2*y2 
z1 ~~ e3*z1 
z2 ~~ e3*z2 
G ~~ G # add

#Define intercept and means
t1 ~ 1
delta ~ 1
t2 ~ 0*1
x1 ~ 0*1
x2 ~ 0*1
y1 ~ i1*1
y2 ~ i1*1
z1 ~ i2*1
z2 ~ i2*1
G ~ 1 # add
'

# Model 3 (strong asumption) + G
m3g <-  '
#latent variables
t1 =~ x1 + l1*y1 + l2*z1 
t2 =~ x2 + l1*y2 + l2*z2
delta =~ 1*t2

#Regression
t2 ~ 1*t1
delta ~ G # add

#Variances and Covariances
t1 ~~ delta
x1 ~~ x2
y1 ~~ y2
z1 ~~ z2
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
z1 ~ i2*1
z2 ~ i2*1
G ~ 1
'


# Model2 weak assumption is met + G 

m2g <-  '
#latent variables
t1 =~ x1 + l1*y1 + l2*z1 
t2 =~ x2 + l1*y2 + l2*z2
delta =~ 1*t2

#Regression
t2 ~ 1*t1
delta ~ G # add

#Variances and Covariances
t1 ~~ delta
x1 ~~ x2
y1 ~~ y2
z1 ~~ z2
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
z1 ~ 1
z2 ~ 1
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
summary(fm4g, fit.measures=TRUE, standardized=F)

############################
### Secondary End-Points ###
############################

# TRAINED AND NEAR TASK-SWITCHING:
#dat <- scogdat[,c('G','v1.trainedTSW.lvl23.cost', 'v2.trainedTSW.lvl23.cost', 'v1.trainedTSW.lvl456.cost', 'v2.trainedTSW.lvl456.cost', 'v1.trainedTSW.lvl789.cost', 'v2.trainedTSW.lvl789.cost')]
#dat <- scogdat[,c('G','v1.nearTSW.lvl23.cost', 'v2.nearTSW.lvl23.cost', 'v1.nearTSW.lvl456.cost', 'v2.nearTSW.lvl456.cost', 'v1.nearTSW.lvl789.cost', 'v2.nearTSW.lvl789.cost')]

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


#############
### PLOTS ###
#############

# Libraries:
library(semPlot)

# This is a detailed scheme of the fitted SEM:
semPaths(fm4g)
# A simplified version is provided in the paper supplement.


# REFERENCE: http://sachaepskamp.com/semPlot/examples
