############################
## SEM for REBOOTII data: ##
##  (6-month follow-up)   ##
############################

# End-point: Spatial Reasoning


# Authors: Alexander V. Lebedev & Martin Lovden
# Date: 2018-05-25

# Load Libraries:
library(lavaan)
library(xlsx)
library(ggplot2)
library(nlme)  

# Set working directory:
dir = '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-11-29/summary/'

# Load data:
load(paste(dir, 'cogdat_cleaned.rda', sep='')) # main dataset
load('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2018-05-17FollowUp/summary/cogdat6m.rda') # 6-month follow-up


# Excluded subjects:
excluded <- c('3035', '5037')
# Uncomment for outlier removal:
#scogdat <- subset(cogdat_cleaned, is.element(cogdat_cleaned$ID, excluded)==F & cogdat_cleaned$PCAoutlier == 'No')
scogdat <- subset(cogdat_cleaned, is.element(cogdat_cleaned$ID, excluded)==F)
scogdat$G[scogdat$group=='con'] <- 0
scogdat$G[scogdat$group=='act'] <- 1
scogdat$G <- as.factor(scogdat$G)


scogdat <- merge(scogdat, cogdat6m, by='ID')

# REASONING:
dat <- scogdat[,c('G','v1.rav', 'v3.rav', 'v1.beta', 'v3.beta', 'v1.wasi', 'v3.wasi')]
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

t.test(comp1[dat$G==0],comp1[dat$G==1])
t.test(comp2[dat$G==0]-comp1[dat$G==0],comp2[dat$G==1]-comp1[dat$G==1])

ddd <- data.frame(ID=as.factor(rep(scogdat$ID,2)), group=as.factor(rep(dat$G,2)), visit=as.factor(c(rep('V1',dim(dat)[1]),rep('V2',dim(dat)[1]))), var=as.numeric(c(comp1,comp2)))
ddd <- na.exclude(ddd)
modME <- lme(var~group*visit,data=ddd, random=~1|ID)
summary(modME)

###############################

# Prepare data frame:

G <- dat$G
work <- as.data.frame(cbind(x1,y1,z1,x2,y2,z2,G))
#work <- work[complete.cases(work),]
work$G <- as.factor(work$G)

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
summary(fm4g, fit.measures=TRUE, standardized=TRUE)

