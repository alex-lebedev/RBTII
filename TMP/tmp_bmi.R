# BMI

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

