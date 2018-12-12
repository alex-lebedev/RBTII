# RBTII rand

library(xlsx)


#rinput <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/randomization/raw/input_W1.xlsx',1)[,c('stuID', 'sex', 'Age', 'Session')]
#rav <- read.csv('/Users/alebedev/Documents/R/REBOOT2/randomization/raw/raven_W1.csv', sep=';')

#rinput <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/randomization/raw/input_W3.xlsx',1)[,c('stuID', 'sex', 'Age', 'Session')]
rinput <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/randomization/raw/input_W5.xlsx',1)[,c('stuID', 'sex', 'Age', 'Session', 'rscore')]
#rav <- read.csv('/Users/alebedev/Documents/R/REBOOT2/randomization/raw/raven_W3.csv', sep=';')

subjs <- names(table(rav$id))
ravs <- as.data.frame(matrix(NA, length(subjs),2))

ravs[,1] <- subjs
colnames(ravs)<-c('stuID', 'rscore')

for (i in 1:length(subjs)){
  tmp <- subset(rav, rav$id==subjs[i])
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract dates for each subject
  tmpv1 <- tmp[as.Date(tmp$date) == dates[1],]
  
  ravs[i,1] <- subjs[i]
  ravs[i,2] <- sum(tmpv1$answer_is_correct)
}

rinput <- as.data.frame(merge(rinput, ravs, by='stuID'))

rinput$gender[rinput$sex=='M'] <- 1
rinput$gender[rinput$sex=='F'] <- 2
rinput$ses[rinput$Session=='M'] <- 1
rinput$ses[rinput$Session=='A'] <- 2

# Repeat:
gr <- c(rep('act', 7), rep('con', 7))
group <- as.factor(sample(gr, length(gr)))
dat <- cbind(rinput,group)
gr1 <- subset(dat, dat$group=='act')
gr2 <- subset(dat, dat$group=='con')
pv <- c(wilcox.test(as.numeric(gr1$rscore), as.numeric(gr2$rscore))$p.value,     
        wilcox.test(as.numeric(gr1$Age), as.numeric(gr2$Age))$p.value,     
        wilcox.test(as.numeric(gr1$gender), as.numeric(gr2$gender))$p.value,
        wilcox.test(as.numeric(gr1$ses), as.numeric(gr2$ses))$p.value)     

names(pv) <- c('rscore', 'Age', 'gender', 'Session')
pv
table(pv<0.5)

w3a <- dat[,c('stuID', 'sex', 'Age', 'Session','rscore', 'group')]

#hist(dat$Age[dat$group=='con'])

#write.xlsx(as.data.frame(w1a), '/Users/alebedev/Documents/R/REBOOT2/randomization/RandomizedWave1.xlsx', row.names=F)
#write.xlsx(as.data.frame(w2a), '/Users/alebedev/Documents/R/REBOOT2/randomization/RandomizedWave2.xlsx', row.names=F)
#write.xlsx(as.data.frame(w3a), '/Users/alebedev/Documents/R/REBOOT2/randomization/RandomizedWave3.xlsx', row.names=F)
write.xlsx(as.data.frame(w4a), '/Users/alebedev/Documents/R/REBOOT2/randomization/RandomizedWave4.xlsx', row.names=F)


# CHECK:

w1 <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/randomization/RandomizedWave1.xlsx',1)
w2 <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/randomization/RandomizedWave2.xlsx',1)
w3 <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/randomization/RandomizedWave3.xlsx',1)
w4 <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/randomization/RandomizedWave4.xlsx',1)
w5 <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/randomization/RandomizedWave5.xlsx',1)


ww <- rbind(w1,w2,w3,w4,w5)
ww$gender[ww$sex=='M'] <- 1
ww$gender[ww$sex=='F'] <- 2
ww$gender[ww$sex=='m'] <- 1
ww$gender[ww$sex=='f'] <- 2
ww$ses[ww$Session=='morn'] <- 1
ww$ses[ww$Session=='afternoon'] <- 2
ww$ses[ww$Session=='M'] <- 1
ww$ses[ww$Session=='A'] <- 2
save('ww')

gr1 <- subset(ww, ww$group=='act')
gr2 <- subset(ww, ww$group=='con')
pv <- c(t.test(as.numeric(gr1$rscore), as.numeric(gr2$rscore))$p.value,     
        t.test(as.numeric(gr1$Age), as.numeric(gr2$Age))$p.value,     
        t.test(as.numeric(gr1$gender), as.numeric(gr2$gender))$p.value,
        t.test(as.numeric(gr1$ses), as.numeric(gr2$ses))$p.value)  

names(pv) <- c('rscore', 'Age', 'gender', 'Session')
pv
table(pv<0.5)


