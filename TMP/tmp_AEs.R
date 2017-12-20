# AEs and QoL:

library(xlsx)

load('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-11-29/summary/demogr.rda')

# AEs:
aes <- read.xlsx2('/Volumes/REBOOTII/RBTII/CRFdata/AEs.xlsx',1)
#aes <- subset(aes, aes$Relationship.with.IMP!='no')
aedat <- cbind(names(table(aes$Subject.number)),table(aes$Subject.number))

colnames(aedat) <- c('ID', 'AEnum')


dat <- merge(demogr, aedat, by='ID', all=T)
dat$AEnum <- as.numeric(as.vector(dat$AEnum))
dat$AEnum[is.na(dat$AEnum)] <- 0

tbl = table(dat$group, dat$AEnum)
chisq.test(tbl) 



qol <- read.xlsx2('/Volumes/REBOOTII/RBTII/CRFdata/QoLeq5d.xlsx',2)

qol[,3:7] <- apply(qol[,3:7],2,as.numeric)

qoldat <- data.frame(ID=qol$StudieID, QOLscore=apply(qol[,3:7],1,sum),visit=rep(c('V1','V2'),dim(qol)[1]/2))


qoldatch <- data.frame(ID=qoldat$ID[qoldat$visit=='V2'],
                       QOLscore=qoldat$QOLscore[qoldat$visit=='V2']-qoldat$QOLscore[qoldat$visit=='V1'])

dat <- merge(demogr, qoldat, by='ID', all=T)

summary(glm(QOLscore~group*visit, data=dat))

tbl = table(dat$group, dat$QOLscore)
chisq.test(tbl) 




