# >>>>>>>>>>>>>>>>>>>
# >>> RBTII_AEs.R >>>
# >>>>>>>>>>>>>>>>>>>


# Authors: Alexander V. Lebedev & Martin Lovden
# Date: 2018-04-20
# Study: REBOOT-II (OSF: https://osf.io/aam9u/)

'
Analysis of adverse events, side-effects and blinding
'

library(xlsx)
library(reshape)
library(ggplot2)

load('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-11-29/summary/demogr.rda')

# AEs:
aes <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/main_analysis/CRFdata/AEs.xlsx',1)
#aes <- subset(aes, aes$Relationship.with.IMP!='no')
aedat <- cbind(names(table(aes$Subject.number)),table(aes$Subject.number))

colnames(aedat) <- c('ID', 'AEnum')


dat <- merge(demogr, aedat, by='ID', all=T)
dat$AEnum <- as.numeric(as.vector(dat$AEnum))
dat$AEnum[is.na(dat$AEnum)] <- 0

tbl = table(dat$group, dat$AEnum)
chisq.test(tbl) 


# Quality of Life:
qol <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/main_analysis/CRFdata/QoLeq5d.xlsx',2)

qol[,3:7] <- apply(qol[,3:7],2,as.numeric)

qoldat <- data.frame(ID=qol$StudieID, QOLscore=apply(qol[,3:7],1,sum),visit=rep(c('V1','V2'),dim(qol)[1]/2))


qoldatch <- data.frame(ID=qoldat$ID[qoldat$visit=='V2'],
                       QOLscore=qoldat$QOLscore[qoldat$visit=='V2']-qoldat$QOLscore[qoldat$visit=='V1'])

dat <- merge(demogr, qoldat, by='ID', all=T)
dat <- dat[complete.cases(dat),]

qol_ch <- c(dat$QOLscore[dat$visit=='V2']-dat$QOLscore[dat$visit=='V1'])
gr <- dat$group[dat$visit=='V2']

summary(glm(QOLscore~group*visit, data=dat))

tbl = table(dat$group, dat$QOLscore)
chisq.test(tbl) 



# Mood:

mood <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/main_analysis/CRFdata/mood.xlsx',1)

ids <- names(table(mood$Studie.ID))
ids <- ids[2:length(ids)]

genMood <- as.data.frame(matrix(NA, length(ids), 32))

colnames(genMood)<- 1:32

alert <- genMood
motiv <- genMood
sleep <- genMood
for (i in 1:length(ids)){
  tmp <- subset(mood, is.element(mood$Studie.ID, ids[i]))
  genMood[i,1:length(tmp$Hur.mår.du.idag.)]<- as.numeric(as.vector(tmp$Hur.mår.du.idag.))
  alert[i,1:length(tmp$Hur.alert.känner.du.dig.idag.)]<- as.numeric(as.vector(tmp$Hur.alert.känner.du.dig.idag.))
  motiv[i,1:length(tmp$Hur.motiverad.är.du.att.lösa.uppgifterna.idag.)] <- 
    as.numeric(as.vector(tmp$Hur.motiverad.är.du.att.lösa.uppgifterna.idag.))
  sleep[i,1:length(tmp$Hur.sov.du.inatt...1.10..1..bra..10..dåligt.)]<- as.numeric(as.vector(tmp$Hur.sov.du.inatt...1.10..1..bra..10..dåligt.))
  }

genMood$ID <- ids
alert$ID <- ids
motiv$ID <- ids
sleep$ID <- ids

dem <- demogr[,c('ID', 'group')]

dat <- merge(dem, genMood, by='ID', all=T)
mdata <- melt(dat, id=c('ID', 'group'))
mdata$variable<-as.numeric(as.vector(mdata$variable))


ggplot(mdata) + 
  stat_smooth(aes(x = variable, y = value, color=group), method = "loess", se = T, size=3, span=0.95, data=mdata) +
  coord_cartesian(ylim = c(1, 10)) +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=0, hjust=1))

modME <- lme(value~group*variable,data=mdata, random=~1|ID,na.action=na.exclude)
summary(modME)

modFE <- glm(value~group*variable,data=mdata, na.action=na.exclude)
summary(modFE)

t.test(dat$`1`[dat$group=='con'],dat$`1`[dat$group=='act'])




# Humör (POMS):
humor <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/main_analysis/CRFdata/humor.xlsx',1)

# Sleep:
diary <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/main_analysis/CRFdata/KSQ_diary.xlsx',2)

ids <- names(table(diary$StudieID))
ids <- ids[2:length(ids)]

item <- as.data.frame(matrix(NA, length(ids),  max(table(diary$StudieID))))

colnames(item)<- 1:6
n=4
for (i in 1:length(ids)){
  tmp <- subset(diary, is.element(diary$StudieID, ids[i]))
  item[i,1:length(tmp[,n])] <- as.numeric(as.vector(tmp[,n]))
}


item$ID <- ids

dem <- demogr[,c('ID', 'group')]
dat <- merge(dem, item, by='ID', all=T)
mdata <- melt(dat, id=c('ID', 'group'))
mdata$variable<-as.numeric(as.vector(mdata$variable))


ggplot(mdata) + 
  stat_smooth(aes(x = variable, y = value, color=group), method = "loess", se = T, size=3, span=0.95, data=mdata) +
  coord_cartesian(ylim = c(1, 50)) +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=0, hjust=1))

modME <- lme(value~group*variable,data=mdata, random=~1|ID,na.action=na.exclude)
summary(modME)
modFE <- glm(value~group*variable,data=mdata, na.action=na.exclude)
summary(modFE)
colnames(diary)[n]

# Humor:
ids <- names(table(humor$StudieID))
item <- as.data.frame(matrix(NA, length(ids),  max(table(humor$StudieID))))
colnames(item)<- 1:6
n=4
for (i in 1:length(ids)){
  tmp <- subset(humor, is.element(humor$StudieID, ids[i]))
  item[i,1:length(tmp[,n])] <- as.numeric(as.vector(tmp[,n]))
}

item$ID <- ids
dem <- demogr[,c('ID', 'group')]
dat <- merge(dem, item, by='ID', all=T)
mdata <- melt(dat, id=c('ID', 'group'))
mdata$variable<-as.numeric(as.vector(mdata$variable))

mdata <- mdata[complete.cases(mdata),]
ggplot(mdata) + 
  stat_smooth(aes(x = variable, y = value, color=group), method = "loess", se = T, size=3, span=0.95, data=mdata) +
  coord_cartesian(ylim = c(1, 10)) +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=0, hjust=1))

modME <- lme(value~group*variable,data=mdata, random=~1|ID,na.action=na.exclude)
summary(modME)
modFE <- glm(value~group*variable,data=mdata, na.action=na.exclude)
summary(modFE)
colnames(humor)[n]

#Blinding:
blinding <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/main_analysis/blinding.xlsx',1)
rand <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/main_analysis/demographics/rand.xlsx',1)

blind_dat <- merge(blinding, rand, by='ID')
blind_dat <- blind_dat[blind_dat$guess!=-1,]

blind_dat$guess <- as.numeric(blind_dat$guess)

guess <- table(blind_dat$guess, blind_dat$group)
chisq.test(guess)

# <<<<<<<<<<<<<<<
# <<< THE END <<<
# <<<<<<<<<<<<<<<