############
# Progress #
############

# Libraries:
library(xlsx)
library(ggplot2)
library(nlme)

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
  nb[i,'nsess'] <- length(nb[i,3:dim(nb)[2]][!is.na(nb[i,3:dim(nb)[2]])])
  tsw[i,'maxlvl']<- max(tsw[i,3:dim(tsw)[2]], na.rm=T)
  tsw[i,'nsess'] <- length(tsw[i,3:dim(tsw)[2]][!is.na(tsw[i,3:dim(tsw)[2]])])
  upd[i,'maxlvl']<- max(upd[i,3:dim(upd)[2]], na.rm=T)
  upd[i,'nsess'] <- length(upd[i,3:dim(upd)[2]][!is.na(upd[i,3:dim(upd)[2]])])
}

# Run
dat <- upd
dat <- subset(dat,is.element(dat$ID, rand$ID))
ids <- dat$ID

tab <- dat
tab <- dat[,2:dim(dat)[2]]
tabr <- as.data.frame(matrix(NA, c(1:20)+20*(length(ids)-1),3))
colnames(tabr) <- c('ID', 'Level', 'Visit')

for (i in 1:length(ids)){
  tabr[c(1:20)+20*(i-1),1] <- as.numeric(as.vector(ids[i]))
  tabr[c(1:20)+20*(i-1),2] <- t(tab[i,2:21])
  tabr[c(1:20)+20*(i-1),3] <- c(1:20)
}

tabr <- merge(tabr, rand, by='ID')

p <- ggplot(data = tabr, aes(x = Visit, y = Level, group = group))
p + stat_smooth(aes(group = group, linetype=group, colour=group, fill=group),size=3)+theme_bw() +
  theme(axis.text=element_text(size=20), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.ticks.x=element_blank())  +
  ylab("Level")

modME <- lme(Level~group*Visit,data=tabr, random=~1|ID, na.action = na.omit)
summary(modME)

tabr <- rbind(tabr_upd,tabr_nb, tabr_tsw)
tabr$task <- as.factor(c(rep('upd', dim(tabr_upd)[1]),rep('nb', dim(tabr_nb)[1]),rep('tsw', dim(tabr_tsw)[1])))

modME <- lme(Level~group*Visit,data=tabr, random=~1|task, na.action = na.omit)
summary(modME)


# TMP:

# REASONING:
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
dat$SRdiff <- dat$SR2-dat$SR1

dat <- merge(dat[,c('ID', 'G', 'SRdiff')], nb[,c('ID', 'maxlvl')], by='ID')
dat <- merge(dat, tsw[,c('ID', 'maxlvl')],by='ID')
dat <- merge(dat, upd[,c('ID', 'maxlvl')], by='ID')
colnames(dat)[c(length(colnames(dat))-2):length(dat)] <- c('NBmax', 'TSWmax', 'UPDmax')
