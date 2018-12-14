# >>>>>>>>>>>>>>>>>>>>>>>>>>>>
# >>> RBTII_DebriefPlots.R >>>
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>


# Authors: Alexander V. Lebedev & Martin Lovden
# Date: 2018-04-20
# Study: REBOOT-II (OSF: https://osf.io/aam9u/)

'
Plotting progress data for REBOOTII debrief meetings.
'

# Load libraries:
library(ggplot2)
library(xlsx)

# Read progress data:
nb <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/progress/debrief/progress_data/progress_nb.xlsx',1)
tsw <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/progress/debrief/progress_data/progress_tsw.xlsx',1)
upd <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/progress/debrief/progress_data/progress_upd.xlsx',1)
nb[,3:dim(nb)[2]] <- apply(nb[,3:dim(nb)[2]],2,as.numeric)
tsw[,3:dim(tsw)[2]] <- apply(tsw[,3:dim(tsw)[2]],2,as.numeric)
upd[,3:dim(upd)[2]] <- apply(upd[,3:dim(upd)[2]],2,as.numeric)

# Read randomization data for a relevant wave:
rand <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/randomization/RandomizedWave5.xlsx',1)

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
dat <- subset(dat,is.element(dat$ID, rand$stuID))
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

p <- ggplot(data = tabr, aes(x = Visit, y = Level))
p + stat_smooth(aes(),size=3)+theme_bw() +
  theme(axis.text=element_text(size=20), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(),axis.ticks.x=element_blank())  +
  ylab("Nivå")+xlab('Besök') # + coord_cartesian(ylim = c(2, 10))


# <<<<<<<<<<<<<<<
# <<< THE END <<<
# <<<<<<<<<<<<<<<
