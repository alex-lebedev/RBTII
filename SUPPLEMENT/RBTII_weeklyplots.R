# PJ: REBOOT-II (Progression Sheets)
# Author: Alexander V. Lebedev (2017-02-13)

# DESCRIPTION:
# The script produces progression plots for a given time-window (from date1 to date2)
# and saves them as png images for each subject

# In the present example, folder structure is as follows
# (these folders have to be created in advance):

# .....~/.../progress/
# ...................data/            (this is where your csv-files are located)
# .......................pics1/       (level-progress)
# .......................pics2/       (RT + accuracy)


# Steps:

# Load the libraries:
library(zoo)
library(RColorBrewer)

# Generate nice palette:
color <- brewer.pal(n = 8, "Dark2")

# Set working directory:
dir <- '/Users/alebedev/Documents/R/REBOOT2/progress/data/'

# Set up time-window:
date1 <- as.Date("2013-01-01") # from
date2 <- as.Date("2018-06-18") # till

# For specific waves
#id_low <- 0
#id_high <- 2000

###############
### N-back: ###
###############

# If column names are present:
nback <-  read.csv2(paste(dir, 'nback_run.csv', sep=''), header=T)

# If w/o column names:
#nback <- read.csv2(paste(dir, 'nback_run.csv', sep=''), header=F)
#colnames(nback) <- c("id","date","level","run","cr","fr","dv","levelpassed")

nback <- nback[which(nchar(nback$id)>3),]
#nback <- subset(nback, nback$id>id_low & nback$id<=id_high) # for specific wave

# Subset of those who were present in a particular time-window:
nback <- subset(nback, as.Date(nback$date)>date1 & as.Date(nback$date)<date2)

# Produce the list of relevant IDs:
ids <-names(table(nback$id))

# Prepare a data.frame to fill-in:
nb <- as.data.frame(matrix(NA, length(ids), 21))
colnames(nb) <- c('ID', paste("V", c(1:20), sep=''))
nb_acc <- nb
nb_t <- nb

# Start the loop:
for (i in 1:length(ids)) {
  # Readout:
  tmp <- subset(nback, nback$id==ids[i]) # select all data for the i-th subject
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract all dates for the i-th subject
  
    nb$ID[i] <- ids[i] # extract and assign the study ID
    nb_acc$ID[i] <- ids[i]
    nb_t$ID[i] <- ids[i]
    for (d in 1:length(dates)){
      tmpD <- tmp[as.Date(tmp$date)==dates[d],] # select all data for the i-th subject on the d-th date
      nb[i,d+1] <- tail(tmpD$level,1) # read and assign final level reached by the i-th subject on the d-th date
    }
    # Generate a png-image:
    png(paste(dir, 'pics1/',ids[i], '_nback_','prog', '.png', sep=''),1000,1000, pointsize=15)
    v <- nb[i,2:dim(nb)[2]]
    v <- v[is.na(v)==F]
    plot(1:length(v), v, type='b', col=color[1], lwd=4, xlab='Visit', ylab='Level',
         main='Progress', cex=3.2, xaxt='n', yaxt='n', ylim=c(1, max(v)+2),xlim=c(0.4, length(v)+0.4))
    axis(1, at=c(1:length(v)), labels=c(1:length(v)))
    points(1:length(v), v, pch=16, col=color[2], cex=3.2)
    text(1:length(v), v+0.25, paste('Nivå', v), col=color[2], cex=1)
    grid(length(v))
    dev.off()

    for (d in 1:length(dates)){
      tmpD <- tmp[as.Date(tmp$date)==dates[d],] # select all data for the i-th subject on the d-th date
      nb_acc[i,d+1] <-  round(mean(tmpD$dv[tmpD$dv>-1]/tmpD$cr[tmpD$dv>-1]),2) # assign accuracy reached by the i-th subject on the d-th date
      nb_t[i,d+1] <- round(90/mean(tmpD$cr),2) # assign RT reached by the i-th subject on the d-th date
    }
    # Generate a png-image:
    png(paste(dir, 'pics2/',ids[i], '_nback_','prog', '.png', sep=''),1000,1000, pointsize=15)
    
    t <- nb_t[i,2:dim(nb_t)[2]]
    t <- t[is.na(t)==F]
    
    plot(1:length(t), t, type='b', col=color[3], lwd=4, xlab='Visit', ylab='Hastighet',
         main='Progress: max nivå', cex=3.2, xaxt='n',yaxt='n', ylim=c(0, sort(t,partial=length(t)-1)[length(t)-1]+1.5), xlim=c(0.4, length(t)+0.4))
    axis(1, at=c(1:length(t)), labels=c(1:length(t)));axis(side = 2, at = c(), labels = FALSE)
    points(1:length(t), t, pch=16, col=color[2], cex=3.2)
    text(1:length(t), t+0.25, paste(t, 's/svar'), col=color[2], cex=1)
    
    v <- nb_acc[i,2:dim(nb_acc)[2]]
    v <- v[is.na(v)==F]
    lines(1:length(v), v,col=color[7], lwd=4, type='b')
    points(1:length(v), v, pch=16, col=color[1], cex=3.2)
    text (1:length(t), v+0.15, paste(v*100, '%'),col=color[1], cex=1)
    legend(3, sort(t,partial=length(t)-1)[length(t)-1]+1.5, c('Hastighet', 'Noggrannhet'), col=color[c(2,1)], pch=16,bty='n', cex=2)
    grid(length(v))
    dev.off()
}


#######################
### Task-switching: ###
#######################

tswitch <-  read.csv2(paste(dir, 'taskswitching_run.csv', sep=''), header=T)
# If w/o column names:
#tswitch <-  read.csv2(paste(dir, 'taskswitching_run.csv', sep=''), header=F)
#colnames(tswitch) <- c("id","date","level","run","cr","fr","dv","levelpassed")

tswitch <- tswitch[which(nchar(tswitch$id)>3),]
#tswitch <- subset(tswitch, tswitch$id>id_low & tswitch$id<=id_high) # for specific wave

# Subset of those who were present in a particular time-window:
tswitch <- subset(tswitch, as.Date(tswitch$date)>date1 & as.Date(tswitch$date)<date2)

# Produce the list of relevant IDs:
ids <-names(table(tswitch$id))

# Prepare a data.frame to fill-in:
tsw <- as.data.frame(matrix(NA, length(ids), 21))
colnames(tsw) <- c('ID', paste("V", c(1:20), sep=''))

tsw_acc <- tsw
tsw_t <- tsw


# Start the loop:
for (i in 1:length(ids)) {
  # Readout:
  tmp <- subset(tswitch, tswitch$id==ids[i]) # select all data for the i-th subject
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract all dates for the i-th subject
    
    tsw$ID[i] <- ids[i] # extract and assign the study ID
    tsw_acc$ID[i] <- ids[i] 
    tsw_t$ID[i] <- ids[i]
    for (d in 1:length(dates)){
      tmpD <- tmp[as.Date(tmp$date)==dates[d],] # select all data for the i-th subject on the d-th date
      tsw[i,d+1] <- tail(tmpD$level,1) # read and assign final level reached by the i-th subject on the d-th date
    }
    # Generate a png-image:
    png(paste(dir, 'pics1/',ids[i], '_tswitch_','prog', '.png', sep=''),1000,1000, pointsize=15)
    v <- tsw[i,2:dim(tsw)[2]]
    v <- v[is.na(v)==F]
    plot(1:length(v), v, type='b', col=color[1], lwd=4, xlab='Visit', ylab='Level',
         main='Progress', cex=3.2, xaxt='n', yaxt='n', ylim=c(1, max(v)+2),xlim=c(0.4, length(v)+0.4))
    axis(1, at=c(1:length(v)), labels=c(1:length(v)))
    points(1:length(v), v, pch=16, col=color[2], cex=3.2)
    text(1:length(v), v+0.25, paste('Nivå', v), col=color[2], cex=1)
    grid(length(v))
    dev.off()

    for (d in 1:length(dates)){
      tmpD <- tmp[as.Date(tmp$date)==dates[d],] # select all data for the i-th subject on the d-th date
      tsw_acc[i,d+1] <- round(mean(tmpD$dv[tmpD$dv>-1]/tmpD$cr[tmpD$dv>-1]),2) # assign accuracy reached by the i-th subject on the d-th date
      tsw_t[i,d+1] <- round(90/mean(tmpD$cr),2) # assign RT reached by the i-th subject on the d-th date
    }
    # Generate a png-image:
    png(paste(dir, 'pics2/',ids[i], '_tswitch_','prog', '.png', sep=''),1000,1000, pointsize=15)
    
    t <- tsw_t[i,2:dim(tsw_t)[2]]
    t <- t[is.na(t)==F]
    
    plot(1:length(t), t, type='b', col=color[3], lwd=4, xlab='Visit', ylab='Hastighet',
         main='Progress: max nivå', cex=3.2, xaxt='n',yaxt='n', ylim=c(0, sort(t,partial=length(t)-1)[length(t)-1]+1.5), xlim=c(0.4, length(t)+0.4))
    axis(1, at=c(1:length(t)), labels=c(1:length(t)));axis(side = 2, at = c(), labels = FALSE)
    points(1:length(t), t, pch=16, col=color[2], cex=3.2)
    text(1:length(t), t+0.25, paste(t, 's/svar'), col=color[2], cex=1)
    
    v <- tsw_acc[i,2:dim(tsw_acc)[2]]
    v <- v[is.na(v)==F]
    lines(1:length(v), v,col=color[7], lwd=4, type='b')
    points(1:length(v), v, pch=16, col=color[1], cex=3.2)
    text (1:length(t), v+0.15, paste(v*100, '%'),col=color[1], cex=1)
    legend(3, sort(t,partial=length(t)-1)[length(t)-1]+1.5, c('Hastighet', 'Noggrannhet'), col=color[c(2,1)], pch=16,bty='n', cex=2)
    grid(length(v))
    dev.off()
}


#################
### Updating: ###
#################

updating <- read.csv2(paste(dir, 'updating_run.csv', sep=''), header=T)
# If w/o column names:
#updating <- read.csv2(paste(dir, 'updating_run.csv', sep=''), header=F)
#colnames(tswitch) <- c("id","date","level","run","cr","fr","dv","levelpassed")

updating <- updating[which(nchar(updating$id)>3),]
#updating <- subset(updating, updating$id>id_low & updating$id<=id_high) # for specific wave

# Subset of those who were present in a particular time-window:
updating <- subset(updating, as.Date(updating$date)>date1 & as.Date(updating$date)<date2)

# Produce the list of relevant IDs:
ids <-names(table(updating$id))

# Prepare a data.frame to fill-in:
upd <- as.data.frame(matrix(NA, length(ids), 21))
colnames(upd) <- c('ID', paste("V", c(1:20), sep=''))
upd_acc <- upd
upd_t <- upd
# Start the loop:
for (i in 1:length(ids)) {
  # Readout:
  tmp <- subset(updating, updating$id==ids[i]) # select all data for the i-th subject
  dates <- as.Date(names(table(as.Date(tmp$date)))) # extract all dates for the i-th subject
    upd$ID[i] <- ids[i] # extract and assign the study ID
    upd_acc$ID[i] <- ids[i]
    upd_t$ID[i] <- ids[i]
    for (d in 1:length(dates)){
      tmpD <- tmp[as.Date(tmp$date)==dates[d],] # select all data for the i-th subject on the d-th date
      upd[i,d+1] <- tail(tmpD$level,1) # read and assign final level reached by the i-th subject on the d-th date
    }
    # Generate a png-image:
    png(paste(dir, 'pics1/',ids[i], '_updating_','prog', '.png', sep=''),1000,1000, pointsize=15)
    v <- upd[i,2:dim(upd)[2]]
    v <- v[is.na(v)==F]
    plot(1:length(v), v, type='b', col=color[1], lwd=4, xlab='Visit', ylab='Level',
         main='Progress', cex=3.2, xaxt='n', yaxt='n', ylim=c(1, max(v)+2),xlim=c(0.4, length(v)+0.4))
    axis(1, at=c(1:length(v)), labels=c(1:length(v)))
    points(1:length(v), v, pch=16, col=color[2], cex=3.2)
    text(1:length(v), v+0.25, paste('Nivå', v), col=color[2], cex=1)
    grid(length(v))
    dev.off()

    for (d in 1:length(dates)){
      tmpD <- tmp[as.Date(tmp$date)==dates[d],] # select all data for the i-th subject on the d-th date
      upd_acc[i,d+1] <-  round(mean(tmpD$dv[tmpD$dv>-1]/tmpD$cr[tmpD$dv>-1]),2) # assign accuracy reached by the i-th subject on the d-th date
      upd_t[i,d+1] <- round(90/mean(tmpD$cr),2) # assign RT reached by the i-th subject on the d-th date
    }
    # Generate a png-image:
    png(paste(dir, 'pics2/',ids[i], '_updating_','prog', '.png', sep=''),1000,1000, pointsize=15)
    
    t <- upd_t[i,2:dim(upd_t)[2]]
    t <- t[is.na(t)==F]
    
    plot(1:length(t), t, type='b', col=color[3], lwd=4, xlab='Visit', ylab='Hastighet',
         main='Progress: max nivå', cex=3.2, xaxt='n',yaxt='n', ylim=c(0, sort(t,partial=length(t)-1)[length(t)-1]+1.5), xlim=c(0.4, length(t)+0.4))
    axis(1, at=c(1:length(t)), labels=c(1:length(t)));axis(side = 2, at = c(), labels = FALSE)
    points(1:length(t), t, pch=16, col=color[2], cex=3.2)
    text(1:length(t), t+0.25, paste(t, 's/svar'), col=color[2], cex=1)
    
    v <- upd_acc[i,2:dim(upd_acc)[2]]
    v <- v[is.na(v)==F]
    lines(1:length(v), v,col=color[7], lwd=4, type='b')
    points(1:length(v), v, pch=16, col=color[1], cex=3.2)
    text (1:length(t), v+0.15, paste(v*100, '%'),col=color[1], cex=1)
    legend(3, sort(t,partial=length(t)-1)[length(t)-1]+1.5, c('Hastighet', 'Noggrannhet'), col=color[c(2,1)], pch=16,bty='n', cex=2)
    grid(length(v))
    dev.off()
    
}


##### END ####

# check progress:

rand1 <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/randomization/RandomizedWave1.xlsx',1)
rand2 <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/randomization/RandomizedWave2.xlsx',1)
rand3 <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/randomization/RandomizedWave3.xlsx',1)
rand4 <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/randomization/RandomizedWave4.xlsx',1)
rand5 <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/randomization/RandomizedWave5.xlsx',1)

rand <- rbind(rand1,rand2,rand3,rand4,rand5)


boxplot(nb[is.element(nb$ID,rand$stuID[rand$group=='con']),2:5],col=color[1])
boxplot(nb[is.element(nb$ID,rand$stuID[rand$group=='act']),2:5],col=color[2], add=T)

t.test(tsw[is.element(tsw$ID,rand$stuID[rand$group=='con']),3],tsw[is.element(tsw$ID,rand$stuID[rand$group=='act']),3])


write.xlsx(nb, file='/Users/alebedev/Documents/R/REBOOT2/progress/debrief/progress_data/progress_nb.xlsx')
write.xlsx(tsw, file='/Users/alebedev/Documents/R/REBOOT2/progress/debrief/progress_data/progress_tsw.xlsx')
write.xlsx(upd, file='/Users/alebedev/Documents/R/REBOOT2/progress/debrief/progress_data/progress_upd.xlsx')

# Plot feedback:
# Reorganize the data:


# Read progress data:
nb <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/progress/debrief/progress_data/progress_nb.xlsx',1)
tsw <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/progress/debrief/progress_data/progress_tsw.xlsx',1)
upd <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/progress/debrief/progress_data/progress_upd.xlsx',1)

# Read randomization data for a relevant wave:
rand <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/randomization/RandomizedWave3.xlsx',1)


# Run
dat <- nb
dat <- subset(dat,is.element(dat$ID, rand$stuID))
ids <- dat$ID

tab <- dat

tabr <- as.data.frame(matrix(NA, c(1:20)+20*(length(ids)-1),3))
colnames(tabr) <- c('id', 'Nivå', 'Besök')

for (i in 1:length(ids)){
  tabr[c(1:20)+20*(i-1),1] <- ids[i]
  tabr[c(1:20)+20*(i-1),2] <- t(tab[i,2:21])
  tabr[c(1:20)+20*(i-1),3] <- c(1:20)
}


ggplot(tabr) + 
 # geom_point(aes(x = Visit, y = Level), size = 3, data=tabr) +
  stat_smooth(aes(x = Besök, y = Nivå), method = "loess", se = T, size=3, span=0.95, data=tabr) +
  coord_cartesian(ylim = c(0, 20)) +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=0, hjust=1))
 