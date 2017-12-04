####################################################
# Plot progress data for REBOOTII debrief meetings #
####################################################

# Load libraries:
library(ggplot2)
library(xlsx)

# Read progress data:
nb <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/progress/debrief/progress_data/progress_nb.xlsx',1)
tsw <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/progress/debrief/progress_data/progress_tsw.xlsx',1)
upd <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/progress/debrief/progress_data/progress_upd.xlsx',1)

# Read randomization data for a relevant wave:
rand <- read.xlsx2('/Users/alebedev/Documents/R/REBOOT2/randomization/RandomizedWave3.xlsx',1)


# Reorganize the data:
dat <- nb # run for nb, tsw and upd
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
