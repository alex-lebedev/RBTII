##############
# RBTII rand #
##############

# Author: Alexander Lebedev
# Date: 2016-11-05

# DESCRIPTION:
# Randomization for Reboot-II
# The script first asks you to select an input file, then performs 
# virtual dice rolls (default number of rolls is 5000) with random group assignments
# and, after running a number of non-parametric tests, selects most optimal
# group assignment vector minimizing between-group differences in:
# age, rscore
 
# The ouptut is saved as "[filename]_randomized.xlsx" in the working directory

# NB!
# Install libraries before running the script for the first time (uncomment to execute):
# install.packages('xlsx') 

############################# START #####################################

rm(list=ls())
# Load libraries:
library(xlsx)

# Select Example data:
iFile <- file.choose()
oFile <- paste(gsub(".xlsx", "", iFile), '_randomized.xlsx', sep='')
rinput <- read.xlsx(iFile,1)[,c('ids', 'age', 'rscore', 'gender')]

nperm = 5000 # controls number of dice rolls, increase for larger samples and/or if you want to make it more precise
group <- as.data.frame(matrix(NA,nperm, dim(rinput)[1]))

ppv <- matrix(NA, nperm, 4)

# The loop starts dice rolls (takes time to run):
for (i in 1:nperm){
  # Produce splits:
  n=dim(rinput)[1]
  if (n/2-round(n/2)==0){
    ogr <- paste(sample(c(1:2)), sep='')
    gr <- c(rep(ogr[1],n/2),rep(ogr[2],n/2))
  } else {
    ogr <- paste(sample(c(1:2)), sep='')
    gr <- c(rep(ogr[1],n/2),rep(ogr[2],n/2+1))
  }
  group[i,] <- as.factor(sample(gr, length(gr)))
  dat <- cbind(rinput,t(group[i,]))
  colnames(dat)[dim(dat)[2]] <- 'group'
  gr1 <- subset(dat, dat$group=='1')
  gr2 <- subset(dat, dat$group=='2')
  
  # Testing:
  pv <- c(
          wilcox.test(as.numeric(gr1$age), as.numeric(gr2$age))$p.value,     
          wilcox.test(as.numeric(gr1$rscore), as.numeric(gr2$rscore))$p.value,
          chisq.test(table(gr1$gender),table(gr1$gender)))

          pv[pv=='NaN']<-0 # gets rid of NaNs
          ppv[i,] <- c(pv,table(pv<0.6)['TRUE'],table(pv==0)['FALSE'])
}

# Clean the outut:
ppv[is.na(ppv[,3]),3] <- 0
min_ppv <- min(ppv[which(ppv[,4]>=2),3])  # minimal number of occasions where p<0.6
group <- group[which(ppv[,4]>=2),]
ppv <- ppv[which(ppv[,4]>=2),1:3]

group <- group[ppv[,3]==min_ppv,]
pv <- ppv[ppv[,3]==min_ppv,]

if (is.null(dim(pv)[1])){
  print('Your raw p-value vector is p:')
  print(round(pv[1:2], 2))
  pvm <- round(mean(pv[1:2]),2)
  print(paste('mean p =', pvm))
  dataset <- cbind(rinput,t(group))
} else {
  pvm <- apply(pv[,1:2],1,mean)
  print('Your raw p-value vector is p:')
  print(round(pv[which(pvm==max(pvm)),1:2],2))
  dataset <- cbind(rinput,t(group[which(pvm==max(pvm))[1],]))
  print(paste('mean p =', round(max(pvm),2)))
}
colnames(dataset)[dim(dataset)[2]] <- 'group'

# Look at the resulting dataset:
head(dataset)

t.test(dataset$age[dataset$group==1],dataset$age[dataset$group==2])
t.test(dataset$rscore[dataset$group==1],dataset$rscore[dataset$group==2])

# Save the output as xlsx (change the directory!):
write.xlsx(as.data.frame(dataset), oFile, row.names=F)
