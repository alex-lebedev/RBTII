##############################################################
# Resting State fMRI data analysis for the REBOOT-II project #
##############################################################

# Author: Alexander V. Lebedev
# Date: 2018-03-01

# Load libraries:
library(lavaan)
library(xlsx)
library(ggplot2)
library(R.matlab)



ids <- sub('sub-*','',dir('/Volumes/REBOOTII/RBTII/PROCESSED/DPABI_rest/fmriprep_Glob/FunImgPW'))
gr <- rep(NA, length(ids))
load('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-11-29/summary/cogdat_cleaned.rda')

gr[is.element(ids,subset(cogdat_cleaned$ID, cogdat_cleaned$group=='con'))] <- 0
gr[is.element(ids,subset(cogdat_cleaned$ID, cogdat_cleaned$group=='act'))] <- 1

coord <- read.table('/Volumes/REBOOTII/NBS/PrePost_ch/results/map/RBT1.node')[,1:3]
rownames(coord)<-c()
colnames(coord)<-c()

write.table(coord, file='/Volumes/REBOOTII/RBTII/PROCESSED/NBS/CraddockCoord.txt',quote = F,col.names = F, row.names = F)



M <- readMat('~/Desktop/NBS/M_rest_fmriprepGlobal.mat')
S<-readMat('/Volumes/REBOOTII/RBTII/PROCESSED/NBS/str_rest_fmriprepGlobal.mat')

p <- 1:200
roi=37;
for (n in 1:200){
    dat <- data.frame(fc=M$M[roi,n,], gr=as.factor(rep(gr,2)), visit=as.factor(c(rep(1,dim(M$M)[3]/2),rep(2,dim(M$M)[3]/2))))
    p[n] <- summary(glm(fc~gr*visit, data=dat))$coeff[4,4]
  }
which(p<0.05)


# Nodal Strength:
p <- 1:200
for (n in 1:200){
  dat <- data.frame(str=c(S$str1[,n],S$str2[,n]), gr=as.factor(rep(gr,2)),
                    visit=as.factor(c(rep(1,dim(S$str1)[1]),rep(2,dim(S$str2)[1]))))
  p[n] <- summary(glm(str~gr*visit, data=dat))$coeff[4,4]
}
which(p<0.05)

# Participation coefficient:
p <- 1:200
for (n in 1:200){
  dat <- data.frame(Ppos=c(S$Ppos1[,n],S$Ppos2[,n]), gr=as.factor(rep(gr,2)),
                    visit=as.factor(c(rep(1,dim(S$Ppos1)[1]),rep(2,dim(S$Ppos2)[1]))))
  p[n] <- summary(glm(Ppos~gr*visit, data=dat))$coeff[4,4]
}
which(p<0.05)



# Mapping connectivity results:

mtx <- readMat('/Users/alebedev/Desktop/NBS/result_WS_Glob_fmriprep_PosNBS_NoThld.mat')
nodes <- mtx$nbs[[2]][[6]]
write.table(nodes,quote=F, col.names=F,row.names=F,
            paste('/Users/alebedev/Desktop/NBS/Glob_fmriprep_Pos.edge', sep=''))


# Correlation with Behavioural Data:

# Load Matrices:
#selFC <- which(nodes >4, arr.ind = TRUE)[c(1,2,4),]
selFC <- which(nodes >4, arr.ind = TRUE)[c(1,3,4,6,7,8),]

#M <- readMat('/Users/alebedev/Desktop/NBS/M_rest_fmriprepGlobal.mat')
M <- readMat('/Users/alebedev/Desktop/NBS/M_rest_fmriprepNoThld.mat')

fcdat <- as.data.frame(matrix(NA, dim(M$M)[3],dim(selFC)[1]))
for(n in 1:dim(selFC)[1]){
  fcdat[,n] <- M$M[selFC[n,'row'],selFC[n,'col'],]
}
#colnames(fcdat) <- c(paste(as.vector(selFC[1,]),collapse='-'),paste(as.vector(selFC[2,]),collapse='-'),paste(as.vector(selFC[3,]),collapse='-'))
colnames(fcdat) <- c(paste(as.vector(selFC[1,]),collapse='-'),paste(as.vector(selFC[2,]),collapse='-'),paste(as.vector(selFC[3,]),collapse='-'))
colnames(fcdat) <- c(paste(as.vector(selFC[1,]),collapse='-'),
                     paste(as.vector(selFC[2,]),collapse='-'),
                     paste(as.vector(selFC[3,]),collapse='-'),
                     paste(as.vector(selFC[4,]),collapse='-'),
                     paste(as.vector(selFC[5,]),collapse='-'),
                     paste(as.vector(selFC[6,]),collapse='-'))

ids <- sub('sub-*','',dir('/Volumes/REBOOTII/RBTII/PROCESSED/DPABI_rest/fmriprep_Glob/FunImgPW'))

fcdat$ID <- rep(ids,2)
fcdat$visit <- c(rep('V1',length(ids)),rep('V2',length(ids)))

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


fcdat1 <- subset(fcdat, fcdat$visit=='V1')
fcdat2 <- subset(fcdat, fcdat$visit=='V2')
df1 <- as.data.frame(merge(fcdat1,dat[,c('ID','group', 'SI1')],by='ID'))
df2 <- as.data.frame(merge(fcdat2,dat[,c('ID','group', 'SI2')],by='ID'))
colnames(df1)[10] <- 'SI'
colnames(df2)[10] <- 'SI'
#colnames(df1)[7] <- 'SI'
#colnames(df2)[7] <- 'SI'


df <- rbind(df1,df2)




# Sig:
n=2; summary(glm(df[,n]~group*visit*SI, data=df))

ddf <- df[,c('ID','visit','group', '171-68')]
colnames(ddf)[4] <- 'fc' 
ddf$visit <- as.factor(ddf$visit)


ggplot(ddf) + 
  stat_smooth(aes(x = visit, y = fc, color=group, group=group), method = "lm", se = T, size=3, span=0.95, data=ddf) +stat_smooth(aes(x = visit, y = fc, color=group, group=ID), method = "lm", se = T, size=0.3, span=0.95, data=ddf) +
  coord_cartesian(ylim = c(0, 0.5)) +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=0, hjust=1))
  

ggplot(ddf, aes(x = visit, y = fc, color=group, group=ID)) +
  geom_line(aes(group = group)) +
  geom_line(data = d)

stat_smooth(aes(x = visit, y = fc, color=group, group=ID), method = "lm", se = T, size=0.3, span=0.95, data=ddf) 
  


# Change-change:

g = 'con'
n=2
si <- c(df$SI[df$visit==2 & df$group==g]-df$SI[df$visit==1 & df$group==g])
fc <- c(df[(df$visit==2 & df$group==g),n]-df[(df$visit==1 & df$group==g),n])
cor(fc, si)

# <<<<<<<<<<<<<<<
# <<< THE END <<<
# <<<<<<<<<<<<<<<