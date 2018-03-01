
library(lavaan)
library(xlsx)
library(ggplot2)
library(R.matlab)


ids <- sub('sub-*','',dir('/Volumes/REBOOTII/RBTII/PROCESSED/DPABI_rest/S2_FunImgARCWSDF'))

gr <- rep(NA, length(ids))

load('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-11-29/summary/cogdat_cleaned.rda')

gr[is.element(ids,subset(cogdat_cleaned$ID, cogdat_cleaned$group=='con'))] <- 0
gr[is.element(ids,subset(cogdat_cleaned$ID, cogdat_cleaned$group=='act'))] <- 1

coord <- read.table('/Volumes/REBOOTII/NBS/PrePost_ch/results/map/RBT1.node')[,1:3]
rownames(coord)<-c()
colnames(coord)<-c()

write.table(coord, file='/Volumes/REBOOTII/RBTII/PROCESSED/NBS/CraddockCoord.txt',quote = F,col.names = F, row.names = F)



M <- readMat('~/Desktop/NBS/M_rest.mat')
S<-readMat('/Volumes/REBOOTII/RBTII/PROCESSED/NBS/str_rest_fmriprep.mat')

p <- 1:200
roi=37;
for (n in 1:200){
    dat <- data.frame(fc=M$M[roi,n,], gr=as.factor(rep(gr,2)), visit=as.factor(c(rep(1,dim(M$M)[3]/2),rep(2,dim(M$M)[3]/2))))
    p[n] <- summary(glm(fc~gr*visit, data=dat))$coeff[4,4]
  }
which(p<0.05)


p <- 1:200
for (n in 1:200){
  dat <- data.frame(str=c(S$str1[,n],S$str2[,n]), gr=as.factor(rep(gr,2)), visit=as.factor(c(rep(1,dim(M$M)[3]/2),rep(2,dim(M$M)[3]/2))))
  p[n] <- summary(glm(str~gr*visit, data=dat))$coeff[4,4]
}
which(p<0.05)



