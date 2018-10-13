# Creativity pilot:
# Inter-rater reliability assessment
# 2017-05-03

library(xlsx)
library(ICC)


creRaw_r1 <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/cFl_F.xlsx',1)
creRaw_r2 <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/cFl_W.xlsx',1)


r1 <- creRaw_r1
r2 <- creRaw_r2

icc <- as.vector(rep(0,dim(r1)[2]))
names(icc) <- colnames(r1)

for (i in 1:length(icc)){
  d <- cbind(as.factor(c(r1$stuID,r2$stuID)), c(r1[,i],r2[,i]))
  icc[i] <- ICCest(d[,1],d[,2])$ICC
}

icc


cor(r1$CompTOT.v1,r2$CompTOT.v1)
cor(r1$CompTOT.v2,r2$CompTOT.v2)


plot(r1$CompTOT.v1,r2$CompTOT.v1, col='black', pch=16, cex=3)
points(r1$CompTOT.v2,r2$CompTOT.v2, col='darkgrey', pch=16, cex=3)




hist(r1$CompTOT.v1, col='black', xlim = c(1,3))
hist(r1$CompTOT.v2, col='darkgrey', xlim = c(1,3), add=T)



hist(r2$CompTOT.v1, col='black', xlim = c(1,3))
hist(r2$CompTOT.v2, col='darkgrey', xlim = c(1,3), add=T)

# Cross-correlation:

fl_v1 <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/cFl_final.xlsx',1)$CompTOT.v1
fl_v2 <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/cFl_final.xlsx',1)$CompTOT.v2
fx_v1 <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/cFx_final.xlsx',1)$CompTOT.v1
fx_v2 <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/cFx_final.xlsx',1)$CompTOT.v2
o_v1 <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/cO_final.xlsx',1)$CompTOT.v1
o_v2 <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/creativity/output/cO_final.xlsx',1)$CompTOT.v2

v1 <- cbind(fl_v1,fx_v1, o_v1)
v2 <- cbind(fl_v2,fx_v2, o_v2)
colnames(v1) <- c('Fluency','Flexibility', 'Originality')
colnames(v1) <- colnames(v2)

### SUPPLEMENT



# Highest disagreement:
dis <- names(which(icc<0.7))


disagr <- cbind(creRaw_r1$StudyID, creRaw_r1$RespN,
        sqrt((creRaw_r1[,dis[1]]-creRaw_r2[,dis[1]])^2),
        sqrt((creRaw_r1[,dis[2]]-creRaw_r2[,dis[2]])^2),
        sqrt((creRaw_r1[,dis[3]]-creRaw_r2[,dis[3]])^2))


colnames(disagr) <- c('StudyID', 'RespN', paste(dis, '_DisValue', sep=''))

write.xlsx(disagr, '/Users/alebedev/Documents/R/REBOOT2/creativity_reliability_pilot/disagreement.xlsx')

mean(icc[str_detect(names(icc), ".FLU")])
mean(icc[str_detect(names(icc), ".FLEX")])
