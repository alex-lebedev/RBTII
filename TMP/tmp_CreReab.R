# Creativity pilot:
# Inter-rater reliability assessment
# 2017-05-03

library(xlsx)
library(ICC)


creRaw <- read.xlsx('/Users/alebedev/Documents/R/REBOOT2/creativity_reliability_pilot/ratings.xlsx',1)


creRaw_r1 <- subset(creRaw, creRaw$Rater=='william')
creRaw_r2 <- subset(creRaw, creRaw$Rater=='filip')
r1 <- aggregate(creRaw_r1[,5:34], list(creRaw_r1$StudyID), mean)
r2 <- aggregate(creRaw_r2[,5:34], list(creRaw_r2$StudyID), mean)

icc <- as.vector(rep(0,dim(r1)[2]))
names(icc) <- colnames(r1)

for (i in 1:length(icc)){
  d <- cbind(as.factor(c(r1$Group.1,r2$Group.1)), c(r1[,i],r2[,i]))
  icc[i] <- ICCest(d[,1],d[,2])$ICC
}

icc <- icc[-c(1,6)]


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
