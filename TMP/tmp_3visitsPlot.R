
library(ggplot2)

load('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-11-29/summary/cogdat_cleaned.rda')
load('/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-12-18-FollowUp/summary/cogdat_cleaned_fu.rda')

dat <- merge(cogdat_cleaned, cogdat_cleaned_fu, by=c('ID', 'group'))
  
  
var1 <- 'v1.rav.x'
var2 <- 'v2.rav.x'
var3 <- 'v1.rav.y'


dat_long <- data.frame(ID=as.factor(rep(dat$ID, 3)),
                       G=as.factor(rep(dat$group, 3)),
                       visit=as.factor(c(rep('V1',dim(dat)[1]),rep('V2',dim(dat)[1]),rep('V3',dim(dat)[1]))),
                       beh=c(dat[,var1],dat[,var2],dat[,var3]))
p <- ggplot(data = dat_long, aes(x = visit, y = beh, group = ID))
p + geom_line(aes(color= G)) + stat_smooth(aes(group = G)) + stat_summary(aes(group = G, color=G, shape = G), 
                                                                          geom = "point", fun.y = mean, size = 5)

