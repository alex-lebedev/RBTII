#######################
# Outlier detection ###
#######################

# Load libraries:
library(xlsx)

# Define working directory:
dir <- '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-11-29/summary/'

# Load source-file:
load(paste(dir, 'sourcedat.rda', sep=''))

# Remove subject "5037" who did not enter the study:
cogdat <- cogdat[!is.na(cogdat$group),]

cogdat$v1.NearUPD <- apply(cogdat[,c('v1.nearUpd.count.lvl2', 'v1.nearUpd.count.lvl4')],1, mean)
cogdat$v2.NearUPD <- apply(cogdat[,c('v2.nearUpd.count.lvl2', 'v2.nearUpd.count.lvl4')],1, mean)
cogdat$v1.TrainedUPD <- apply(cogdat[,c('v1.trainedUpd.count.lvl2', 'v1.trainedUpd.count.lvl4')],1, mean)
cogdat$v2.TrainedUPD <- apply(cogdat[,c('v2.trainedUpd.count.lvl2', 'v2.trainedUpd.count.lvl4')],1, mean)

v1_vars <- c(colnames(cogdat[,grep("v1.", colnames(cogdat))]),colnames(cogdat[,grep(".v1", colnames(cogdat))]))
v2_vars <- c(colnames(cogdat[,grep("v2.", colnames(cogdat))]),colnames(cogdat[,grep(".v2", colnames(cogdat))]))

cogdat.v1 <- cogdat[,c('ID','group',v1_vars)]
cogdat.v2 <- cogdat[,c('ID','group', v2_vars)]
cogdat.v1.con <- subset(cogdat.v1, cogdat.v1$group=='con')
cogdat.v1.act <- subset(cogdat.v1, cogdat.v1$group=='act')
cogdat.v2.con <- subset(cogdat.v2, cogdat.v2$group=='con')
cogdat.v2.act <- subset(cogdat.v2, cogdat.v2$group=='act')



# Clone source-files:
cogdat.v1_cleaned <- as.data.frame(cogdat.v1)
cogdat.v2.con_cleaned <- cogdat.v2.con
cogdat.v2.act_cleaned <- cogdat.v2.act

cogdat.diff_cleaned <- data.frame(ID=cogdat.v1[,'ID'],group=cogdat.v1[,'group'],cogdat.v2[,-c(1:2)]-cogdat.v1[,-c(1:2)])
cogdat.diff.con_cleaned <- subset(cogdat.diff_cleaned, cogdat.diff_cleaned$group=='con')
cogdat.diff.act_cleaned <- subset(cogdat.diff_cleaned, cogdat.diff_cleaned$group=='act')


# 3:

# Visit 1 (both groups):
outls <- matrix(F, dim(cogdat.v1_cleaned)[1], dim(cogdat.v1_cleaned)[2])
for (v in 3:dim(cogdat.v1)[2]){
  Q13 <- quantile(cogdat.v1[,v], c(.25, .75), na.rm=T) 
  Upper = Q13[2] + (3 * (Q13[2] - Q13[1]))
  Lower = Q13[1]- (3 * (Q13[2] - Q13[1]))
  c(Upper, Lower)
  outs <- which(cogdat.v1[,v]<Lower | cogdat.v1[,v]>Upper)
  if (length(outs)==0){
  } else {
    outls[outs,v] <- T
  }
}
table(outls)
which(outls == T, arr.ind=TRUE)
names(cogdat.v1_cleaned[which(outls == T, arr.ind=TRUE)[,2]])
cogdat.v1_cleaned[outls] <- NA 
((table(outls)[2]-2)/sum(table(outls)))*100
# without 2 bianry-variables (not used in the analysis), it's 1 data-entry: 
# ((table(outls)[2]-2)/sum(table(outls)))*100 = 0.04%


# VISIT 2 (separate groups):

# V2-Controls:

outls <- matrix(F, dim(cogdat.v2.con_cleaned)[1], dim(cogdat.v2.con_cleaned)[2])
for (v in 3:dim(cogdat.v2.con)[2]){
  Q13 <- quantile(cogdat.v2.con[,v], c(.25, .75), na.rm=T) 
  Upper = Q13[2] + (3 * (Q13[2] - Q13[1]))
  Lower = Q13[1]- (3 * (Q13[2] - Q13[1]))
  c(Upper, Lower)
  outs <- which(cogdat.v2.con[,v]<Lower | cogdat.v2.con[,v]>Upper)
  if (length(outs)==0){
  } else {
    outls[outs,v] <- T
  }
}
table(outls)
which(outls == T, arr.ind=TRUE)
names(cogdat.v2.con_cleaned[which(outls == T, arr.ind=TRUE)[,2]])
cogdat.v2.con_cleaned[outls] <- NA 
((table(outls)[2]-1)/sum(table(outls)))*100
# without 1 bianry-variable (not used in the analysis), it's 4 data-entries: 
# ((table(outls)[2]-1)/sum(table(outls)))*100 = 0.33%



# V2-Active:
outls <- matrix(F, dim(cogdat.v2.act_cleaned)[1], dim(cogdat.v2.act_cleaned)[2])
for (v in 3:dim(cogdat.v2.act)[2]){
  Q13 <- quantile(cogdat.v2.act[,v], c(.25, .75), na.rm=T) 
  Upper = Q13[2] + (3 * (Q13[2] - Q13[1]))
  Lower = Q13[1]- (3 * (Q13[2] - Q13[1]))
  c(Upper, Lower)
  outs <- which(cogdat.v2.act[,v]<Lower | cogdat.v2.act[,v]>Upper)
  if (length(outs)==0){
  } else {
    outls[outs,v] <- T
  }
}
table(outls)
which(outls == T, arr.ind=TRUE)
names(cogdat.v2.act_cleaned[which(outls == T, arr.ind=TRUE)[,2]])
cogdat.v2.act_cleaned[outls] <- NA 
((table(outls)[2])/sum(table(outls)))*100
# it's 2 data-entries: 
# ((table(outls)[2])/sum(table(outls)))*100 = 0.16%

# DIFFERENCE (separate groups):

# DIFF-Controls:
outls <- matrix(F, dim(cogdat.diff.con_cleaned)[1], dim(cogdat.diff.con_cleaned)[2])
for (v in 3:dim(cogdat.diff.con_cleaned)[2]){
  Q13 <- quantile(cogdat.diff.con_cleaned[,v], c(.25, .75), na.rm=T) 
  Upper = Q13[2] + (3 * (Q13[2] - Q13[1]))
  Lower = Q13[1]- (3 * (Q13[2] - Q13[1]))
  c(Upper, Lower)
  outs <- which(cogdat.diff.con_cleaned[,v]<Lower | cogdat.diff.con_cleaned[,v]>Upper)
  if (length(outs)==0){
  } else {
    outls[outs,v] <- T
  }
}
table(outls)
which(outls == T, arr.ind=TRUE)
names(cogdat.v2.con_cleaned[which(outls == T, arr.ind=TRUE)[,2]])

# Manual checks:
# Subjects [23, 'v2.vrec'] and [29,'near2back.OA.v2']:


cogdat.diff.con_cleaned$ID[c(23,29)] # subject IDs


# VREC:
# Distribution:
hist(cogdat.diff.con_cleaned[,'v2.vrec'])

# Check values:
cogdat.diff.con_cleaned[23,'v2.vrec'] # outlier diff value
cogdat.v1_cleaned[which(cogdat.v1_cleaned$ID==cogdat.diff.con_cleaned$ID[23]),'v1.vrec'] # outlier V1 value
cogdat.v2_cleaned[which(cogdat.v2_cleaned$ID==cogdat.diff.con_cleaned$ID[23]),'v2.vrec'] # outlier V2 value

# remove v1 value for "4148":
cogdat.v1_cleaned[which(cogdat.v1_cleaned$ID==cogdat.diff.con_cleaned$ID[23]),'v1.vrec'] <- NA


# near2back.OA.v2:

# Distributions:
hist(cogdat.diff.con_cleaned[,'near2back.OA.v2'])
hist(cogdat.v1_cleaned[,'near2back.OA.v1'])
hist(cogdat.v2.con_cleaned[,'near2back.OA.v2'])

# Check values:
cogdat.diff.con_cleaned[29,'near2back.OA.v2'] # outlier diff value

cogdat.v1_cleaned[which(cogdat.v1_cleaned$ID==cogdat.diff.con_cleaned$ID[29]),'near2back.OA.v1'] # outlier V1 value
cogdat.v2_cleaned[which(cogdat.v2_cleaned$ID==cogdat.diff.con_cleaned$ID[29]),'near2back.OA.v2'] # outlier V2 value

# remove v2 value for "5165":
cogdat.v2.con_cleaned[which(cogdat.v2.con_cleaned$ID==cogdat.diff.con_cleaned$ID[29]),'near2back.OA.v2'] <- NA


# DIFF-Act:
outls <- matrix(F, dim(cogdat.diff.act_cleaned)[1], dim(cogdat.diff.act_cleaned)[2])
for (v in 3:dim(cogdat.diff.act_cleaned)[2]){
  Q13 <- quantile(cogdat.diff.act_cleaned[,v], c(.25, .75), na.rm=T) 
  Upper = Q13[2] + (3 * (Q13[2] - Q13[1]))
  Lower = Q13[1]- (3 * (Q13[2] - Q13[1]))
  c(Upper, Lower)
  outs <- which(cogdat.diff.act_cleaned[,v]<Lower | cogdat.diff.act_cleaned[,v]>Upper)
  if (length(outs)==0){
  } else {
    outls[outs,v] <- T
  }
}
table(outls)
names(cogdat.v2.act_cleaned[which(outls == T, arr.ind=TRUE)[,2]])
which(outls == T, arr.ind=TRUE)
# *binary scores can be ignored (not used in the analysis)
# Outliers: Act-Subjects [8, 'v2.trainedUpd.count.lvl2'] and [25,'v2.speed.sw1']:

# IDs:
cogdat.diff.act_cleaned$ID[c(8,25)]

# MANUAL: v2.trainedUpd.count.lvl2:

# Distributions:
hist(cogdat.diff.act_cleaned[,'v2.trainedUpd.count.lvl2'])
hist(cogdat.v1_cleaned[,'v1.trainedUpd.count.lvl2'])
hist(cogdat.v2.act_cleaned[,'v2.trainedUpd.count.lvl2'])
# Check values:
cogdat.diff.act_cleaned[8,'v2.trainedUpd.count.lvl2'] # outlier diff value
cogdat.v1_cleaned[which(cogdat.v1_cleaned$ID==cogdat.diff.act_cleaned$ID[8]),'v1.trainedUpd.count.lvl2'] # outlier V1 value
cogdat.v2_cleaned[which(cogdat.v2_cleaned$ID==cogdat.diff.act_cleaned$ID[8]),'v2.trainedUpd.count.lvl2'] # outlier V2 value

# Remove values:
cogdat.v1_cleaned[which(cogdat.v1_cleaned$ID==cogdat.diff.act_cleaned$ID[8]),'v1.trainedUpd.count.lvl2'] <- NA
cogdat.v2.act_cleaned[which(cogdat.v2.act_cleaned$ID==cogdat.diff.act_cleaned$ID[8]),'v2.trainedUpd.count.lvl2'] <- NA

# MANUAL: v2.speed.sw1:
# Distributions:
hist(cogdat.diff.act_cleaned[,'v2.speed.sw1'])
hist(cogdat.v1_cleaned[,'v1.speed.sw1'])
hist(cogdat.v2.act_cleaned[,'v2.speed.sw1'])
# Check values:
cogdat.diff.act_cleaned[25,'v2.speed.sw1'] # outlier diff value
cogdat.v1_cleaned[which(cogdat.v1_cleaned$ID==cogdat.diff.act_cleaned$ID[25]),'v1.speed.sw1'] # outlier V1 value
cogdat.v2_cleaned[which(cogdat.v2_cleaned$ID==cogdat.diff.act_cleaned$ID[25]),'v2.speed.sw1'] # outlier V2 value

# Remove value at visit 2:
cogdat.v2.act_cleaned[which(cogdat.v2.act_cleaned$ID==cogdat.diff.act_cleaned$ID[25]),'v2.speed.sw1'] <- NA


# Merge data:
cogdat.v2.all_cleaned <- rbind(cogdat.v2.con_cleaned,cogdat.v2.act_cleaned)
cogdat.final_cleaned <- merge(cogdat.v1_cleaned, cogdat.v2.all_cleaned, by=c('ID','group'))


nam <- names(cogdat.final_cleaned)

for (i in 3:length(nam)){

print(cor(cogdat[,nam[i]],cogdat.final_cleaned[,nam[i]], use='complete'))
}


cogdat_cleaned <- cogdat.final_cleaned
# Normality testing:

normdist <- as.vector(rep(NA,dim(cogdat.v1_cleaned)[2]))
for (i in 3:dim(cogdat.v1_cleaned)[2]){
  v = i
  shapp <- shapiro.test(cogdat.v1_cleaned[,v])$p.value
  if (shapp>=0.01) {
    normdist[i] <- 1
  } else {
    normdist[i] <-0
  }
}

nonnormVars<- colnames(cogdat.v1_cleaned)[normdist==0][is.na(colnames(cogdat.v1_cleaned)[normdist==0])==F]

# The following part attempts to correct the distributions and is to be done manually:

v=1
nonnormVars[v]
var <- cogdat_cleaned[,nonnormVars[v]]
hist(var)
shapiro.test(var)

# Exclude irrelevant/ok variables
nonnormVars <- nonnormVars[-c(1, 2, 5,7)]



v=1
nonnormVars[v]
var <- cogdat_cleaned[,nonnormVars[v]]
hist((var))
shapiro.test((var))

# Transform:
cogdat_cleaned$v1.anl.transf.3root <- cogdat_cleaned$v1.anl^(1/3)
cogdat_cleaned$v2.anl.transf.3root <- cogdat_cleaned$v2.anl^(1/3)

cogdat_cleaned$v1.trainedUpd.count.lvl4.transf.cubed <- (cogdat_cleaned$v1.trainedUpd.count.lvl4)^3
cogdat_cleaned$v2.trainedUpd.count.lvl4.transf.cubed <- (cogdat_cleaned$v1.trainedUpd.count.lvl4)^3

cogdat_cleaned$v1.fl1.cost.3root <- cogdat_cleaned$v1.fl1.cost^(1/3)
cogdat_cleaned$v2.fl1.cost.3root <- cogdat_cleaned$v2.fl1.cost^(1/3)



# Still bad:
cogdat_cleaned$near2back.OA.v1.transf.cubed <- cogdat_cleaned$near2back.OA.v1^3
cogdat_cleaned$near2back.OA.v2.transf.cubed <- cogdat_cleaned$near2back.OA.v2^3
cogdat_cleaned$trained2back.OA.v1.transf.cubed <- cogdat_cleaned$trained2back.OA.v1^3
cogdat_cleaned$trained2back.OA.v2.transf.cubed <- cogdat_cleaned$trained2back.OA.v2^3
cogdat_cleaned$v1.nearUpd.count.lvl4.transf.log1p_cubed <- (log1p(cogdat_cleaned$v1.nearUpd.count.lvl4)^3)
cogdat_cleaned$v2.nearUpd.count.lvl4.transf.log1p_cubed <- (log1p(cogdat_cleaned$v2.nearUpd.count.lvl4)^3)


# save cleaned data:
save('cogdat_cleaned', 'persdat', file=paste(dir, 'cogdat_cleaned.rda', sep=''))

### END

# Calculate Mahalanobis with predictor variables

library(mice)

df2 <- cogdat_cleaned[, 3:dim(cogdat_cleaned)[2]]   
# DO NOT RUN
#df_tmp<- mice(df2,m=5,maxit=50,meth='pmm',seed=500)
#summary(df_tmp)
#save('df_tmp', file=paste(dir,'mice_outliers.rda', sep=''))

load(paste(dir,'mice_outliers.rda', sep=''))
df2comp <- complete(df_tmp)


ddf <- df2comp[,c('v1.nearUpd.count.lvl2', 'v1.nearUpd.count.lvl4', 'v2.nearUpd.count.lvl2', 'v2.nearUpd.count.lvl4',
                  'v1.trainedUpd.count.lvl2', 'v1.trainedUpd.count.lvl4', 'v2.trainedUpd.count.lvl2', 'v2.trainedUpd.count.lvl4',
                  'v1.rav', 'v2.rav', 'v1.beta', 'v2.beta', 'v1.wasi', 'v2.wasi',
                  'v1.anl', 'v2.anl', 'v1.syll', 'v2.syll', 'v1.vinf', 'v2.vinf',
                  'near2back.OA.v1', 'near2back.OA.v2', 'near3back.OA.v1', 'near3back.OA.v2',
                  'trained2back.OA.v1', 'trained2back.OA.v2', 'trained3back.OA.v1', 'trained3back.OA.v2',
                  'v1.nearRSW1.cost', 'v2.nearRSW1.cost', 'v1.nearRSW2.cost', 'v2.nearRSW2.cost',
                  'v1.nearTSW.lvl23.cost', 'v2.nearTSW.lvl23.cost', 'v1.nearTSW.lvl456.cost', 'v2.nearTSW.lvl456.cost', 'v1.nearTSW.lvl789.cost', 'v2.nearTSW.lvl789.cost',
                  'v1.trainedTSW.lvl23.cost', 'v2.trainedTSW.lvl23.cost', 'v1.trainedTSW.lvl456.cost', 'v2.trainedTSW.lvl456.cost', 'v1.trainedTSW.lvl789.cost', 'v2.trainedTSW.lvl789.cost'
)]

m_dist <- mahalanobis(ddf, colMeans(ddf), cov(ddf))
cogdat_cleaned$MD <- round(m_dist, 1)



cogdat_cleaned$MDoutlier <- "No"
#cogdat_cleaned$MDoutlier[cogdat_cleaned$MD > 999] <- "Yes"    # Threshold set to 50


pc12 <- prcomp(ddf,center=T)$x[,1:2]

plot(pc12)
text(pc12, labels=c(1:dim(pc12)[1]), cex= 0.7, offset = 10)
cogdat_cleaned$PCAoutlier <- 'No'

save('cogdat_cleaned', 'persdat', file=paste(dir, 'cogdat_cleaned.rda', sep=''))


save('cogdat_cleaned', 'persdat', file=paste(dir, 'cogdat_cleaned.rda', sep=''))