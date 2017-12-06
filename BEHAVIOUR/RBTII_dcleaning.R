#######################
# Outlier detection ###
#######################

# Load libraries:
library(xlsx)

dir <- '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-11-29/summary/'

load(paste(dir, 'sourcedat.rda', sep=''))
cogdat_cleaned <- cogdat


# 2.2:
cogdat_cleaned <- cogdat
outls <- matrix(F, dim(cogdat)[1], dim(cogdat)[2])



for (v in 10:dim(cogdat)[2]){
  
  Q13 <- quantile(cogdat[,v], c(.25, .75), na.rm=T) 
  Upper = Q13[2] + (2.2 * (Q13[2] - Q13[1]))
  Lower = Q13[1]- (2.2 * (Q13[2] - Q13[1]))
  c(Upper, Lower)
  outs <- which(cogdat[,v]<Lower | cogdat[,v]>Upper)
  if (length(outs)==0){
  } else {
    outls[outs,v] <- T
  }
}
cogdat_cleaned[outls] <- NA 

normdist <- as.vector(rep(NA,dim(cogdat_cleaned)[2]))
for (i in 10:dim(cogdat_cleaned)[2]){
  v = i
  shapp <- shapiro.test(cogdat_cleaned[,v])$p.value
  if (shapp>=0.01) {
    normdist[i] <- 1
  } else {
    normdist[i] <-0
  }
}



nonnormVars<- colnames(cogdat_cleaned)[normdist==0][is.na(colnames(cogdat_cleaned)[normdist==0])==F]

# The following part attempts to correct the distributions and is to be done manually:

v=1
nonnormVars[v]
var <- cogdat_cleaned[,nonnormVars[v]]
hist(var)
shapiro.test(var)

# MB OK: ,11, 17
# Consider excl 18
nonnormVars <- nonnormVars[-c(1, 2, 9,11,14, 16, 17,18,19)]



v=1
nonnormVars[v]
var <- cogdat_cleaned[,nonnormVars[v]]
hist((var))
shapiro.test((var))

cogdat_cleaned$v1.anl.transf.3root <- cogdat_cleaned$v1.anl^(1/3)
cogdat_cleaned$v2.anl.transf.3root <- cogdat_cleaned$v2.anl^(1/3)

cogdat_cleaned$v1.nearUpd.count.lvl4.transf.log1p_cubed <- (log1p(cogdat_cleaned$v1.nearUpd.count.lvl4)^3)
cogdat_cleaned$v2.nearUpd.count.lvl4.transf.log1p_cubed <- (log1p(cogdat_cleaned$v2.nearUpd.count.lvl4)^3)

cogdat_cleaned$v1.trainedUpd.count.lvl4.transf.cubed <- (cogdat_cleaned$v1.trainedUpd.count.lvl4)^3
cogdat_cleaned$v2.trainedUpd.count.lvl4.transf.cubed <- (cogdat_cleaned$v1.trainedUpd.count.lvl4)^3

cogdat_cleaned$v1.fl1.cost.3root <- cogdat_cleaned$v1.fl1.cost^(1/3)
cogdat_cleaned$v2.fl1.cost.3root <- cogdat_cleaned$v2.fl1.cost^(1/3)



# Still bad:
cogdat_cleaned$near2back.OA.v1.transf.cubed <- cogdat_cleaned$near2back.OA.v1^3
cogdat_cleaned$near2back.OA.v2.transf.cubed <- cogdat_cleaned$near2back.OA.v2^3
cogdat_cleaned$trained2back.OA.v1.transf.cubed <- cogdat_cleaned$trained2back.OA.v1^3
cogdat_cleaned$trained2back.OA.v2.transf.cubed <- cogdat_cleaned$trained2back.OA.v2^3




vars <- colnames(cogdat_cleaned[,grep("v1.", colnames(cogdat_cleaned))])[1:31]


diff_df <- as.data.frame(matrix(NA, dim(cogdat_cleaned)[1], length(vars)))
diff_df$ID <- cogdat_cleaned$ID



vnam <- vars
for (i in 1:length(vars)){
  vnam[i] <- strsplit(vars, 'v1.')[[i]][2]
}
colnames(diff_df) <- c(paste(vnam,'.diff', sep=''), 'ID')


for (i in 1:length(vars)){
  v <- vnam[i]
  diff_df[,paste(v,'.diff',sep='')] <- cogdat_cleaned[,paste('v2.',v,sep='')]-cogdat_cleaned[,paste('v1.',v,sep='')]
}
####

outls_diff <- matrix(F, dim(diff_df)[1], dim(diff_df)[2])

for (v in 1:c(dim(diff_df)[2]-1)){
  Q13 <- quantile(diff_df[,v], c(.25, .75), na.rm=T) 
  Upper = Q13[2] + (2.2 * (Q13[2] - Q13[1]))
  Lower = Q13[1]- (2.2 * (Q13[2] - Q13[1]))
  c(Upper, Lower)
  outs <- which(diff_df[,v]<Lower | diff_df[,v]>Upper)
  if (length(outs)==0){
  } else {
    outls_diff[outs,v] <- T
  }
}

for (i in 1:dim(outls_diff)[1]){
  print(paste('subject',diff_df$ID[i],'has', sum(outls_diff[i,1:31]), 'outlier values'))
}




#cogdat_cleaned[outls] <- NA 

### END





save('cogdat_cleaned', 'persdat', file=paste(dir, 'cogdat_cleaned.rda', sep=''))




# Calculate Mahalanobis with predictor variables

library(mice)

df2 <- cogdat_cleaned[, 10:83]   
# DO NOT RUN
#df_tmp<- mice(df2,m=5,maxit=50,meth='pmm',seed=500)
#summary(df_tmp)
# save('df_tmp', file=paste(dir,'mice_outliers.rda', sep=''))

load(paste(dir,'mice_outliers.rda', sep=''))
df2comp <- complete(df_tmp)


ddf <- df2comp[,c('v1.nearUpd.count.lvl2', 'v1.nearUpd.count.lvl4', 'v2.nearUpd.count.lvl2', 'v2.nearUpd.count.lvl4',
                  'v1.trainedUpd.count.lvl2', 'v1.trainedUpd.count.lvl4', 'v2.trainedUpd.count.lvl2', 'v2.trainedUpd.count.lvl4',
                  'v1.rav', 'v2.rav', 'v1.beta', 'v2.beta', 'v1.wasi', 'v2.wasi',
                  'v1.anl', 'v2.anl', 'v1.syll', 'v2.syll', 'v1.vinf', 'v2.vinf',
                  'near2back.OA.v1', 'near2back.OA.v2', 'near3back.OA.v1', 'near3back.OA.v2',
                  'trained2back.OA.v1', 'trained2back.OA.v2', 'trained3back.OA.v1', 'trained3back.OA.v2',
                  'v1.nearRSW1.cost', 'v2.nearRSW1.cost', 'v1.nearRSW2.cost', 'v2.nearRSW2.cost', 'v1.nearTSW.cost', 'v2.nearTSW.cost',
                  'v1.trainedTSW.lvl23.cost', 'v2.trainedTSW.lvl23.cost', 'v1.trainedTSW.lvl456.cost', 'v2.trainedTSW.lvl456.cost', 'v1.trainedTSW.lvl789.cost', 'v2.trainedTSW.lvl789.cost'
)]

m_dist <- mahalanobis(ddf, colMeans(ddf), cov(ddf))
cogdat_cleaned$MD <- round(m_dist, 1)

cogdat_cleaned$MDoutlier <- "No"
cogdat_cleaned$MDoutlier[cogdat_cleaned$MD > 55] <- "Yes"    # Threshold set to 50


pc12 <- prcomp(ddf,center=T)$x[,1:2]

plot(pc12)
text(pc12, labels=c(1:dim(pc12)[1]), cex= 0.7, offset = 10)

cogdat_cleaned$PCAoutlier <- 'No'
cogdat_cleaned$PCAoutlier[52] <- 'Possible'



save('cogdat_cleaned', 'persdat', file=paste(dir, 'cogdat_cleaned.rda', sep=''))