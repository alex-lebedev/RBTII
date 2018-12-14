# >>>>>>>>>>>>>>>>>>>>>
# >>> TMP_Tracula.R >>>
# >>>>>>>>>>>>>>>>>>>>>


# Authors: Alexander V. Lebedev & Martin Lovden
# Date: 2018-04-20
# Study: REBOOT-II (OSF: https://osf.io/aam9u/)

'
Analysis of the diffusion data from TRACULA.
'

# TRACULA

library(xlsx)

tracts <- dir('/Volumes/REBOOTII/RBTII/PROCESSED/trac_summary')


tmp <- data.frame(matrix(NA, length( read.csv(paste('/Volumes/REBOOTII/RBTII/PROCESSED/trac_summary/',tracts[1], sep=''), sep='\t')[,1]),
                             2))

tmp[,1] <- read.csv(paste('/Volumes/REBOOTII/RBTII/PROCESSED/trac_summary/',tracts[1], sep=''), sep='\t')[,1]
tmp[,2] <- read.csv(paste('/Volumes/REBOOTII/RBTII/PROCESSED/trac_summary/',tracts[1], sep=''), sep='\t')$FA_Avg
colnames(tmp) <- c('sID', tracts[1])   



i=2
while (i<=length(tracts))
{
  tmp1 <- data.frame(matrix(NA, length( read.csv(paste('/Volumes/REBOOTII/RBTII/PROCESSED/trac_summary/',tracts[i], sep=''), sep='\t')[,1]),
                           2))
  tmp1[,1] <- read.csv(paste('/Volumes/REBOOTII/RBTII/PROCESSED/trac_summary/',tracts[i], sep=''), sep='\t')[,1]
  tmp1[,2] <- read.csv(paste('/Volumes/REBOOTII/RBTII/PROCESSED/trac_summary/',tracts[i], sep=''), sep='\t')$FA_Avg
  colnames(tmp1) <- c('sID', tracts[i])   
  tmp <- merge(tmp, tmp1, by='sID', all=T)
  i=i+1
}

colnames(tmp) <- strsplit(colnames(tmp),'_avg33_mni_bbr.txt')
dtrac <- tmp
dtrac$ID <- substr(dtrac$sID, 5, 8)
dtrac$visit <- substr(dtrac$sID, 10, 10)

dtrac$visit <- as.factor(paste('V',dtrac$visit, sep=''))
dat <- merge(dtrac, ddd_tot, by=c('ID', 'visit')) 


#i=3;
#colnames(dat)[i]; t.test(dat[dat$visit==1 & dat$group == 'act',i],dat[dat$visit==2& dat$group == 'act',i], paired=T)
#summary(glm(dat[,i]~group*visit, data=dat))







i=17; colnames(dat)[i];


v1 <- subset(dat, visit=='V1');v2 <- subset(dat, visit=='V2');

dcor <- cbind(v1[,i],v2[,i],v2[,i]-v1[,i],v1$SR,v2$SR,v2$SR-v1$SR)
colnames(dcor) <- c(paste(colnames(dat)[i],'1'), paste(colnames(dat)[i],'2'),paste(colnames(dat)[i],'diff'),
                    'SR_1', 'SR_2', 'SR_diff')

cor(dcor[complete.cases(dcor),])[3,6]

# <<<<<<<<<<<<<<<
# <<< THE END <<<
# <<<<<<<<<<<<<<<

