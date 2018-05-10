
# Load libraries:
library(lavaan)
library(xlsx)
library(ggplot2)
library(R.matlab)

voi <- readMat('/Volumes/REBOOTII/RBTII/ANALYSIS/Nback_fmriprep/3back/VOI_VOI.mat')
spmat <- readMat('/Volumes/REBOOTII/RBTII/ANALYSIS/Nback_fmriprep/3back/SPM.mat')


strsplit(strsplit(as.character(vv[1]), split = '/Volumes/REBOOTII/RBTII/ANALYSIS/Nback_fmriprep/V[1-2]/')[[1]][2], split='/con_0002')[[1]][1]

ids <- sub('sub-*','',dir('/Volumes/REBOOTII/RBTII/PROCESSED/DPABI_rest/fmriprep_Glob/FunImgPW'))

d <- data.frame(group=c(rep('con',28), rep('act', 29)),voi=voi$Y, visit=c(rep('V1',length(ids)),rep('V2',length(ids))))

vv <- as.vector(spmat$SPM[[1]][[1]])

for (i in 1:length(vv)[1]){
  d$ID[i] <- strsplit(strsplit(as.character(vv[i]),
                               split = '/Volumes/REBOOTII/RBTII/ANALYSIS/Nback_fmriprep/V[1-2]/sub-')[[1]][2],
                                split='/con_0002')[[1]][1]
}


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


df <- merge(dat, d, by=c('ID', 'group')) 
df[which(df$visit=='V1'), 'SR'] <- df$SI1[which(df$visit=='V1')]
df[which(df$visit=='V2'), 'SR'] <- df$SI2[which(df$visit=='V2')]


summary(glm(voi~group*visit*SR, data=df))


# Compelte resting state and structural analyses, then:
ddd <- merge(ddd_tot, df, by=c('ID', 'visit', 'group'))
ddd <- merge(ddd, fcdat, by=c('ID', 'visit'))

# In-scanner n-back performance:
load('/Volumes/REBOOTII/RBTII/nback_scanner-2017-11-25.rda')

#nb <- subset(nback_scanner[,c('ID', 'group', 'visit', 'perf')], nback_scanner$load==3)
tmp <- apply(cbind(nback_scanner$perf[nback_scanner$load==1],nback_scanner$perf[nback_scanner$load==2],
            nback_scanner$perf[nback_scanner$load==3]),1,mean)
nb$perf <- tmp

ddd <- merge(ddd, nb, by=c('ID', 'visit', 'group'))


cor(ddd[,c('SR.x', 'voi', 'SNvol', '171-68', '74-69', '187-95', 'perf')])


dd <- data.frame(SR = ddd$SR.y[ddd$visit=='V2']-ddd$SR.y[ddd$visit=='V1'],
                 nb = ddd$perf[ddd$visit=='V2']-ddd$perf[ddd$visit=='V1'],
                 tTSW = ddd$tTSW[ddd$visit=='V2']-ddd$tTSW[ddd$visit=='V1'],
                 voi = ddd$voi[ddd$visit=='V2']-ddd$voi[ddd$visit=='V1'],
                 c171_68 = ddd$`171-68`[ddd$visit=='V2']-ddd$`171-68`[ddd$visit=='V1'],
                 c74_69 = ddd$`74-69`[ddd$visit=='V2']-ddd$`74-69`[ddd$visit=='V1'],
                 c187_95 = ddd$`187-95`[ddd$visit=='V2']-ddd$`187-95`[ddd$visit=='V1'],
                 SNvol = ddd$`SNvol`[ddd$visit=='V2']-ddd$`SNvol`[ddd$visit=='V1'])
                 
# OR:

dd <- data.frame(SR = ddd$SR.y[ddd$visit=='V2']-ddd$SR.y[ddd$visit=='V1'],
                                   nb = ddd$perf[ddd$visit=='V2']-ddd$perf[ddd$visit=='V1'],
                                   tTSW = ddd$tTSW[ddd$visit=='V2']-ddd$tTSW[ddd$visit=='V1'],
                                   voi = ddd$voi[ddd$visit=='V2']-ddd$voi[ddd$visit=='V1'],
                                   c39_15 = ddd$`39-15`[ddd$visit=='V2']-ddd$`39-15`[ddd$visit=='V1'],
                                   c68_50 = ddd$`68-50`[ddd$visit=='V2']-ddd$`68-50`[ddd$visit=='V1'],
                                  c182_56 = ddd$`182-56`[ddd$visit=='V2']-ddd$`182-56`[ddd$visit=='V1'],
                                  c187_95 = ddd$`187-95`[ddd$visit=='V2']-ddd$`187-95`[ddd$visit=='V1'],
                                  c187_73 = ddd$`187-73`[ddd$visit=='V2']-ddd$`187-73`[ddd$visit=='V1'],
                                  c196_135 = ddd$`196-135`[ddd$visit=='V2']-ddd$`196-135`[ddd$visit=='V1'])
dd$group<-ddd$group[ddd$visit=='V1']


cor(dd[,1:7])


# Plot:
var1 = 'SR'
var2 = 'SNvol'

cor(dd[,c(var1, var2)])[1,2]; summary(glm(dd[,var1]~dd[,var2]))
cor(dd[dd$group=='con',c(var1, var2)])[1,2]; summary(glm(dd[dd$group=='con',var1]~dd[dd$group=='con',var2]))
cor(dd[dd$group=='act',c(var1, var2)])[1,2]; summary(glm(dd[dd$group=='act',var1]~dd[dd$group=='act',var2]));
plot(dd[,var2],dd[,var1], col=color[1], pch=17, cex=2.5, type = 'n')
points(dd[dd$group=='con',var2],dd[dd$group=='con',var1], col=color[1], pch=17, cex=2.5, add=T)
abline(summary(glm(dd[dd$group=='con',var1]~dd[dd$group=='con',var2])),col=color[1], lwd=8, add=T)
points(dd[dd$group=='act',var2],dd[dd$group=='act',var1], col=color[2], pch=16, cex=2.5, add=T)
abline(summary(glm(dd[dd$group=='act',var1]~dd[dd$group=='act',var2])),col=color[2], lwd=8, add=T)
abline(summary(glm(dd[,var1]~dd[,var2])),col='darkgrey', lwd=8, lty=3, add=T)


# Pre-post spagghetti plot:

ddf <- df[,c('ID','visit','group', '171-68')]
colnames(ddf)[4] <- 'voi' 
ddf$visit <- as.factor(ddf$visit)


ggplot(ddf) + 
  stat_smooth(aes(x = visit, y = voi, color=group, group=group), method = "lm", se = T, size=3, span=0.95, data=ddf) +stat_smooth(aes(x = visit, y = voi, color=group, group=ID), method = "lm", se = T, size=0.3, span=0.95, data=ddf) +
  coord_cartesian(ylim = c(-0.35, 0.65)) +
  theme(text = element_text(size=30),
        axis.text.x = element_text(angle=0, hjust=1))


