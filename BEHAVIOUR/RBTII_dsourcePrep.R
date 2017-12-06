# Produce Source File:

# Load libraries:
library(xlsx)

#excluded <- c('3035', '5037')
# Define working directory:
dir <- '/Users/alebedev/Documents/R/REBOOT2/BEHAVIOR/2017-11-29/summary/'


# Load demographics data:
load(paste(dir, 'demogr.rda', sep=''))


# Start:

# I. INTELLIGENCE:

# Ia. Spatial Intelligence:
rav <- read.xlsx2(paste(dir, '', sep='transfer_raven.xlsx'),1)[,c('ID','v1.Score','v2.Score')]
beta <- read.xlsx2(paste(dir, '', sep='transfer_beta.xlsx'),1)[,c('ID','v1.Acc','v2.Acc')]
wasi <- read.xlsx2(paste(dir, '', sep='transfer_wasi.xlsx'),1)[,c('ID','v1.Acc','v2.Acc')]

colnames(rav) <- c('ID','v1.rav', 'v2.rav')
colnames(beta) <- c('ID','v1.beta', 'v2.beta')
colnames(wasi) <- c('ID','v1.wasi', 'v2.wasi')

cogdat <- merge(demogr, rav, by='ID', all=T)
cogdat <- merge(cogdat, beta, by='ID', all=T)
cogdat <- merge(cogdat, wasi, by='ID', all=T)

# Ib. Verbal Intelligence:
vinf <- read.xlsx2(paste(dir, '', sep='transfer_verbal_inference.xlsx'),1)[,c('ID','v1.Acc','v2.Acc')]
wcomp <- read.xlsx2(paste(dir, '', sep='transfer_word_comprehension.xlsx'),1)[,c('ID','v1.Acc','v2.Acc')]
syll <- read.xlsx2(paste(dir, '', sep='transfer_syllogism.xlsx'),1)[,c('ID','v1.Acc','v2.Acc')]
anl <- read.xlsx2(paste(dir, '', sep='transfer_analogies.xlsx'),1)[,c('ID','v1.Acc','v2.Acc')]


colnames(vinf) <- c('ID','v1.vinf', 'v2.vinf')
colnames(wcomp) <- c('ID','v1.wcomp', 'v2.wcomp')
colnames(syll) <- c('ID','v1.syll', 'v2.syll')
colnames(anl) <- c('ID','v1.anl', 'v2.anl')

cogdat <- merge(cogdat, vinf, by='ID', all=T)
cogdat <- merge(cogdat, wcomp, by='ID', all=T)
cogdat <- merge(cogdat, syll, by='ID', all=T)
cogdat <- merge(cogdat, anl, by='ID', all=T)

######################
# II. WORKING MEMORY #
######################

# IIa. Updating:
n2b <- read.xlsx2(paste(dir, '', sep='transfer_near_2back.xlsx'),1)[,c('NB2.ID','NB2.OA.v1','NB2.OA.v2')]
n3b <- read.xlsx2(paste(dir, '', sep='transfer_near_3back.xlsx'),1)[,c('NB3.ID','NB3.OA.v1','NB3.OA.v2')]

t2b <- read.xlsx2(paste(dir, '', sep='transfer_trained_2back.xlsx'),1)[,c('tnb2.ID','tnb2.OA.v1','tnb2.OA.v2')]
t3b <- read.xlsx2(paste(dir, '', sep='transfer_trained_3back.xlsx'),1)[,c('tnb3.ID','tnb3.OA.v1','tnb3.OA.v2')]

nupd <- read.xlsx2(paste(dir, '', sep='transfer_near_upd.xlsx'),1)[,c('ID','v1.CorrCount.lvl2','v1.CorrYN.lvl2',
                                                                      'v1.CorrCount.lvl4', 'v1.CorrYN.lvl4',
                                                                      'v2.CorrCount.lvl2','v2.CorrYN.lvl2',
                                                                      'v2.CorrCount.lvl4', 'v2.CorrYN.lvl4')]
supd <- read.xlsx2(paste(dir, '', sep='transfer_spatial_updating.xlsx'),1)[,c('ID','v1.CorrCount','v1.CorrYN',
                                                                      'v2.CorrCount','v2.CorrYN')]
tupd <- read.xlsx2(paste(dir, '', sep='transfer_trained_upd.xlsx'),1)[,c('ID','v1.CorrCount.lvl2','v1.CorrYN.lvl2',
                                                                      'v1.CorrCount.lvl4', 'v1.CorrYN.lvl4',
                                                                      'v2.CorrCount.lvl2','v2.CorrYN.lvl2',
                                                                      'v2.CorrCount.lvl4', 'v2.CorrYN.lvl4')]

colnames(n2b) <- c('ID', 'near2back.OA.v1','near2back.OA.v2')
colnames(n3b) <- c('ID', 'near3back.OA.v1','near3back.OA.v2')
colnames(t2b) <- c('ID', 'trained2back.OA.v1','trained2back.OA.v2')
colnames(t3b) <- c('ID', 'trained3back.OA.v1','trained3back.OA.v2')
colnames(nupd) <- c('ID','v1.nearUpd.count.lvl2','v1.nearUpd.binary.lvl2',
                    'v1.nearUpd.count.lvl4','v1.nearUpd.binary.lvl4',
                    'v2.nearUpd.count.lvl2','v2.nearUpd.binary.lvl2',
                    'v2.nearUpd.count.lvl4','v2.nearUpd.binary.lvl4')
colnames(tupd) <- c('ID','v1.trainedUpd.count.lvl2','v1.trainedUpd.binary.lvl2',
                    'v1.trainedUpd.count.lvl4','v1.trainedUpd.binary.lvl4',
                    'v2.trainedUpd.count.lvl2','v2.trainedUpd.binary.lvl2',
                    'v2.trainedUpd.count.lvl4','v2.trainedUpd.binary.lvl4')
colnames(supd) <- c('ID','v1.trainedSupd.count','v1.trainedSupd.binary',
                    'v2.trainedSupd.count','v2.trainedSupd.binary')

cogdat <- merge(cogdat, n2b, by='ID', all=T)
cogdat <- merge(cogdat, n3b, by='ID', all=T)
cogdat <- merge(cogdat, t2b, by='ID', all=T)
cogdat <- merge(cogdat, t3b, by='ID', all=T)
cogdat <- merge(cogdat, nupd, by='ID', all=T)
cogdat <- merge(cogdat, tupd, by='ID', all=T)
cogdat <- merge(cogdat, supd, by='ID', all=T)

# IIb. Switching 

rsw1 <- read.xlsx2(paste(dir, '', sep='transfer_near_ruleswitching.xlsx'),1)[,c('ID','v1.Acc.sw','v1.RT.sw','v1.RT.ps','v1.RT.ns',
                                                                                'v2.Acc.sw','v2.RT.sw','v2.RT.ps','v2.RT.ns')]
rsw2 <- read.xlsx2(paste(dir, '', sep='transfer_trained_ruleswitching_trial.xlsx'),1)[,c('ID','v1.Acc.sw','v1.RT.sw','v1.RT.ps','v1.RT.ns',
                                                                                         'v2.Acc.sw','v2.RT.sw','v2.RT.ps','v2.RT.ns')]
tsw1 <- read.xlsx2(paste(dir, '', sep='transfer_near_taskswitching.xlsx'),1)[,c('ID','v1.nearTSW.lvl23.cost', 'v2.nearTSW.lvl23.cost',
                                                                                'v1.nearTSW.lvl456.cost', 'v2.nearTSW.lvl456.cost',
                                                                                'v1.nearTSW.lvl789.cost', 'v2.nearTSW.lvl789.cost')]
tsw2 <- read.xlsx2(paste(dir, '', sep='transfer_trained_taskswitching.xlsx'),1)[,c('ID','v1.trainedTSW.lvl23.cost', 'v2.trainedTSW.lvl23.cost',
                                                                                   'v1.trainedTSW.lvl456.cost', 'v2.trainedTSW.lvl456.cost',
                                                                                   'v1.trainedTSW.lvl789.cost', 'v2.trainedTSW.lvl789.cost')]
sfl <- read.xlsx2(paste(dir, '', sep='transfer_spatial_flanker.xlsx'),1)[,c('ID','v1.Acc.sw1','v1.RT.sw0','v1.RT.sw1',
                                                                                'v2.Acc.sw1','v2.RT.sw0','v2.RT.sw1')]
nfl <- read.xlsx2(paste(dir, '', sep='transfer_numeric_flanker.xlsx'),1)[,c('ID','v1.Acc.sw1','v1.RT.sw0','v1.RT.sw1',
                                                                            'v2.Acc.sw1','v2.RT.sw0','v2.RT.sw1')]

rsw1[,-1] <- apply(rsw1[,-1],2,as.numeric)
rsw2[,-1] <- apply(rsw2[,-1],2,as.numeric)
tsw1[,-1] <- apply(tsw1[,-1],2,as.numeric)
tsw2[,-1] <- apply(tsw2[,-1],2,as.numeric)
sfl[,-1] <- apply(sfl[,-1],2,as.numeric)
nfl[,-1] <- apply(nfl[,-1],2,as.numeric)

# Calculate ruleswtich-cost:
rsw1$v1.nearRSW1.cost <- c(rsw1$v1.RT.ps-rsw1$v1.RT.ns)
rsw1$v2.nearRSW1.cost <- c(rsw1$v2.RT.ps-rsw1$v2.RT.ns)
rsw2$v1.nearRSW2.cost <- c(rsw2$v1.RT.ps-rsw2$v1.RT.ns)
rsw2$v2.nearRSW2.cost <- c(rsw2$v2.RT.ps-rsw2$v2.RT.ns)

sfl$v1.fl1.cost <- c(sfl$v1.RT.sw1-sfl$v1.RT.sw0)
sfl$v2.fl1.cost <- c(sfl$v2.RT.sw1-sfl$v2.RT.sw0)
nfl$v1.fl2.cost <- c(nfl$v1.RT.sw1-nfl$v1.RT.sw0)
nfl$v2.fl2.cost <- c(nfl$v2.RT.sw1-nfl$v2.RT.sw0)

rsw1 <- rsw1[,c('ID', 'v1.nearRSW1.cost', 'v2.nearRSW1.cost')]
rsw2 <- rsw2[,c('ID', 'v1.nearRSW2.cost', 'v2.nearRSW2.cost')]
sfl <- sfl[,c('ID', 'v1.fl1.cost', 'v2.fl1.cost')]
nfl <- nfl[,c('ID', 'v1.fl2.cost', 'v2.fl2.cost')]

cogdat <- merge(cogdat, rsw1, by='ID', all=T)
cogdat <- merge(cogdat, rsw2, by='ID', all=T)
cogdat <- merge(cogdat, tsw1, by='ID', all=T)
cogdat <- merge(cogdat, tsw2, by='ID', all=T)
cogdat <- merge(cogdat, sfl, by='ID', all=T)
cogdat <- merge(cogdat, nfl, by='ID', all=T)

##################
### III. SPEED ###
##################

speed <- read.xlsx2(paste(dir, '', sep='transfer_trained_perceptual_matching.xlsx'),1)[,c('ID','v1.Acc.sw1','v1.RT.sw0','v1.RT.sw1',
                                                                                           'v2.Acc.sw1','v2.RT.sw0','v2.RT.sw1')]
speed[,-1] <- apply(speed[,-1],2,as.numeric)
speed <- speed[,c('ID','v1.RT.sw0','v1.RT.sw1','v2.RT.sw0','v2.RT.sw1')]
colnames(speed) <- c('ID','v1.speed.sw0','v1.speed.sw1','v2.speed.sw0','v2.speed.sw1')


cogdat <- merge(cogdat, speed, by='ID', all=T)


###########################
### IV. EPISODIC MEMORY ###
###########################

vrec <- read.xlsx2(paste(dir, '', sep='transfer_verbal_recall.xlsx'),1)[,c('ID','v1.Acc', 'v2.Acc')]
srec <- read.xlsx2(paste(dir, '', sep='transfer_spatial_recall.xlsx'),1)[,c('ID','v1.Acc', 'v2.Acc')]

colnames(vrec) <- c('ID','v1.vrec', 'v2.vrec')
colnames(srec) <- c('ID','v1.srec', 'v2.srec')

cogdat <- merge(cogdat, vrec, by='ID', all=T)
cogdat <- merge(cogdat, srec, by='ID', all=T)

cogdat[,10:dim(cogdat)[2]] <- apply(cogdat[,10:dim(cogdat)[2]],2,as.vector)
cogdat[,10:dim(cogdat)[2]] <- apply(cogdat[,10:dim(cogdat)[2]],2,as.numeric)

######################
### V. PERSONALITY ###
######################

pdi <- read.xlsx2(paste(dir, '', sep='pdi.xlsx'),1)[,c('ID','v1.Score_dist', 'v1.Score_time', 'v1.Score_conf',
                                                       'v2.Score_dist', 'v2.Score_time', 'v2.Score_conf')]


baratt <- read.xlsx2(paste(dir, '', sep='baratt.xlsx'),1)[,c('ID','v1.A.Att', 'v1.A.CogInst',
                                                        'v1.A', 'v1.M.Mot', 'v1.M.Pers',
                                                        'v1.M', 'v1.N.SelfCon', 'v1.N.CogComp', 'v1.N',
                                                        'v2.A.Att', 'v2.A.CogInst',
                                                        'v2.A', 'v2.M.Mot', 'v2.M.Pers',
                                                        'v2.M', 'v2.N.SelfCon', 'v2.N.CogComp', 'v2.N')]

neo <- read.xlsx2(paste(dir, '', sep='NEO.xlsx'),1)[,c('ID','v1.O', 'v1.C', 'v1.E', 'v1.A', 'v1.N',
                                                       'v2.O', 'v2.C', 'v2.E', 'v2.A', 'v2.N')]
                                                       
colnames(neo) <- c('ID',paste(c('v1.O', 'v1.C', 'v1.E', 'v1.A', 'v1.N',
                   'v2.O', 'v2.C', 'v2.E', 'v2.A', 'v2.N'),'.neo',sep=''))

lucid <- read.xlsx2(paste(dir, '', sep='lucid.xlsx'),1)[,c('ID','v1.q0','v1.insight', 'v1.control',
                     'v1.thought', 'v1.realism', 'v1.memory', 'v1.dissociation', 'v1.NegEm',
                     'v1.PosEm', 'v2.q0','v2.insight', 'v2.control', 'v2.thought', 'v2.realism',
                     'v2.memory', 'v2.dissociation', 'v2.NegEm', 'v2.PosEm')]

ceq <- read.xlsx2(paste(dir, '', sep='ceq.xlsx'),1)[,c('ID','v1.score', 'v2.score')]
madre <- read.xlsx2(paste(dir, '', sep='madre.xlsx'),1)

persdat <- merge(demogr, pdi, by='ID', all=T)
persdat <- merge(persdat, baratt, by='ID', all=T)
persdat <- merge(persdat, neo, by='ID', all=T)
persdat <- merge(persdat, lucid, by='ID', all=T)
persdat <- merge(persdat, ceq, by='ID', all=T)

persdat[,10:dim(persdat)[2]] <- apply(persdat[,10:dim(persdat)[2]],2,as.vector)
persdat[,10:dim(persdat)[2]] <- apply(persdat[,10:dim(persdat)[2]],2,as.numeric)

# Save source file:
save('cogdat', 'persdat', file=paste(dir, 'sourcedat.rda', sep=''))

############################
### VI. IN-SCANNER TASKS ###
############################

# [...]

####


