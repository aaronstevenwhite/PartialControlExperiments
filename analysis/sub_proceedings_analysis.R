###############
# load packages
###############

## data manipulation
library(plyr)
library(reshape)

## graphing
library(ggplot2)

## various utilities
library(MASS)

## mixed effects models
library(MCMCglmm)

## graphing with tikz
library(tikzDevice)

## random forests
library(randomForest)
library(party)

## training/tuning functions
library(caret)

## distance functions
library(proxy)

## multicore support
library(doMC)

registerDoMC(cores = 4)

###############
# configuration
###############

## set the ggplot theme to black and white
theme_set(theme_bw())

###########
# data load
###########

## load dataset from its location in the experiments folder

root <- '~/PartialControl/'

load.dataset <- function(root, fname, exp.num){
  ## create file path string
  fpath <- paste(root, fname, sep='')
  
  ## read the file from the file path location
  df <- read.csv(fpath, header=T)
  
  ## turn the subject column into a factor
  df$subj <- as.factor(df$subj)
  
  ## add the experiment number column
  df$exp <- exp.num
  
  ## create a column with the Likert scale response converted into a factor
  df$response.factor <- as.factor(df$response)
  
  return(df)
}

# load datasets
pc1 <- load.dataset(root=root, fname='data/pc1/pc1_data.csv', exp.num='one')
pc2 <- load.dataset(root=root, fname='data/pc2/pc2_data.csv', exp.num='two')
pc3 <- load.dataset(root=root, fname='data/pc3/pc3_data.csv', exp.num='three')

## combine all three datasets
pc <- rbind(pc1, pc2, pc3)

## create a new column for formatted verb labels
pc$verb.formatted <- pc$verb

## format the verb labels
levels(pc$verb.formatted) <- c('baseline', 
                               'began to', 
                               'claimed to', 
                               'filler', 
                               'hoped to', 
                               'intended to', 
                               'loved to', 
                               'managed to', 
                               'needed to', 
                               'tried to', 
                               'wanted to', 
                               'was likely to', 
                               'decided to', 
                               'expected to', 
                               'offered to', 
                               'planned to', 
                               'preferred to', 
                               'promised to', 
                               'refused to', 
                               'was afraid to', 
                               'was eager to', 
                               'was ready to', 
                               'deserved to', 
                               'hated -ing', 
                               'hated to', 
                               'liked -ing', 
                               'liked to', 
                               'loved -ing', 
                               'pretended to', 
                               'regretted -ing', 
                               'remembered -ing', 
                               'remembered to')


#############
# RT outliers
#############

## plot mean logged reaction times
p.rt <- ggplot(pc, aes(x=subj, y=rt)) + geom_boxplot() + scale_y_log10(name='Reaction time')

## calculate each participant's mean logged reaction time
logrt.means <- ddply(pc, .(subj), summarize, mean.logrt=mean(log(rt)))

## calculate the mean of the mean logged reaction times
mean.mean.logrt <- mean(logrt.means$mean)

## calculate the standard deviation of the mean logged reaction times
std.dev <- sd(logrt.means$mean)

## take the subset of participants that have a mean logged reaction time
## less than two standard deviations below the mean of mean logged reaction times
exclude <- subset(logrt.means, mean.logrt < (mean.mean.logrt - (std.dev * 2)))

## remove participants in the exclude dataframe
for (subject in unique(exclude$subj)){
  pc <- subset(pc, subj != subject)  
}

## drop those subjects from the levels of subj
pc$subj <- pc$subj[drop=T,]


#########
# fillers
#########

## remove the fillers
pc.nofiller <- subset(pc, verb != 'filler')

## drop "filler" from levels
pc.nofiller$verb <- pc.nofiller$verb[drop=T,]
pc.nofiller$verb.formatted <- pc.nofiller$verb.formatted[drop=T,]
pc.nofiller$pred <- pc.nofiller$pred[drop=T,]
pc.nofiller$ptype <- pc.nofiller$ptype[drop=T,]


###########
# raw means
###########

## calculate raw means by verb and predicate type (collective v. noncollective)
raw.mean <- ddply(pc.nofiller, .(verb.formatted,ptype), summarise, mean=mean(response), cilo=mean(response)-sd(response)/length(response), cihi=mean(response)+sd(response)/length(response))

## put separate predicate type in separate columns
raw.mean.cast <- cast(raw.mean[,c('verb.formatted', 'ptype', 'mean')], verb.formatted~ptype)

## find the difference between collective and noncollective predicate types for each verb
raw.mean.cast$diff <- raw.mean.cast$noncollective - raw.mean.cast$collective

## order both the verb and formatted verb columns by this difference
raw.mean$verb <- ordered(raw.mean$verb.formatted, 
                         levels=raw.mean.cast$verb.formatted[rev(order(raw.mean.cast$diff))])

pc.nofiller$verb.mean.ordered <- ordered(pc.nofiller$verb.formatted, 
                                         levels=raw.mean.cast$verb.formatted[rev(order(raw.mean.cast$diff))])

## create the mean rating plot
p.mean.rating <- ggplot(raw.mean, aes(x=verb, y=mean-4, fill=ptype, ymax=cihi, ymin=cilo)) + 
                 geom_bar(color="black", stat="identity", position="dodge") + 
                 scale_fill_grey(name="Embedded\npredicate type") + 
                 scale_y_continuous(name='Mean rating', 
                                    breaks=seq(-2, 3), 
                                    labels=seq(2,7)) + 
                 ggtitle('Mean rating by verb and embedded predicate type') + 
                 theme(axis.text.x = element_text(angle=45, 
                                                  hjust=1, 
                                                  vjust=1), 
                       legend.justification=c(1,0), 
                       legend.position=c(1,0), 
                       legend.background=element_rect(color="black")) + 
                 geom_hline(yintercept=0) + 
                 scale_x_discrete(name='')

## write the mean ratings graph to a TikZ file
tikz('~/PartialControl/analysis/plots/rawmeanrating.tikz', width=6.5, height=4)
p.mean.rating
dev.off()


###############
# model fitting
###############

## set the seed for reproducibility
set.seed(456)

## specify weakest proper priors
clmm.prior <- list(R = list(V = 1, nu = 0, fix = 1),
                   G = list(G1 = list(V = diag(72), nu = .002),
                            G2 = list(V = diag(62), nu = .002),
                            G3 = list(V = 1, nu = .002),
                            G4 = list(V = 1, nu = .002)))

## fit the model (this took ~2 hours on my machine)
m <- MCMCglmm(response.factor ~ verb.formatted*ptype + exp*ptype, 
              random=~idh(verb.formatted*ptype + pred*ptype):subj + idh(verb.formatted*ptype):pred + subj + pred,
              prior = clmm.prior,
              family = "ordinal",
              burnin = 200000,
              nitt = 1200000,
              thin = 1000, 
              pr=T,
              data = pc.nofiller)

## save the model to a file
#save(file='~/PartialControl/analysis/clmm.RData', m)


##################
# model statistics
##################

## model.component will be either Sol (coefficient estimates) or VCV (variance estimates)
extract.mode.and.hpd <- function(model.component){
  ## extract posterior modes from model component
  modes <- as.data.frame(posterior.mode(model.component))
  names(modes) <- 'mode'
  modes$term <- as.factor(rownames(modes))
  
  ## extract highest posterior density intervals from model component
  hpd.intervals <- as.data.frame(HPDinterval(model.component))
  hpd.intervals$term <- as.factor(rownames(hpd.intervals))
  
  ## merge modes and hpd intervals
  estimates <- merge(modes, hpd.intervals)
  
  return(estimates)
}

###############
# fixed effects
###############

## "filtered ratings" in the paper

## extract fixed effects statistics
model.coef.estimates <- extract.mode.and.hpd(m$Sol)

## extract verb fixed effects
verb.fixef.rowindices <- seq(nrow(model.coef.estimates)-59, nrow(model.coef.estimates), 2)
verb.collective.fixed.effects <- model.coef.estimates[verb.fixef.rowindices,]

## remove term type ("verb") from beggining of term string
verb.collective.fixed.effects$verb <- as.factor(substring(verb.collective.fixed.effects$term, 15))

## order verbs by fixed levels

verb.collective.fixed.effects$verb.ordered <- ordered(verb.collective.fixed.effects$verb, levels=verb.collective.fixed.effects$verb[order(verb.collective.fixed.effects$mode)])

#### plot fixed effects

p.fixed <- ggplot(verb.collective.fixed.effects, aes(x=verb.ordered, y=mode, ymax=upper, ymin=lower)) + 
           geom_bar(color="black", 
                    stat="identity", 
                    position="dodge") + 
           geom_errorbar(position=position_dodge(.9), 
                         width=.2) +  
           scale_y_continuous(name='Rating difference from baseline', 
                              breaks=-1:4) + 
           ggtitle('Verb rating after filtering variability due to participant and embedded predicate') +
           opts(axis.text.x = element_text(angle=45, 
                                           hjust=1, 
                                           vjust=1), 
                legend.justification=c(1,0), 
                legend.position=c(1,.8), 
                legend.background=element_rect(color="black")) + 
           geom_hline(yintercept=0) + 
           scale_x_discrete(name='')

## write the "filtered ratings" to a TikZ file
tikz('~/PartialControl/analysis/plots/filtered.tikz', width=6.5, height=4)
p.fixed
dev.off()

################
# random effects
################

model.var.estimates <- extract.mode.and.hpd(m$VCV)

## extract only collective coefficients

model.var.estimates <- model.var.estimates[grep('noncollective', model.var.estimates$term, invert=T),]

## add variance type

verb.collective.random.effects <- model.var.estimates[grep('\\.', model.var.estimates$term),]
verb.collective.random.effects <- verb.collective.random.effects[grep('noncollective', verb.collective.random.effects$term, invert=T),]

verb.collective.random.effects$Variability <- 'participant'
verb.collective.random.effects[grep('\\.pred', verb.collective.random.effects$term),]$Variability <- 'embedded predicate'

process.term <- function(term){
  s <- strsplit(term, '\\.')
  l <- length(s[[1]])-1
  verb <- s[[1]][l]
  verb <- sub('formatted', '', verb)
  
  return(verb)
}

process.term <- Vectorize(process.term)

verb.collective.random.effects$verb <- as.factor(process.term(as.character(verb.collective.random.effects$term)))

verb.collective.random.effects <- merge(verb.collective.random.effects, as.data.frame(levels(pc.nofiller$verb.formatted)), by.x='verb', by.y=names(as.data.frame(levels(pc.nofiller$verb.formatted))))

####

verb.collective.random.effects$verb.ordered <- ordered(verb.collective.random.effects$verb, levels=c('base', as.vector(verb.collective.fixed.effects$verb[order(verb.collective.fixed.effects$mode)])))

####

verb.collective.random.effects[verb.collective.random.effects$mode < 0,]$mode <- 0

####

p.random <- ggplot(verb.collective.random.effects, 
                   aes(x=verb.ordered, 
                       y=mode, 
                       ymax=upper, 
                       ymin=lower, 
                       fill=Variability)) + 
            geom_bar(color="black", 
                     stat="identity", 
                     position="dodge") +
            scale_fill_grey() + 
            scale_y_continuous(name='Variance') + 
            ggtitle('Variability in embedding predicate rating') +
            opts(axis.text.x = element_text(angle=45, hjust=1, vjust=1), 
                 legend.justification=c(1,0), 
                 legend.position=c(.65,.6), 
                 legend.background=element_rect(color="black")) + 
            geom_hline(yintercept=0) + 
            scale_x_discrete(name='')

## write the "rating variability" to a TikZ file
tikz('~/PartialControl/analysis/plots/variability.tikz', width=6.5, height=4)
p.random
dev.off()


##################
# feature modeling
##################

## extract model fixed effects
model.coefs <- as.data.frame(m$Sol[,2:31])
model.coefs <- melt(model.coefs)
names(model.coefs) <- c('verb', 'acceptability')
model.coefs$sample <- 1:1000

model.coefs$verb <- as.factor(substring(model.coefs$verb,15))

## load features
features.path <- paste(root, "features_sub.csv", sep='')

features <- read.csv(features.path, header=T, sep='\t')

## turn all feature columns into factors
for (f in names(features)[2:length(names(features))]){
  features[[f]] <- as.factor(features[[f]])
}

## put the features and the fixed effect coefficients together
model.coefs <- merge(model.coefs, features)

## set reference levels for each variable (only necessary for regression)
# model.coefs$attitudinality <- relevel(model.coefs$attitudinality, 'nonattitudinal')
# model.coefs$transparency <- relevel(model.coefs$transparency, 'transparent')
# model.coefs$telicity <- relevel(model.coefs$telicity, 'telic')
# model.coefs$tense <- relevel(model.coefs$tense, 'none')
# model.coefs$simultaneity <- relevel(model.coefs$simultaneity, 'simultaneous')
# model.coefs$truth <- relevel(model.coefs$truth, 'neither')
# model.coefs$complement <- relevel(model.coefs$complement, 'to')
# model.coefs$syntclass <- relevel(model.coefs$syntclass, 'V')

## create column for verb (as opposed to verb+complement) 
# process.verb <- function(verb){
#   verb.split <- strsplit(as.character(verb), split='\\s')[[1]]
#   ind <- length(verb.split)-1
#   return(verb.split[ind])
# }
# 
# process.verb <- Vectorize(process.verb)
# 
# model.coefs$verb.base <- as.factor(process.verb(model.coefs$verb))

## set random seed for reproducibility
set.seed(317)

## specify training/tuning controller
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           verboseIter=T)

## specify tuning grid
rfGrid <- expand.grid(.mtry=1:4)

## fit random forests with repeated cross validation and stratified bootstrap
forest.fit <- train(acceptability ~ attitudinality + transparency + simultaneity + telicity,
                    data=model.coefs,
                    method = "rf",
                    trControl=fitControl,
                    tuneGrid=rfGrid,
                    ntree=1000,
                    importance=T,
                    strata='verb')

## check for MRSE elbow to avoid overfitting
## MRSE "elbow" occurs at mtry=2
#plot(forest.fit)

## fit forest with 2 tries per split
forest <- randomForest(acceptability ~ attitudinality + transparency + simultaneity + telicity,
                       importance=T,
                       strata=verb,
                       ntree=1000,
                       mtry=2,
                       data=model.coefs)

## extract variable importance 
forest.varimp <- forest$importance

## fit conditional random forest
cond.forest <- cforest(acceptability ~ attitudinality + transparency + simultaneity + telicity,
                      data=model.coefs)

## compute conditional variable importance
cond.forest.varimp <- varimp(cond.forest, conditional=T)

## predict acceptability from random forest
model.coefs$prediction <- predict(forest, predict.all=T)

## compute means at each successive split
split.means <- list()
splits <- c('attitudinality', 'transparency', 'simultaneity', 'telicity')

for (index in 1:length(splits)){
  variable <- splits[index]
  split.means[[variable]] <- ddply(model.coefs,
                                   splits[1:index],
                                   summarise,
                                   mean.acceptability=mean(acceptability))
}
