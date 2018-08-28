#!/usr/bin/env Rscript

library(runjags)
library(coda)
library(mcmcplots)

aineisto <- readRDS('modeldata/general_new9.rds')

for(varname in c('lang','funct','morph','pos','isnumeric')){
    aineisto[[varname]] <- as.factor(aineisto[[varname]])
}

observations <- xtabs(~ lang + funct + morph + pos + isnumeric + location3, data=aineisto)
totals <- xtabs(~ lang + funct + morph + pos + isnumeric, data=aineisto)

dataList <- list(observations=observations, totals=totals, Nlang = length(unique(aineisto$lang)), Nfunct = length(unique(aineisto$funct)), Nmorph = length(unique(aineisto$morph)), Npos = length(unique(aineisto$pos)), Nisnumeric = length(unique(aineisto$isnumeric)), Nlocation3 = length(unique(aineisto$location3)))

monitor <- c('lang', 'funct', 'morph', 'pos', 'isnumeric', 'std.lang', 'std.funct', 'std.morph', 'std.pos', 'std.isnumeric', 'lang.funct', 'lang.morph', 'lang.pos', 'lang.isnumeric', 'std.lang.funct', 'std.lang.morph', 'std.lang.pos', 'std.lang.isnumeric')

RunJagsModel <- run.jags(data=dataList, monitor=monitor, model='model_specifications/general_new9/model.bugs', adapt=30000.0, n.chains=2.0, burnin=10000.0, thin=1.0, sample=20000.0, method='parallel')

post <- as.mcmc.list(RunJagsModel)
saveRDS(post,"modeldata/general_new9_post.rds")
mcmcplot(post,dir="mcmc_diagnostics/general_new9")
summary(post)