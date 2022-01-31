## R Markdown
trees <- read.csv('https://raw.githubusercontent.com/dmcglinn/quant_methods/gh-pages/data/treedata_subset.csv')
trees
# 1. Carry out an exploratory analysis using the tree dataset. Metadata for the tree study can be found here. Specifically, I would like you to develop and compare models for species cover for a habitat generalist Acer rubrum (Red maple) and a habitat specialist Abies fraseri (Frasier fir). Because this dataset includes both continuous and discrete explanatory variables use the function Anova in the packages car as such
library(car)
# Anova(my_mod, type=3)
# Compare the p-values you observe using the function Anova to those generated using summary.
hey girl 

# For each species address the following additional questions:

# how well does the exploratory model appear to explain cover?
# which explanatory variables are the most important?
# do model diagnostics indicate any problems with violations of OLS assumptions?
# are you able to explain variance in one species better than another, why might this be the case?
# Prior to addressing the above questions you will want to restructure and subset the data using the # # following R code:

# we wish to model species cover across all sampled plots
# create site x sp matrix for two species 
sp_cov = with(trees, tapply(cover, list(plotID, spcode), 
                            function(x) round(mean(x))))
sp_cov = ifelse(is.na(sp_cov), 0, sp_cov)
sp_cov = data.frame(plotID = row.names(sp_cov), sp_cov)
# create environmental matrix
cols_to_select = c('elev', 'tci', 'streamdist', 'disturb', 'beers')
env = aggregate(trees[ , cols_to_select], by = list(trees$plotID), 
                function(x) x[1])
names(env)[1] = 'plotID'
# merge species and enviornmental matrices
site_dat = merge(sp_cov, env, by='plotID')
# subset species of interest
abies = site_dat[ , c('ABIEFRA', cols_to_select)]
acer  = site_dat[ , c('ACERRUB', cols_to_select)]
names(abies)[1] = 'cover'
names(acer)[1] = 'cover'

library(ggplot2)
library(gridExtra)
library(scatterplot3d)
library(MASS)

abies
acer
names(abies)
names(acer)


ggplot(data = abies) + 
  geom_boxplot(mapping = aes(x = disturb, y = cover)) +  
  labs(x = 'Disturbance', y = 'Cover', title = 'abies disturbance versus cover') 

ggplot(data = acer) + 
  geom_boxplot(mapping = aes(x = disturb, y = cover)) +  
  labs(x = 'Disturbance', y = 'Cover', title = 'acer disturbance versus cover') 


quantile(abies$cover[abies$disturb == 'VIRGIN'])
quantile(abies$cover[abies$disturb == 'CORPLOG'])
quantile(abies$cover[abies$disturb == 'SETTLE'])
quantile(abies$cover[abies$disturb == 'LT-SEL'])

quantile(acer$cover[acer$disturb == 'VIRGIN'])
quantile(acer$cover[acer$disturb == 'CORPLOG'])
quantile(acer$cover[acer$disturb == 'SETTLE'])
quantile(acer$cover[acer$disturb == 'LT-SEL'])

#ABIES
plot(cover ~ elev, data = abies, xlab = 'Elevation (m)',
     ylab = 'Cover', main= 'abies elevation versus cover')

plot(cover ~ tci, data = abies, xlab = 'Topographic Coverage Index',
     ylab = 'Cover', main= 'abies tci versus cover')

plot(cover ~ streamdist, data = abies, xlab = 'Stream Distance (m)',
     ylab = 'Cover', main= 'abies stream distance versus cover')

plot(cover ~ beers, data = abies, xlab = 'Beers',
     ylab = 'Cover', main= 'abies beers versus cover')

#ACER
plot(cover ~ elev, data = acer, xlab = 'Elevation (m)',
     ylab = 'Cover', main= 'acer elevation versus cover')

plot(cover ~ tci, data = acer, xlab = 'Topographic Coverage Index',
     ylab = 'Cover', main= 'acer tci versus cover')

plot(cover ~ streamdist, data = acer, xlab = 'Stream Distance (m)',
     ylab = 'Cover', main= 'acer stream distance versus cover')

plot(cover ~ beers, data = acer, xlab = 'Beers',
     ylab = 'Cover', main= 'acer beers versus cover')

# Intercept only model for Abers and Acies, cover as y variable

null_mod_abies = lm(cover ~ 1, data = abies)
null_mod_acer = lm(cover ~ 1, data = acer)     
null_mod_abies
null_mod_acer

mean(abies$cover)
mean(acer$cover)

plot(cover ~ 1, data = abies)
abline(null_mod_abies, lwd = 2)
abline(h = mean(abies$cover), col = 'red', lty = 2, lwd = 2)

plot(cover ~ 1, data = acer)
abline(null_mod_acer, lwd = 2)
abline(h = mean(acer$cover), col = 'red', lty = 2, lwd = 2)

# create single variable main effect models for each 
# abies
elev_mod_abies = lm(cover ~ elev, data = abies)
tci_mod_abies = lm(cover ~ tci, data = abies)
streamdist_mod_abies = lm(cover ~ streamdist, data = abies)
disturb_mod_abies = lm(cover ~ disturb, data = abies)
beers_mod_abies = lm(cover ~ beers, data = abies)

# acer 
elev_mod_acer = lm(cover ~ elev, data = acer)
tci_mod_acer = lm(cover ~ tci, data = acer)
streamdist_mod_acer = lm(cover ~ streamdist, data = acer)
disturb_mod_acer = lm(cover ~ disturb, data = acer)
beers_mod_acer = lm(cover ~ beers, data = acer)

# abies single model main effect summaries 
summary(elev_mod_abies)
Anova(elev_mod_abies, type= 3)

summary(tci_mod_abies)
summary(streamdist_mod_abies)
summary(disturb_mod_abies)
summary(beers_mod_abies)

# significant p-values = elev, disturb (b/c of VIRGIN), streamdist

# acer single model main effect summaries 
summary(elev_mod_acer)
summary(tci_mod_acer)
summary(streamdist_mod_acer)
summary(disturb_mod_acer)
summary(beers_mod_acer)

# significant p-values = all EXCEPT tci

# beers has a significant effect on acers, but not abies

# all main effects for both
all_mod_abies = lm(cover ~ elev + tci + streamdist + disturb + beers, data = abies) 
all_mod_acer = lm(cover ~ elev + tci + streamdist + disturb + beers, data = acer)

summary(all_mod_abies)
summary(all_mod_acer)

# identifying outlers and modifying acer bc of them

identify(acer$cover ~ acer$tci, n = 2)

# outliers in this plot tci vs acer cover
identify(acer$cover ~ acer$tci, n = 2)
[1] 121 318
# remove tci outliers
acer_subset = acer[-c(121, 318), ]
plot(cover ~ tci, data= acer_subset, main="acer subset tci versus cover", xlab='tci', ylab= 'cover')

dim(acer)
dim(acer_subset)

plot(cover ~ elev, data = acer, xlab = 'Elevation (m)',
     ylab = 'Cover', main= 'acer elevation versus cover')

plot(cover ~ tci, data = acer, xlab = 'Topographic Coverage Index',
     ylab = 'Cover', main= 'acer tci versus cover')

plot(cover ~ streamdist, data = acer, xlab = 'Stream Distance (m)',
     ylab = 'Cover', main= 'acer stream distance versus cover')

plot(cover ~ beers, data = acer, xlab = 'Beers',
     ylab = 'Cover', main= 'acer beers versus cover')

# remove possible stream distance acer outlier
identify(acer$cover ~ acer$streamdist, n=1)
acer_subset = acer[-c(121, 318, 297), ]
dim(acer_subset)

plot(cover ~ streamdist, data = acer_subset, xlab = 'Stream Distance (m)',
     ylab = 'Cover', main= 'acer stream distance versus cover subset')


elev_mod_acersubset = lm(cover ~ elev, data = acer_subset)
tci_mod_acersubset = lm(cover ~ tci, data = acer_subset)
streamdist_mod_acersubset = lm(cover ~ streamdist, data = acer_subset)
disturb_mod_acersubset = lm(cover ~ disturb, data = acer_subset)
beers_mod_acersubset = lm(cover ~ beers, data = acer_subset)

# comparing the original to the subset without outliers 
summary(elev_mod_acer)
summary(elev_mod_acersubset)
summary(tci_mod_acer)
summary(tci_mod_acersubset)
summary(streamdist_mod_acer)
summary(streamdist_mod_acersubset)
summary(disturb_mod_acer)
summary(disturb_mod_acersubset)
summary(beers_mod_acer)
summary(beers_mod_acersubset)

# comparing the original main model with the subset main model
all_mod_acersubset = lm(cover ~ elev + tci + streamdist + disturb + beers, data = acer_subset)
summary(all_mod_acersubset)
summary(all_mod_acer)

# removing the outliers did slightly improve the R-squared and adjusted R- squared, it also greatly increased the significance of tci( lowered the p value), so the subset without the outliers will be used from now on

# model with elev, tci, and beers 

etb_mod_acersubset = lm(cover ~ elev + tci + beers , data = acer_subset)
summary(etb_mod_acersubset)

etb_interaction_mod_acersubset = lm(cover ~ elev + tci + beers + tci * elev + elev * beers + beers * tci + beers * tci * elev , data = acer_subset)
summary(etb_interaction_mod_acersubset)

# interaction effect with elev:tci and elev:beers
AIC(etb_interaction_mod_acersubset)
#[1] 3413.76
AIC(etb_mod_acersubset)
#[1] 3419.997
## the above model just proved my suspicions on how important elevation was to the model

#adding elevation interaction to main model
elev_interaction_mod_acersubset = lm(cover ~ elev + tci + streamdist + disturb + beers + elev * tci + elev * beers + elev * streamdist + elev * disturb, data = acer_subset)
summary(elev_interaction_mod_acersubset)

elev_interaction_mod_acersubset = lm(cover ~ elev + tci + streamdist + disturb + beers + elev * tci + elev * beers + elev * streamdist + elev * disturb, data = acer_subset)

# adding interaction to whole model
# ask on tuesday how to include only VIRGIN disturb values in regression


full_mod_acersubset = update(all_mod_acersubset, ~ . + elev * disturb * tci * streamdist * beers)
all_mod_acersubset = lm(cover ~ elev + tci + streamdist + disturb + beers, data = acer_subset)

summary(all_mod_acersubset)
summary(elev_interaction_mod_acersubset)
summary(full_mod_acersubset) 
# this makes it absurdly long, but it does increase the r-squared...


anova(all_mod_acersubset, full_mod_acersubset)

AIC(full_mod_acersubset)
AIC(all_mod_acersubset)

# remove abies outlier in tci and elevation  
# identify(abies$cover ~ abies$tci, n=2)

abies_subset = abies[-c(121,33), ]
dim(abies_subset)

# comparing the main models of original and subset
all_mod_abiessubset = lm(cover ~ elev + tci + streamdist + disturb + beers, data = abies_subset)
summary(all_mod_abiessubset)
summary(all_mod_abies)


# abies with interaction
full_mod_abiessubset = update(all_mod_abiessubset, ~ . + elev * disturb * tci * streamdist * beers)
elev_interaction_mod_abiessubset = lm(cover ~ elev + tci + streamdist + disturb + beers + elev * tci + elev * beers + elev * streamdist + elev * disturb, data = abies_subset)

summary(all_mod_abiessubset)
summary(elev_interaction_mod_abiessubset)
summary(full_mod_abiessubset) 

# interaction had an even larger effect on the abies distributions 

# assume all poisson calculations are done using the subsets with outliers removed
summary(elev_interaction_mod_abiessubset)
summary(elev_interaction_mod_acersubset)

# poisson indivisual variables acer
elev_mod_acer_poi = glm(cover ~ elev, data = acer_subset, family = "poisson")
tci_mod_acer_poi = glm(cover ~ tci, data = acer_subset, family = "poisson")
streamdist_mod_acer_poi = glm(cover ~ streamdist, data = acer_subset, family = "poisson")
disturb_mod_acer_poi = glm(cover ~ disturb, data = acer_subset, family = "poisson")
beers_mod_acer_poi = glm(cover ~ beers, data = acer_subset, family = "poisson")

# poisson individual variables abies 
elev_mod_abies_poi = glm(cover ~ elev, data = abies_subset, family = "poisson")
tci_mod_abies_poi = glm(cover ~ tci, data = abies_subset, family = "poisson")
streamdist_mod_abies_poi = glm(cover ~ streamdist, data = abies_subset, family = "poisson")
disturb_mod_abies_poi = glm(cover ~ disturb, data = abies_subset, family = "poisson")
beers_mod_abies_poi = glm(cover ~ beers, data = abies_subset, family = "poisson")

# summaries of individual poisson models
summary(elev_mod_abies_poi)
summary(tci_mod_abies_poi)
summary(streamdist_mod_abies_poi)
summary(disturb_mod_abies_poi)
summary(beers_mod_abies_poi)

summary(elev_mod_acer_poi)
summary(tci_mod_acer_poi)
summary(streamdist_mod_acer_poi)
summary(disturb_mod_acer_poi)
summary(beers_mod_acer_poi)


# poisson with elevation interaction only
abies_poi = glm(cover ~ elev + tci + streamdist + disturb + beers + elev * tci + elev * beers + elev * streamdist + elev * disturb, data = abies_subset, 
                family='poisson')
acer_poi = glm(cover ~ elev + tci + streamdist + disturb + beers + elev * tci + elev * beers + elev * streamdist + elev * disturb, data = acer_subset, 
               family='poisson')

summary(abies_poi)
summary(acer_poi)

# pseudo r ^2
pseudo_r2 = function(glm_mod) {
  1 -  glm_mod$deviance / glm_mod$null.deviance
}
pseudo_r2(abies_poi)
pseudo_r2(acer_poi)

# comparing poisson with equivalent OLS
summary(abies_poi)
summary(elev_interaction_mod_abiessubset)
pseudo_r2(abies_poi)
summary(acer_poi)
summary(elev_interaction_mod_acersubset)
pseudo_r2(acer_poi)
# comparing with anova
Anova(abies_poi, type=3)
Anova(elev_interaction_mod_abiessubset, type=3)
Anova(acer_poi, type=3)
Anova(elev_interaction_mod_acersubset, type=3)

# poisson might be better at estimating abies, but not acer based on the pseudo
# r squared values, but we are confused why none of the variables in the summary description were significant
