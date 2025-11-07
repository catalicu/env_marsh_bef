# Title: Marsh delta ammonia
# Author: 'Dr CG, from SGL thesis scripts'
# Date last edits: '11/05/25'

# Description: This script generates Fig3, featuring the relationship between
# delta ammonia and location, season.
# This script also produces the statistical analysis for the relationships.

# Libraries
library(ggplot2)
library(vegan)
library(lubridate)
library(viridis)
library(gridExtra)
library(lme4)
library(car)

# plot themes
Theme=theme_classic(base_size=11, base_family="Helvetica") +
  theme(axis.line = element_line(linewidth = 1, colour = "black", linetype = "solid")) +theme(plot.title = element_text(size = 12))+ theme(panel.border=element_rect(fill=NA))
Theme2=Theme+ theme(legend.position="none") + theme(panel.border=element_rect(fill=NA))

# load data
setwd('..') # set writing directory to script location and then go one folder up

# Environmental data
meta.fun=read.csv('input_data/TableDeltaNH3_Div.summary.211.csv', header=TRUE)
meta.div=read.table('output_tables/div_table2025-11-06_processed.txt', header=TRUE)

# Edits
## format date in meta.fun
meta.fun$Date2 <- mdy(meta.fun$Date_sampled)
meta.fun$Date_form=as.Date(meta.fun$Date_form.1)
## make a column with site-date information as sampleID
meta.fun$SampleID=paste(meta.fun$Location, meta.fun$Date2, sep='_')
meta.fun$SampleID=paste(meta.fun$Location, meta.fun$Date2, sep='_')

##organize the location and season parameters in ordered factors
### season
meta.fun$Season=factor(meta.fun$Season, 
                       levels=c('Autumn', 'Winter', 'Spring'))
### location
meta.fun$Location=factor(meta.fun$Location, 
                         levels=c('OxPond_1', 'OxPond2', 'Treatmentmarshes', 
                                  'EW_influent','Allen', 'Gearheart','Hauser', 'BayDischarge'))

# FIGURE 3
location.plot2=ggplot(meta.fun, aes(Location, delta_ammonia_by_date)) + 
  geom_boxplot() +
  geom_jitter(size=3,alpha=0.6, aes(shape=factor(Location), color=factor(Season)))+ 
  #scale_color_manual(values=c('orange', 'black', 'yellow')) +
  scale_color_viridis(option='turbo', discrete=TRUE) + 
  ylab('Delta Ammonia (mg/L)') + Theme2 +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5)) +
  scale_shape_manual(values=c(16,17,18,3,4,7,8,10)) +
  annotate('text', label='a.', x=0.7, y=10)
season.plot2=ggplot(meta.fun, aes(Season, delta_ammonia_by_date)) + 
  geom_boxplot() +
  geom_jitter(size=3, alpha=0.6, aes(shape=factor(Location), color=factor(Season)))+ 
  #scale_color_manual(values=c('orange', 'black', 'yellow')) +
  scale_color_viridis(option='turbo', discrete=TRUE) + 
  ylab('Delta Ammonia (mg/L)') + Theme2 +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.6)) +
  scale_shape_manual(values=c(16,17,18,3,4,7,8,10)) +
  annotate('text', label='b.', x=0.5, y=10)
Fig.fun=arrangeGrob(location.plot2, season.plot2, ncol=2)  
#quartz()
plot(Fig.fun)

# STATISTICS:  model selection for delta ammonia ~ season * location
# null model (no relationship)
mod.null.am=lm(delta_ammonia_by_date~1, data=meta.fun)
# null model (only random)
mod.null.am.rand=lmer(delta_ammonia_by_date~1 + (1|Location), data=meta.fun)
# linear model season
mod.am.season.lm=lm(delta_ammonia_by_date~Season, data=meta.fun)
# mixed model season and location
mod.am.season.lmei=lmer(delta_ammonia_by_date~Season + (1|Location), data=meta.fun)
# evaluate
AIC(mod.null.am, mod.null.am.rand, mod.am.season.lm, mod.am.season.lmei)

# Anova wont run if it only has random factor
# add a random fixed variable
meta.fun$y <- rnorm(nrow(meta.fun))
mod.null.am.rand2=lmer(delta_ammonia_by_date~y + (1|Location), data=meta.fun, REML=FALSE)

Anova(mod.null.am.rand2)
summary(mod.null.am.rand2)
# compare with GLMM that includes season as fixed factor
anova(mod.am.season.lmei, mod.null.am.rand)
Anova(mod.am.season.lmei)

# compare mixed model to lm model without location
# first create a mock random variable
meta.fun$Loc_mock=(rep(1,length(meta.fun$Location)))
# create a glmm where the random component has no variation
# notice the control modification so it allows for no groups in random variable
mod.am.season.lmei0=lmer(delta_ammonia_by_date~ 1 + (1|Loc_mock), data=meta.fun, control=lmerControl(check.nlev.gtr.1 = "ignore"))
anova(mod.am.season.lmei0, mod.null.am.rand2)
