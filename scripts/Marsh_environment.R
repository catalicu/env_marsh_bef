# Title: 'Marsh Environment'
# Author: 'Dr CG, from SGL thesis scripts'
# Date last edits: '10/30/30'

# Description: This script xxxx

# Libraries
library(ggplot2)
library(vegan)
library(plyr)
library(dplyr)

# Plot themes
## With legend
Theme=theme_classic(base_size=11, base_family="Helvetica") +
  theme(axis.line = element_line(linewidth = 1, colour = "black", linetype = "solid")) +theme(plot.title = element_text(size = 12))
## Without legends
Theme2=Theme+ theme(legend.position="none") + theme(panel.border=element_rect(fill=NA))

# load data
setwd('..') # set writing directory to script location and then go one folder up
## ASV table to calculate diversity metrics
ASVtable=read.table('phyloseq_outputs/ASVtable_Marsh12021-07-20.txt', header=TRUE)
    ASVtable2=t(ASVtable)
    ASVtable3=ASVtable2[-c(1:23),]
## Taxonomy table
TAXtable=read.table('phyloseq_outputs/TAXtable_Marsh12021-07-20.txt', header=TRUE)

## Metadata molecular from the phyloseq object
metatable=read.table('phyloseq_outputs/METAtable_Marsh12021-07-20.txt', header=TRUE)
    metatable2=metatable[-c(1:23),]
    metatable2$sampleID=metatable2$X.SampleID
## Metadata molecular (raw) - does not include sludge data
meta.table=read.csv('input_data/Metadata_marshstudy.csv', header=TRUE)
    meta.table$sampleID=meta.table$SampleID
## Environmental data
env.table=read.csv('input_data/Envdata_marshstudy.csv', header=TRUE)
    env.table2=env.table[-which(duplicated(env.table$sampleID)),]

# Edits and data processing
## summary environmental: 
### removed sludge data
env.Slu=env.table[which(env.table$type=='sludge'),]
env.wSlu=env.table[-which(env.table$type=='sludge'),]
### calculate average of replicate samples per pond/per date
summary_table <- env.wSlu %>%
  group_by(Location, Date) %>%
  summarise(
    Ammonia = mean(`Ammonia (mg/L)`, na.rm = TRUE),
    BOD = mean(BOD, na.rm = TRUE),
    Temperature = mean(`Water Temp. (C)`, na.rm = TRUE),
    Percipitation = mean(`Percipitation (in)`, na.rm = TRUE)
  )
summary_table <- summary_table %>%
  group_by(Location) %>%
  mutate(
    Ammonia_average_Loc = mean(Ammonia, na.rm = TRUE),
    delta_ammonia_by_date = Ammonia - mean(Ammonia, na.rm = TRUE),
    Delta_Ammonia_Loc = Ammonia - Ammonia_average_Loc
  )
summary_table <- summary_table %>%
  mutate(
    Season = case_when(
      month(Date) %in% c(12,1,2) ~ "Winter",
      month(Date) %in% c(3,4,5) ~ "Spring",
      month(Date) %in% c(6,7,8) ~ "Summer",
      month(Date) %in% c(9,10,11) ~ "Autumn"
    ),
    Percipitation_Regime = ifelse(`Percipitation (in)` > threshold, "Wet", "Dry")
  )

### saved version
env.summary.table=read.csv('input_data/TableDeltaNH3_Div.summary.211.csv', header=TRUE)
  env.summary1=env.summary.table[,-1]

## Edits to metadata
### season
meta.env$Season=factor(meta.env$Season, 
                       levels=c('Autumn', 'Winter', 'Spring'))
### location
meta.env$Location=factor(meta.env$Location, 
                         levels=c('OxPond_1', 'OxPond2', 'Treatmentmarshes',
                                  'EW_influent', 'Allen', 'Gearheart','Hauser', 
                                  'BayDischarge'))
meta.env$Date_form=as.Date(meta.env$Date_form)

# Univariate diversity measures
richness=specnumber(ASVtable3, MARGIN=1)
shannon=diversity(ASVtable3, index='shannon', MARGIN=1)
simpson=diversity(ASVtable3, index='simpson', MARGIN=1)
evenness=shannon/log(richness)

#merge metadata and univariate diversity measures
meta3=meta2[which(meta2[,1]%in%rownames(ASVtable3)),]
meta.div=data.frame(meta3, richness, shannon, simpson, evenness)

# merge env, meta:
env.table3=env.table2[which(env.table2$sampleID  %in%  metatable2$sampleID),]
meta1=left_join(metatable2, env.table3, by=c('sampleID'))
meta2=left_join(metatable, meta1)

# summary
Div.summary=ddply(meta.div, .(Sample_Location, Date_sampled), summarize, count=length(richness), meanR=mean(richness), sdR=sd(richness), meanShann=mean(shannon), sdShan=sd(shannon), meanSimp=mean(simpson), sdSimp=sd(simpson), meanE=mean(evenness), sdE=sd(evenness))
Div.summary