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


# load species tables
setwd('..') # set writing directory to script location and then go one folder up
## ASV table to calculate diversity metrics
ASVtable=read.table('phyloseq_outputs/ASVtable_Marsh12021-07-20.txt', header=TRUE)
    ASVtable2=t(ASVtable)
    ASVtable3=ASVtable2[-c(1:23),]
## Taxonomy table
TAXtable=read.table('phyloseq_outputs/TAXtable_Marsh12021-07-20.txt', header=TRUE)
## Metadata molecular
metatable=read.table('phyloseq_outputs/METAtable_Marsh12021-07-20.txt', header=TRUE)
    metatable2=metatable[-c(1:23),]
    metatable2$sampleID=metatable2$X.SampleID

# metadata tables
## molecular
meta.table=read.csv('input_data/Metadata_marshstudy.csv', header=TRUE)
  meta.table$sampleID=meta.table$SampleID
## environmental
env.table=read.csv('input_data/Envdata_marshstudy.csv', header=TRUE)
env.table2=env.table[-which(duplicated(env.table$sampleID)),]
## summary environmental: 
### removed sludge data
env.wSlu=env.table[-which(env.table$type=='sludge'),]
### calculate average of replicate samples per pond/per date

env.summary.table=read.csv('input_data/TableDeltaNH3_Div.summary.211.csv', header=TRUE)

# merge env, meta:
env.table3=env.table2[which(env.table2$sampleID  %in%  metatable2$sampleID),]
meta1=left_join(metatable2, env.table3, by=c('sampleID'))
meta2=left_join(metatable, meta1)

# summary
Div.summary=ddply(meta.div, .(Sample_Location, Date_sampled), summarize, count=length(richness), meanR=mean(richness), sdR=sd(richness), meanShann=mean(shannon), sdShan=sd(shannon), meanSimp=mean(simpson), sdSimp=sd(simpson), meanE=mean(evenness), sdE=sd(evenness))
Div.summary

# Edits to metadata
# season
meta.env$Season=factor(meta.env$Season, 
                       levels=c('Autumn', 'Winter', 'Spring'))
# location
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
