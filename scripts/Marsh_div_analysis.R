# Title: 'Marsh div analysis'
# Author: 'Dr CG, from SGL thesis scripts'
# Date last edits: '10/30/30'

# Description: This script xxxx

# Libraries
library(ggplot2)
library(vegan)
#library(plyr)
library(dplyr)
#library(lme4)
#library(nlme)
#library(gridExtra)

# Plot themes
## With legend
Theme=theme_classic(base_size=11, base_family="Helvetica") +
  theme(axis.line = element_line(linewidth = 1, colour = "black", linetype = "solid")) +theme(plot.title = element_text(size = 12))
## Without legends
Theme2=Theme+ theme(legend.position="none") + theme(panel.border=element_rect(fill=NA))


# load species tables
setwd('..') # set writing directory to script location and then go one folder up
ASVtable=read.table('phyloseq_outputs/ASVtable_Marsh12021-07-20.txt', header=TRUE)
  ASVtable2=t(ASVtable)
  ASVtable3=ASVtable2[-c(1:23),]
  
TAXtable=read.table('phyloseq_outputs/TAXtable_Marsh12021-07-20.txt', header=TRUE)

metatable=read.table('phyloseq_outputs/METAtable_Marsh12021-07-20.txt', header=TRUE)
  metatable2=metatable[-c(1:23),]

metatable2$sampleID=metatable2$X.SampleID

# metadata tables
meta.table=read.csv('input_data/Metadata_marshstudy.csv', header=TRUE)
meta.table$sampleID=meta.table$SampleID
env.table=read.csv('input_data/Envdata_marshstudy.csv', header=TRUE)
    env.table2=env.table[-which(duplicated(env.table$sampleID)),]

# merge env, meta:
env.table3=env.table2[which(env.table2$sampleID  %in%  metatable2$sampleID),]
meta1=left_join(metatable2, env.table3, by=c('sampleID'))
meta2=left_join(metatable, meta1)

# re arrange data:
# - Include environmental parameters (ST)
# - Separate experiment from survey study.

# Univariate diversity measures

richness=specnumber(ASVtable3, MARGIN=1)
shannon=diversity(ASVtable3, index='shannon', MARGIN=1)
simpson=diversity(ASVtable3, index='simpson', MARGIN=1)
evenness=shannon/log(richness)

meta.div=data.frame(meta1, richness, shannon, simpson, evenness)

# summary
Div.summary=ddply(meta.div, .(Sample_Location, Date_sampled), summarize, count=length(richness), meanR=mean(richness), sdR=sd(richness), meanShann=mean(shannon), sdShan=sd(shannon), meanSimp=mean(simpson), sdSimp=sd(simpson), meanE=mean(evenness), sdE=sd(evenness))
Div.summary
#ggplot(Div.summary, aes(Sample_Location, meanR)) + geom_point(size=3, color='black', shape=21, aes(fill=Date_sampled))+ geom_errorbar(aes(x=Sample_Location, ymin=meanR-sdR, ymax=meanR+sdR))+ Theme 
ggplot(Div.summary, aes(Sample_Location, meanR)) + geom_point(size=3, color='black', shape=21, aes(fill=Date_sampled))+ Theme 

ggplot(Div.summary, aes(Date_sampled, meanR)) + geom_point(size=3, color='black', shape=21, aes(fill=Sample_Location))+  Theme 


## from Marsh_environment.R
# remove if redundant
# Add Univariate diversity measures
## Calculate diverstiy
richness=specnumber(ASVtable3, MARGIN=1)
shannon=diversity(ASVtable3, index='shannon', MARGIN=1)
simpson=diversity(ASVtable3, index='simpson', MARGIN=1)
evenness=shannon/log(richness)

## Merge original metadata and univariate diversity measures
meta.data=metatable2[which(metatable2[,1]%in%rownames(ASVtable3)),]
meta.div=data.frame(meta.data, richness, shannon, simpson, evenness)
meta.div2=meta.div[-which(meta.div$type=='sludge'),]

# summary
Div.summary=ddply(meta.div2, .(Sample_Location, Date_sampled), summarize, count=length(richness), meanR=mean(richness), sdR=sd(richness), meanShann=mean(shannon), sdShan=sd(shannon), meanSimp=mean(simpson), sdSimp=sd(simpson), meanE=mean(evenness), sdE=sd(evenness))
Div.summary

## Add env:
meta1=summary_table[which(summary_table$sampleID  %in%  meta.div2$sampleID),]
meta2=left_join(meta1, meta.div2, by=c('Location', 'Date'))

### saved version - with checked pond names
env.summary.table=read.csv('input_data/TableDeltaNH3_Div.summary.211.csv', header=TRUE)
env.summary1=env.summary.table[,-1]
dim(env.summary1)
