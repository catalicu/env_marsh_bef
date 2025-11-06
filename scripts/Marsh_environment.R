# Title: 'Marsh Environment'
# Author: 'Dr CG, from SGL thesis scripts'
# Date last edits: '10/30/30'

# Description: This script processes data tables to facilitate further analyses.
# It generates the following data tables:
# *


# Libraries
library(ggplot2)
library(vegan)
library(plyr)
library(dplyr)
library(lubridate)

# Plot themes
## With legend
Theme=theme_classic(base_size=11, base_family="Helvetica") +
  theme(axis.line = element_line(linewidth = 1, colour = "black", linetype = "solid")) +theme(plot.title = element_text(size = 12))
## Without legends
Theme2=Theme+ theme(legend.position="none") + theme(panel.border=element_rect(fill=NA))

# load data
setwd('..') # set writing directory to script location and then go one folder up

## Environmental data
env.table=read.csv('input_data/Envdata_marshstudy.csv', header=TRUE)
    env.table2=env.table[-which(duplicated(env.table$sampleID)),]

# Edits and data processing
## summary environmental: 
### removed sludge data
env.Slu=env.table[which(env.table$type=='sludge'),]
summary_table=env.table[-which(env.table$type=='sludge'),]

# convert dates to seasons
summary_table$Season=summary_table$Date
unique(summary_table$Date)

summary_table[c(which(summary_table$Date=='10/3/2019'),
                 which(summary_table$Date=='10/5/2019'),
                 which(summary_table$Date=='10/20/2019')),
               which(colnames(summary_table)=='Season')]='Autumn'

summary_table[c(which(summary_table$Date=='11/3/2019'),
                 which(summary_table$Date=='11/8/2019'),
                 which(summary_table$Date=='12/9/2019')),
               which(colnames(summary_table)=='Season')]='Winter'

summary_table[c(which(summary_table$Date=='1/9/2020'),
                 which(summary_table$Date=='1/17/2020'),
                 which(summary_table$Date=='1/10/2020')),
               which(colnames(summary_table)=='Season')]='Spring'

### Format dates
#### Convert Date column to Date type
summary_table$Date2 <- mdy(summary_table$Date)

### location
### add names
summary_table$Location_names=summary_table$Location
unique(summary_table$Location)

### summarize the multisamples within oxponds
summary_table[c(which(summary_table$Location=='Pond1.1'),
                which(summary_table$Location=='Pond1.2'),
                which(summary_table$Location=='Pond1.3'),
                which(summary_table$Location=='Pond1.4'),
                which(summary_table$Location=='Pond1.5')),
                which(colnames(summary_table)=='Location_names')]='Oxidation_Pond1'

summary_table[c(which(summary_table$Location=='Pond2.1'),
                which(summary_table$Location=='Pond2.2'),
                which(summary_table$Location=='Pond2.3'),
                which(summary_table$Location=='Pond2.4'),
                which(summary_table$Location=='Pond2.5')),
                which(colnames(summary_table)=='Location_names')]='Oxidation_Pond2'

summary_table[c(which(summary_table$Location=='Point9')),
                which(colnames(summary_table)=='Location_names')]='TreatmentMarshes'

summary_table[c(which(summary_table$Location=='Point11')),
                which(colnames(summary_table)=='Location_names')]='EW_Influent'

summary_table[c(which(summary_table$Location=='Point12')),
                which(colnames(summary_table)=='Location_names')]='Allen'

summary_table[c(which(summary_table$Location=='Point14')),
                which(colnames(summary_table)=='Location_names')]='Gearheart'

summary_table[c(which(summary_table$Location=='Point15')),
                which(colnames(summary_table)=='Location_names')]='Hauser'

summary_table[c(which(summary_table$Location=='Point16')),
                which(colnames(summary_table)=='Location_names')]='BayDischarge'

summary_table$Location_names=factor(summary_table$Location_names, 
                                    levels=c('Oxidation_Pond1', 'Oxidation_Pond2', 
                                             'TreatmentMarshes',
                                             'EW_Influent', 'Allen', 
                                             'Gearheart','Hauser', 
                                             'BayDischarge'))

### calculate average of replicate samples per pond/per date
summary_table2.location.date<- summary_table %>%
  group_by(sampleID,Location_names, Season, Date2) %>%
  summarise(
    Ammonia = mean(`Ammonia..mg.L.`, na.rm = TRUE),
    BOD = mean(BOD, na.rm = TRUE),
    Temperature = mean(`Water.Temp...C.`, na.rm = TRUE),
    Precipitation = mean(`Percipitation..in.`, na.rm = TRUE),
    count=n(),
    .groups = 'drop')

### calculate average per date
summary_table3.date <- summary_table %>%
  group_by(Date2) %>%
  summarise(
    Ammonia = mean(`Ammonia..mg.L.`, na.rm = TRUE),
    BOD = mean(BOD, na.rm = TRUE),
    DO = mean(DO, na.rm=TRUE),
    Temperature = mean(`Water.Temp...C.`, na.rm = TRUE),
    Precipitation = mean(`Percipitation..in.`, na.rm = TRUE),
    count=n(),
    .groups = 'drop')

dim(summary_table2.location.date)
dim(summary_table3.date)

# save
outPATH='output_tables/'
date=Sys.Date()
env.path = paste(outPATH,'env_table', date, sep='')

write.table(summary_table2.location.date, paste(env.path, '_summary_date_location', '.txt', sep=''), sep="\t") 
write.table(summary_table3.date, paste(env.path, '_summary_date', '.txt', sep=''), sep="\t") 
write.table(summary_table, paste(env.path,'_processed', '.txt', sep=''), sep="\t") 

# Evaluate temporal trends
## Water temperature
colnames(summary_table)
summary(lm(Temperature ~ Date2, data=summary_table3.date))

date_temp=ggplot(data=summary_table3.date, aes(x = Date2, y = Temperature)) +
  geom_point() +
  scale_x_date(date_labels = "%b %d, %Y") +  
  labs(x = "Date", y = "Water Temperature (C)") +
  Theme + geom_smooth(method='lm', se=FALSE, color='black', linetype='dashed')

## Precipitation 
colnames(summary_table)
summary(lm(Precipitation~ Date2, data=summary_table3.date))
date_prec=ggplot(data=summary_table3.date, aes(x = Date2, y = Precipitation)) +
  geom_point() +
  scale_x_date(date_labels = "%b %d, %Y") +  
  labs(x = "Date", y = "Precipitation (in)") +
  Theme 


# Calculate correlations
## water temperature and precipitation
colnames(summary_table2)
summary(lm(Precipitation ~ Temperature, data=summary_table3.date))
temp.prec=ggplot(aes(Temperature, Precipitation, ), data=summary_table3.date) + geom_point() +
  Theme2

## air and water temperatures
colnames(summary_table)
summary(lm(Water.Temp...C. ~ Outside.Temp...C., data=summary_table))
temp_out.in=ggplot(aes(Outside.Temp...C., Water.Temp...C.), data=summary_table) + geom_jitter() +
  Theme2 + geom_smooth(method='lm', color='black', se=FALSE, linetype='dashed') +
  ylab('Water Temperature (C)') + xlab('Air Temperature (C)')

## water temperature and DO
colnames(summary_table)
summary(glm(DO ~ Temperature, data=summary_table3.date))
temp.do=ggplot(aes(Temperature, DO), data=summary_table3.date) + geom_point() +
  Theme2 

## water temperature and BOD
colnames(summary_table)
summary(glm(BOD ~ Temperature, data=summary_table3.date))
temp_BOD=ggplot(aes(Temperature, BOD), data=summary_table3.date) + geom_jitter() +
  Theme2 

## precipitation and BOD
colnames(summary_table)
summary(glm(BOD ~ Precipitation, data=summary_table3.date))
prec.BOD=ggplot(aes(Precipitation, BOD), data=summary_table3.date) + geom_jitter() +
  Theme2

# Summary plot
library(gridExtra)
summary.plots.env=arrangeGrob(date_prec, date_temp, 
            temp.prec, temp_out.in, 
            temp_BOD, prec.BOD)
plot(summary.plots.env)

