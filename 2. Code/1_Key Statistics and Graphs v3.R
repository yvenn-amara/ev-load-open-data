##### 1. Importing functions libraries and data #####
library(tidyverse)
library(mgcv)
library(ggplot2)
library(RColorBrewer)
setwd("C:/Users/yvenn/OneDrive/Bureau/Thèse/10. Articles/3. GitHub/EV-Load-Open-Data/2. Code") # Change this line to set Working directory to source file location
source("0_Utility_v2.R")
Sys.setlocale("LC_ALL","English")

# City of Boulder
raw_boulder = read.csv("../1. Data/3. Boulder/ev_chargingstationdata_Boulder_March 2020.csv", sep=";")
boulder = boulder_prep(raw_boulder)

nrow(raw_boulder) - nrow(boulder) # 1,659 transactions lost
100*(nrow(raw_boulder) - nrow(boulder))/nrow(raw_boulder) # 8.42% transactions lost
boulder_weekdays = boulder %>% filter(Weekend == 0)
boulder_weekends =  boulder %>% filter(Weekend == 1)

# City of Palo Alto
raw_palo_alto = read.csv("../1. Data/4. City of Palo Alto/temp_4046898750647841845.csv", sep=",")
palo_alto=palo_alto_prep(raw_palo_alto)

nrow(raw_palo_alto) - nrow(palo_alto) # 230 transactions lost
100*(nrow(raw_palo_alto) - nrow(palo_alto))/nrow(raw_palo_alto) # 0.17% transactions lost

palo_alto_weekdays = palo_alto %>% filter(Weekend == 0)
palo_alto_weekends =  palo_alto %>% filter(Weekend == 1)

# City of Dundee
dundee_files = list.files(path='../1. Data/1. Dundee/', pattern="*.csv", full.names=TRUE)
dundee_list = lapply(dundee_files, read.csv, na.strings = "")
dundee = dundee_prep(dundee_list) 

nrow(bind_rows(dundee_list)) - nrow(dundee) # 5,701 transactions lost # Note: 278 rows with NAs (most likely End.Time and Total kWh); We lose a total of 5,178 transcations with data quality checks
100*(nrow(bind_rows(dundee_list)) - nrow(dundee))/nrow(bind_rows(dundee_list)) # 10.81% transactions lost

dundee_weekdays = dundee %>% filter(Weekend == 0)
dundee_weekends =  dundee %>% filter(Weekend == 1)

# Cities of Perth and Kinross
perth_files = list.files(path='../1. Data/2. Perth City/', pattern="*.csv", full.names=TRUE)
raw_perth = bind_rows(lapply(perth_files, read.csv))
perth = prep_perth(raw_perth) 

nrow(raw_perth) - nrow(perth) # 2,728 transactions lost # We lose 185 transactions from na.omit due to the Total.kWh field missing values; A total of 1444 rows with filtering for End after Start and 1965 in total when fitering for durations of less than 2 days; A total of 2683 rows when filtering for positive Energy
100*(nrow(raw_perth) - nrow(perth))/nrow(raw_perth) # 4.09% transactions lost

perth_weekdays = perth %>% filter(Weekend == 0)
perth_weekends =  perth %>% filter(Weekend == 1)

# City of Paris
paris_files = list.files(path='../1. Data/5. Belib/', pattern="transactions*", full.names=TRUE)
raw_paris = bind_rows(lapply(paris_files, read.csv, sep=';'))

paris = prep_paris(raw_paris) 

nrow(raw_paris) - nrow(paris) # 963 transactions lost
100*(nrow(raw_paris) - nrow(paris))/nrow(raw_paris) # 14.28% transactions lost

paris_weekdays = paris %>% filter(Weekend == 0)
paris_weekends =  paris %>% filter(Weekend == 1)

# Domestics UK
raw_domestics = read.csv("../1. Data/6. Chargepoint Analysis/Domestics.csv")
domestics = prep_domestics(raw_domestics)

nrow(raw_domestics) - nrow(domestics) # 220 605 transactions lost
100*(nrow(raw_domestics) - nrow(domestics))/nrow(raw_domestics) # 6.94% transactions lost

domestics_weekdays = domestics %>% filter(Weekend == 0)
domestics_weekends =  domestics %>% filter(Weekend == 1)

##### 2. Key Statistics #####

overall_stats(boulder,boulder_weekdays,boulder_weekends) 
overall_stats(palo_alto,palo_alto_weekdays,palo_alto_weekends)
overall_stats(dundee,dundee_weekdays,dundee_weekends)
overall_stats(perth,perth_weekdays,perth_weekends)
overall_stats(paris,paris_weekdays,paris_weekends)
overall_stats(domestics,domestics_weekdays,domestics_weekends)

##### 3. Key Histograms #####

myColors <- brewer.pal(8,"Dark2")
names(myColors) <- list("Boulder","Palo Alto","Dundee","Perth","Paris","Domestics UK","Weekdays","Weekends")
colScale <- scale_colour_manual(name = "Data Set",values = myColors, aesthetics = c("colour", "fill"))

all_histograms_Charge_Duration(list(boulder,palo_alto),list("Boulder","Palo Alto"), colScale)
all_histograms_Park_Duration(list(palo_alto,dundee,perth,paris,domestics),list("Palo Alto","Dundee","Perth","Paris","Domestics UK"), colScale)
all_histograms_Energy(list(boulder,palo_alto,dundee,perth,paris,domestics),list("Boulder","Palo Alto","Dundee","Perth","Paris","Domestics UK"), colScale)

##### 3. Average Daily Plug-ins Profiles #####

all_plug_profile(list(boulder_weekdays,palo_alto_weekdays,dundee_weekdays,perth_weekdays,paris_weekdays), list('Boulder','Palo Alto','Dundee','Perth','Paris'), colScale)
all_plug_profile(list(boulder_weekends,palo_alto_weekends,dundee_weekends,perth_weekends,paris_weekends), list('Boulder','Palo Alto','Dundee','Perth','Paris'), colScale)
all_plug_profile(list(boulder_weekdays,palo_alto_weekdays,dundee_weekdays,perth_weekdays,paris_weekdays), list('Boulder','Palo Alto','Dundee','Perth','Paris'), colScale, column='end')
all_plug_profile(list(boulder_weekends,palo_alto_weekends,dundee_weekends,perth_weekends,paris_weekends), list('Boulder','Palo Alto','Dundee','Perth','Paris'), colScale, column='end')

colScale = scale_colour_manual(name = "Domestics UK",values = myColors, aesthetics = c("colour", "fill"))

all_plug_profile(list(domestics_weekdays,domestics_weekends), list("Weekdays","Weekends"), colScale)
all_plug_profile(list(domestics_weekdays,domestics_weekends), list("Weekdays","Weekends"), colScale, column='end')


##### 4. Trend in daily plug-ins over time #####
plug_trend(boulder)
plug_trend(palo_alto)
plug_trend(dundee)
plug_trend(perth)
plug_trend(paris)
plug_trend(domestics)

##### Appendix: Fitting Statistical Distributions #####

# Boulder
distr(boulder$Energy) # gamma
distr(boulder$Charge.Duration/(60)) # gamma

# Palo Alto
distr(palo_alto$Energy) # weibull
distr(palo_alto$Charge.Duration/(60)) # weibull
distr(palo_alto$Park.Duration/(60)) # gamma

# Dundee
distr(dundee$Energy) # weibull
distr(dundee$Park.Duration/(60)) # lognormal

# Perth
distr(perth$Energy) # weibull
distr(perth$Park.Duration/(60)) # weibull

# Paris
distr(paris$Energy) # weibull
distr(paris$Park.Duration/(60)) # lognormal

# Domestics UK
distr(domestics$Energy) # gamma
distr(sample(domestics$Park.Duration/(60),1000000)) # weibull - here we take a subsample of the data for optimisation purposes
