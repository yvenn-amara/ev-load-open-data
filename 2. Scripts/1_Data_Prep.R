##### 1. Importing functions libraries #####
library(tidyverse)
library(stringr)
setwd("C:/Users/yvenn/OneDrive/Bureau/Thèse/10. Articles/3. GitHub/EV-Load-Open-Data/") # Change this line to set Working directory to source file location
source("2. Scripts/0_Utility_v2.R")
Sys.setlocale("LC_ALL","English")

##### 2. Data Prep #####

##### City of Boulder #####
print("====== Boulder Prep ======")

if (file.exists("3. Output/boulder_2021.csv")) {
  print("Loading existing file")
  boulder = read.csv("3. Output/boulder_2021.csv") %>%
    mutate(Start = parse_date_time(Start, "%Y-%m-%d %H:%M:%S", tz="America/Denver"),
           End = parse_date_time(End, "%Y-%m-%d %H:%M:%S", tz="America/Denver"))
} else {
  print("Creating file")
  raw_boulder = read.csv("1. Input Data/3. Boulder/ev_chargingstationdata_Boulder_March 2021.csv", sep=",")
  boulder = boulder2_prep(raw_boulder)
  write.csv(boulder,"3. Output/boulder_2021.csv", row.names = FALSE)
}

quick_stats(raw_boulder,boulder)

##### City of Palo Alto #####
print("====== Palo Alto Prep ======")

if (file.exists("3. Output/palo_alto.csv")) {
  print("Loading existing file")
  palo_alto = read.csv("3. Output/palo_alto.csv") %>%
    mutate(Start = parse_date_time(Start, "%Y-%m-%d %H:%M:%S", tz="US/Pacific"),
           End = parse_date_time(End, "%Y-%m-%d %H:%M:%S", tz="US/Pacific"))
} else{
  print("Creating file")
  raw_palo_alto = read.csv("1. Input Data/4. City of Palo Alto/ChargePoint Data CY20Q4.csv", sep=",")
  palo_alto=palo_alto_prep(raw_palo_alto)
  write.csv(palo_alto,"3. Output/palo_alto.csv", row.names = FALSE)
}

quick_stats(raw_palo_alto,palo_alto)

##### City of Dundee #####
print("====== Dundee Prep ======")

if (file.exists("3. Output/dundee.csv")) {
  print("Loading existing file")
  dundee = read.csv("3. Output/dundee.csv") %>%
    mutate(Start = parse_date_time(Start, "%Y-%m-%d %H:%M:%S", tz="Europe/London"),
           End = parse_date_time(End, "%Y-%m-%d %H:%M:%S", tz="Europe/London"))
} else{
  print("Creating file")
  dundee_files = list.files(path='1. Input Data/1. Dundee/', pattern="*.csv", full.names=TRUE)
  dundee_list = lapply(dundee_files, read.csv, na.strings = "")
  dundee = dundee_prep(dundee_list) 
  write.csv(dundee,"3. Output/dundee.csv", row.names = FALSE)
}

# quick_stats(bind_rows(dundee_list),dundee)

##### Cities of Perth and Kinross #####
print("====== Perth & Kinross Prep ======")

if (file.exists("3. Output/perth.csv")) {
  print("Loading existing file")
  perth = read.csv("3. Output/perth.csv")%>%
    mutate(Start = parse_date_time(Start, "%Y-%m-%d %H:%M:%S", tz="Europe/London"),
           End = parse_date_time(End, "%Y-%m-%d %H:%M:%S", tz="Europe/London"))
} else{
  print("Creating file")
  perth_files = list.files(path='1. Input Data/2. Perth City/', pattern="*.csv", full.names=TRUE)
  raw_perth = bind_rows(lapply(perth_files, read.csv))
  perth = prep_perth(raw_perth) 
  write.csv(perth,"3. Output/perth.csv", row.names = FALSE)
}

quick_stats(raw_perth,perth)

##### City of Paris #####
print("====== Paris Prep ======")

if (file.exists("3. Output/paris.csv")) {
  print("Loading existing file")
  paris = read.csv("3. Output/paris.csv")%>%
    mutate(Start = parse_date_time(Start, "%Y-%m-%d %H:%M:%S", tz="Europe/Paris"),
           End = parse_date_time(End, "%Y-%m-%d %H:%M:%S", tz="Europe/Paris"))
} else{
  print("Creating file")
  paris_files = list.files(path='1. Input Data/5. Belib/', pattern="transactions*", full.names=TRUE)
  raw_paris = bind_rows(lapply(paris_files, read.csv, sep=';'))
  paris = prep_paris(raw_paris) 
  write.csv(paris,"3. Output/paris.csv", row.names = FALSE)
}
# quick_stats(raw_paris,paris)

##### Domestics UK ##### 
print("====== Domestics UK Prep ======")

if (file.exists("3. Output/domestics.csv")) {
  print("Loading existing file")
  domestics = read.csv("3. Output/domestics.csv") %>%
    mutate(Start = parse_date_time(Start, "%Y-%m-%d %H:%M:%S", tz="Europe/London"),
           End = parse_date_time(End, "%Y-%m-%d %H:%M:%S", tz="Europe/London"))
} else{
  print("Creating file")
  raw_domestics = read.csv("1. Input Data/6. Chargepoint Analysis/Domestics.csv")
  domestics = prep_domestics(raw_domestics)
  write.csv(domestics,"3. Output/domestics.csv", row.names = FALSE)
}
# quick_stats(raw_domestics,domestics)

##### ACN ##### 
print("====== ACN Prep ======")

## Caltech
print("1. Caltech")

if (file.exists("3. Output/caltech.csv")) {
  print("Loading existing file")
  caltech = read.csv("3. Output/caltech.csv") %>%
    mutate(Start = parse_date_time(Start, "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"),
           End = parse_date_time(End, "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"))
} else{
  print("Creating file")
  library(rjson)
  json_caltech = fromJSON(file="1. Input Data/7. ACN Data/Caltech.json")
  
  
  list_caltech = lapply(json_caltech$`_items`, to_char_names)
  raw_caltech = data.frame(matrix(unlist(list_caltech), nrow=length(list_caltech), byrow=T))
  names(raw_caltech) = names(json_caltech$`_items`[[1]])
  
  caltech = prep_acn(raw_caltech)
  write.csv(caltech,"3. Output/caltech.csv", row.names = FALSE)
}

## JPL
print("2. JPL")

if (file.exists("3. Output/jpl.csv")) {
  print("Loading existing file")
  jpl = read.csv("3. Output/jpl.csv") %>%
    mutate(Start = parse_date_time(Start, "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"),
           End = parse_date_time(End, "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"))
} else{
  print("Creating file")
  json_jpl = fromJSON(file="1. Input Data/7. ACN Data/JPL.json")
  
  list_jpl = lapply(json_jpl$`_items`, to_char_names)
  raw_jpl = data.frame(matrix(unlist(list_jpl), nrow=length(list_jpl), byrow=T))
  names(raw_jpl) = names(json_jpl$`_items`[[1]])
  
  jpl = prep_acn(raw_jpl)
  write.csv(jpl,"3. Output/jpl.csv", row.names = FALSE)
}

## Office 1
print("3. Office1")

if (file.exists("3. Output/office.csv")) {
  print("Loading existing file")
  office = read.csv("3. Output/office.csv") %>%
    mutate(Start = parse_date_time(Start, "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"),
           End = parse_date_time(End, "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"))
} else{
  print("Creating file")
  json_office = fromJSON(file="1. Input Data/7. ACN Data/Office 1.json")
  
  list_office = lapply(json_office$`_items`, to_char_names)
  raw_office = data.frame(matrix(unlist(list_office), nrow=length(list_office), byrow=T))
  names(raw_office) = names(json_office$`_items`[[1]])
  
  office = prep_acn(raw_office)
  write.csv(office,"3. Output/office.csv", row.names = FALSE)
}

print("Final ACN Concatenation")

if (file.exists("3. Output/acn.csv")) {
  print("Loading existing file")
  office = read.csv("3. Output/acn.csv") %>%
    mutate(Start = parse_date_time(Start, "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"),
           End = parse_date_time(End, "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"))
} else{
  print("Creating file")
  
  acn = rbind(caltech %>% mutate(data = "caltech"),
              jpl %>% mutate(data = "jpl"),
              office %>% mutate(data = "office"))
  write.csv(acn,"3. Output/acn.csv", row.names = FALSE)
}

# raw_rows = nrow(raw_caltech) + nrow(raw_jpl) + nrow(raw_office)
# print(paste('Transactions Retained: ',nrow(acn)," (",round(100*nrow(acn)/raw_rows,2), "%)", sep=""   ))
# print(paste('Transactions Discarded: ',raw_rows - nrow(acn)," (",round(100*(raw_rows - nrow(acn))/raw_rows,2), "%)", sep="" ))
# 
# print(paste('First Transaction:',min(date(acn$Start))))
# print(paste('Last Transaction:',max(date(acn$Start))))


##### SAP #####
print("====== SAP Prep ======")

if (file.exists("3. Output/sap.csv")) {
  print("Loading existing file")
  sap = read.csv("3. Output/sap.csv") %>%
    mutate(Start = parse_date_time(Start, "%Y-%m-%d %H:%M:%S", tz="Europe/Paris"),
           End = parse_date_time(End, "%Y-%m-%d %H:%M:%S", tz="Europe/Paris"))
} else{
  print("Creating file")
  raw_sap = read.csv("1. Input Data/8. SAP Labs France/conso-ve-sap0.csv", sep=';')
  sap = prep_sap(raw_sap)
  write.csv(sap,"3. Output/sap.csv", row.names = FALSE)
}

# quick_stats(raw_sap,sap)


