##### 1. Importing functions libraries #####
library(tidyverse)
library(fitdistrplus)
setwd("C:/Users/yvenn/OneDrive/Bureau/Thèse/10. Articles/3. GitHub/EV-Load-Open-Data/") # Change this line to set Working directory to source file location
source("2. Scripts/0_Utility_v2.R")
source("2. Scripts/1_Data_Prep.R")

#Arrival
print("====== 1. Arrival ======")
print("Boulder")
distr(boulder$Arrival)
print("Palo Alto")
distr(palo_alto$Arrival)
print("Dundee")
distr(dundee$Arrival)
print("Perth")
distr(perth$Arrival)
print("Paris")
distr(paris$Arrival)
print("Domestics UK")
distr(domestics$Arrival)
print("ACN")
distr(acn$Arrival)
print("SAP")
distr(sap$Arrival)

#Charge.Duration
print("====== 2. Charge.Duration ======")
print("Boulder")
distr(boulder$Charge.Duration)
print("Palo Alto")
distr(palo_alto$Charge.Duration)
print("ACN")
distr(acn$Charge.Duration/10) #scaling to fix boundary issues
print("SAP")
distr(sap$Charge.Duration)

#Park.Duration
print("====== 3. Park.Duration ======")
print("Boulder")
distr(boulder$Park.Duration)
print("Palo Alto")
distr(palo_alto$Park.Duration/10) #scaling to fix boundary issues
print("Dundee")
distr(dundee$Park.Duration)
print("Perth")
distr(perth$Park.Duration)
print("Paris")
distr(paris$Park.Duration)
print("Domestics UK")
distr(domestics$Park.Duration/10) #scaling to fix boundary issues
print("ACN")
distr(acn$Park.Duration/10) #scaling to fix boundary issues 
print("SAP")
distr(sap$Park.Duration/10) #scaling to fix boundary issues

#Energy
print("====== 4. Energy ======")
print("Boulder")
distr(boulder$Energy)
print("Palo Alto")
distr(palo_alto$Energy)
print("Dundee")
distr(dundee$Energy)
print("Perth")
distr(perth$Energy)
print("Paris")
distr(paris$Energy)
print("Domestics UK")
distr(domestics$Energy)
print("ACN")
distr(acn$Energy)
print("SAP")
distr(sap$Energy)
