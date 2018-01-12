rm(list = ls())

# Load the OCE package
library(oce)
library(ocedata)
library(sea)
library(xtable)
data(coastlineWorldFine)

# Set the master (source) and output folders for the data
cruiseID <- 'S275'

rootFold <- file.path("~/Documents/SEA",cruiseID,"cruiseReport")
foldship <- file.path(rootFold,"data","SHIPDATA")
saveLoc <- file.path(rootFold,"tables")

# Station Summary
filename <- file.path(foldship,paste0(cruiseID,"_Station Summary.xlsx"))
tableStatSum(filename,saveLoc=saveLoc)

# Hydrocasts
filename <- file.path(foldship,paste0(cruiseID,"_Hydrowork.xlsm"))
tableHydro(filename,saveLoc=saveLoc)

# CTD
filename <- file.path(foldship,paste0(cruiseID,"_Ctdwork.xlsm"))
tableCTD(filename,saveLoc=saveLoc)

# SS
filename <- file.path(foldship,paste0(cruiseID,"_surfsamp.xlsm"))
tableSS(filename,saveLoc=saveLoc)

# Neuston
filename <- file.path(foldship,paste0(cruiseID,"_Neuston.xlsm"))
tableNeuston(filename,saveLoc=saveLoc)

# 100 Count
filename <- file.path(foldship,paste0(cruiseID,"_Neuston.xlsm"))
table100Count(filename,saveLoc=saveLoc)
