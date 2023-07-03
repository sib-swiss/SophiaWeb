library(dsSwissKnifeClient) #dssSomething
library(dsQueryLibrary) # dsqSomething
library(dsResource)   ### dsrSomething
library(dsBaseClient) ### ds.something
library(magrittr)
library(jsonlite)
setwd('/mnt/shareddisk/datashield/SophiaWeb/')
confFile <- './config.json'
config <- readChar(confFile, file.info(confFile)$size) %>%
  gsub('(?<=:)\\s+|(?<=\\{)\\s+|(?<=,)\\s+|(?<=\\[)\\s+','',., perl = TRUE) %>%
  fromJSON()
opals <- appLogin(config, '3xC@libur')
x <- prepareData(opals)
