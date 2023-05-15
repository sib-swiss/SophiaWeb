library(dsSwissKnifeClient) #dssSomething
library(dsQueryLibrary) # dsqSomething
library(dsResource)   ### dsrSomething
library(dsBaseClient) ### ds.something
library(magrittr)
library(jsonlite)
setwd('/mnt/shareddisk/datashield/SophiaWeb/')
source('appLogin.R')
source('prepareData.R')
source('oneVarStats.R')
confFile <- './config.json'
config <- readChar(confFile, file.info(confFile)$size) %>%
  gsub('(?<=:)\\s+|(?<=\\{)\\s+|(?<=,)\\s+|(?<=\\[)\\s+','',., perl = TRUE) %>%
  fromJSON()
opals <- appLogin(config, '3xC@libur')
varList <- prepareData(opals)
write(toJSON(varList),'data/vars.json')

datashield.symbols(opals)


sts <- sapply(names(varmap), function(x){
  if(varmap[[x]]$type == 'number'){
    sapply(varmap[[x]]$cohorts, function(y){
      oneVarStats(x,opals[y])
    }, simplify = FALSE)
  }
}, simplify = FALSE) %>% dssSwapKeys

sapply(names(sts), function(x){
  if(!dir.exists(paste0('./data/',x))){
    dir.create(paste0('./data/',x))
  }
  sapply(names(sts[[x]]), function(y){
    print(y)
    write(serializeJSON(sts[[x]][[y]]), paste0('./data/',x,'/',y))
  })
})
