library(dsSwissKnifeClient) #dssSomething
library(dsQueryLibrary) # dsqSomething
library(dsResource)   ### dsrSomething
library(dsBaseClient) ### ds.something
library(magrittr)
library(jsonlite)
setwd('/mnt/shareddisk/datashield/SophiaWeb/src')
source('appLogin.R')
source('prepareData.R')
source('oneVarStats.R')
confFile <- '../config/config.one.json'
config <- readChar(confFile, file.info(confFile)$size) %>%
  gsub('(?<=:)\\s+|(?<=\\{)\\s+|(?<=,)\\s+|(?<=\\[)\\s+','',., perl = TRUE) %>%
  fromJSON()
source('/mnt/shareddisk/Sophia/super_secret.R')
opals <- appLogin(config,  getOption('datashield.password'))
varList <-tryCatch(prepareData(opals), error = function(e){
  datashield.logout(opals)
  stop(e)
})
write(toJSON(varList),'../data/vars.json')

datashield.symbols(opals)


sts <- sapply(names(varmap), function(x){
  if(varmap[[x]]$type == 'number'){
    sapply(varmap[[x]]$cohorts, function(y){
      tryCatch(oneVarStats(x,opals[y]), error = function(e){
        datashield.errors()
        datashield.logout(opals[y])
      })
    }, simplify = FALSE)
  }
}, simplify = FALSE) %>% dssSwapKeys

sapply(names(sts), function(x){
  if(!dir.exists(paste0('../data/',x))){
    dir.create(paste0('../data/',x))
  }
  sapply(names(sts[[x]]), function(y){
    print(y)
    write(serializeJSON(sts[[x]][[y]]), paste0('../data/',x,'/',y))
  })
})
datashield.logout(opals)
