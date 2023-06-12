library(jsonlite)
library(RestRserve)
library(magrittr)

loadApp <- function(){
  app <-  Application$new(middleware = list(CORSMiddleware$new()), content_type = "application/json")
  #app <-  Application$new( content_type = "application/json")
  
  # load the enpoint descriptions in a sandbox env:
  temp <- new.env()
  for(p in list.files(config$endpointDir, full.names = TRUE)){
    source(p, local = temp)
  }
  
  # and attach them to the app (name of endpoint in the file must match the path)
  for(endPath in ls(temp)){
    endp <- get(endPath, temp)
    endp$path <- paste0('/', endPath)
    do.call(app$add_route, endp)
  }
  rm(temp)
  return(app)
}



confFile <- '../config/config.json'
config <- readChar(confFile, file.info(confFile)$size) %>%
            gsub('(?<=:)\\s+|(?<=\\{)\\s+|(?<=,)\\s+|(?<=\\[)\\s+','',., perl = TRUE) %>%
            fromJSON()


##### load and start the app:
app <- loadApp()

backend <- BackendRserve$new()
backend$start(app, http_port = config$port)
