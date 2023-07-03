library(jsonlite)
library(RestRserve)
library(magrittr)

MyCORSMw <- R6::R6Class(
  classname = "MyCORSMw",
  inherit = CORSMiddleware, 
  public = list(
    initialize = function(routes = "/", match = "partial", id = "CORSMiddleware") {
      checkmate::assert_character(routes, pattern = "^/")
      checkmate::assert_subset(match, c("exact", "partial"))
      checkmate::assert_string(id, min.chars = 1L)
      
      if (length(match) == 1L) {
        match = rep(match, length(routes))
      }
      if (length(routes) != length(match)) {
        stop("length 'match' must be 1 or equal length 'routes'")
      }
      
      self$id = id
      
      self$process_response = function(request, response) {
        prefixes_mask = match == "partial"
        if ((request$path %in% routes[!prefixes_mask]) ||
            any(startsWith(request$path, routes[prefixes_mask])) &&
            # response successful
            response$status_code < 300) {
          response$set_header("Access-Control-Allow-Origin", response$get_header("Access-Control-Allow-Origin", "*"))
          response$set_header("Access-Control-Allow-Methods", "POST, GET, OPTIONS, PUT, DELETE, PATCH")
          response$set_header("Access-Control-Allow-Headers", "Origin, Content-Type, Accept, Authorization, X-Request-With")
          response$set_header("Access-Control-Allow-Credentials", "true")
        }
        invisible(TRUE)
      }
      
      self$process_request = function(request, response) {
        invisible(TRUE)
      }
    }
  )
    
)



loadApp <- function(){
  app <-  Application$new(middleware = list(MyCORSMw$new()), content_type = "application/json")
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

################## debug ##############
#req = Request$new(
#  path = "/getvars",
#  headers = list("Access-Control-Request-Method" = "GET"),
#  method = "GET"
#)

 #x <- app$process_request(req)
######################################
backend <- BackendRserve$new()
backend$start(app, http_port = config$port)
