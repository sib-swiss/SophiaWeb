appLogin <- function(config, pass){

  logindata <- config$loginData
  resourceMap <- config$resourceMap
  # finalise logindata

  logindata$user <-config$appUser
  logindata$password <- pass

  
  logindata$driver <- 'OpalDriver'
  ######### make one logindata entry per resource (as opposed to one per server - we'll have one or more connections per server) ###########
  resnames <- dssSwapKeys(resourceMap)

  logindata <- lapply(names(resnames), function(x){
    out <- logindata[logindata$server == resnames[[x]],,drop=FALSE]
    out$server <- x
    out
  }) %>% Reduce(rbind,.)
  ##################################################
  ######################### login where allowed, fail silently elsewhere #################################
  opals <- list()
  prev.url <- ''
  for(i in logindata$server){
    this.url <- logindata[logindata$server == i,'url']
    if(this.url == prev.url){
	    cat(paste0('Waiting a bit to avoid spooking the server at ', unname(this.url), "\n"))
    	    Sys.sleep(1)
    }
    prev.url <- this.url
    cat(paste0('Connection to ', unname(logindata[logindata$server == i,'server']),"\n"))
    
    #try(opals[i] <- datashield.login(logindata[logindata$server == i,,drop = FALSE]), silent = FALSE)
    tryCatch(opals[i] <- datashield.login(logindata[logindata$server == i,,drop = FALSE]), error = function(e){
      datashield.logout(opals)
      stop(e)
    })
    #opals[i] <- datashield.login(logindata[logindata$server == i,,drop = FALSE])
  }
  #### sanitize and save logindata in the environment for later user logins
  logindata$password <- NULL
  logindata$user <- NULL
  assign('logindata', logindata, envir = .GlobalEnv)
  ####### opals in the environment

  return(opals)
}
