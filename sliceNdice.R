sliceNdice <- function(sid, func, var, type = 'combine' , cohorts = NULL){
  # only use the cohorts where we know we have this variable
  # varmap is a global list in the forked process
 if(!is.null(cohorts) && length(cohorts) == 1){
    cohorts <- strsplit(cohorts, ',\\s*')[[1]]
  }


  if(!is.null(varmap[[var]]$cohorts)){
    if(is.null(cohorts)){
      cohorts <- varmap[[var]]$cohorts
    } else {
    #  cohorts <- strsplit(cohorts, ',\\s*')[[1]]
      cohorts <- intersect(cohorts, varmap[[var]]$cohorts)
    }
  } else {
    stop('This variable is not available in any cohort.')
  }

  op_pre <-opals[cohorts]
############## important for all functions!!!! #################
  restricted  <- restrictToRelevantCols(var, sid, 'working_set', datasources = op_pre)
  dfName <- restricted$dfName
  op <- restricted$availableOpals
  on.exit(
    try(datashield.rm(op_pre, dfName))
  )

##################################################################
  if(varmap[[var]]$type == 'number'){
  #  var = paste0('working_set$', var)
    var = paste0(dfName, '$', var)
  symb <- datashield.symbols(op)
    ret <- tryCatch(do.call(func, list(var,type = type ,datasources = op)), error = function(e){
  	list(breaks = 0 , counts = 0 , mids = 0, density =0,xname = 'xvect', equidist = TRUE)				
    })
    if(type == 'split'){
      if(length(names(op)) == 1){
        ret <- list(ret)
      }
      names(ret) <- names(op)
    } else {
      ret <- list(global = ret)
    }
    cls <- sapply(ret, class) %>% Reduce(union, .)
    if('histogram' %in% cls){  # toJSON doesn't like this
      ret <- sapply(ret, unclass, simplify = FALSE)
    }
  } else if(varmap[[var]]$type == 'nominal'){
    # var = paste0('working_set$', var)
    var = paste0(dfName,'$', var)

    ret <- dssTable(var, type = type, datasources = op)

  ret <- sapply(ret, function(x){
      t <- sum(x)
      y <- as.list(x)
      y$Total <- t
      y
    }, simplify = FALSE)
 #   tbl <- dssTable(var, type = type, datasources = op)
 #   ret <- list()
 #   ret$breaks <- names(tbl$global)
 #   ret$counts <- as.vector(tbls$global)
  } else {
    stop(paste0('Not implemented for type ',varmap[[var]]$type ))
  }

  ret
  
}
