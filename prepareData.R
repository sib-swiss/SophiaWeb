 prepareData <- function(opals){

  #first the resources:
  sapply(names(opals), function(res){
    datashield.assign.resource(opals[res], sub('.','_',res, fixed = TRUE), res, async = TRUE)
  })
  # load the 2 data frames
  ################ !!!!!!!!!!!!!!!!!!!!!! ############### only for development!!!!!
  ##########################################
  dsqLoad(symbol= 'measurement',
                   domain = 'concept_name',
                   query_name = 'measurement',
                   where_clause = 'value_as_number is not null and measurement_date is not null and measurement_type_concept_id not in (268435456, 268435457, 268435458)',
                  # row_limit =  6000000,
                  #  row_limit =  1000000, ## dev only
                   union = TRUE,
                   datasources = opals)
  dsqLoad(symbol= 'person',
          domain = 'concept_name',
          query_name = 'person',
          union = TRUE,
          datasources = opals)
 


  # fix funky measurement dates:

  #dssSubset('measurement', 'measurement', row.filter = 'measurement_date >= "01-01-1970"', datasources = opals)
  dssSubset('measurement', 'measurement', row.filter = 'as.Date(measurement_date) >= as.Date("01-01-1970")', datasources = opals)

  ############## calculate age #####################
  # order by measurement date for each person_id
  dssSubset('measurement', 'measurement', 'order(person_id, measurement_date)', async = TRUE, datasources = opals)
  #  measurement dates as numbers:
  dssDeriveColumn('measurement', 'measurement_date_n', 'as.numeric(as.Date(measurement_date))', datasources = opals)
  # add a dummy column just for the widening formula, this will hold eventually the 'aggregate' first measurement date
  dssDeriveColumn('measurement', 'f', '"irst_measurement_dat.e"', datasources = opals)
  # now we can widen by that column and pick the first value:
  dssPivot(symbol = 'first_m_dates', what ='measurement', value.var = 'measurement_date_n',
           formula = 'person_id ~ f',
           by.col = 'person_id',
           fun.aggregate = function(x)x[1], # we are sure it's the first date, baseline, they've been ordered
           async = TRUE,
           datasources = opals)

  dssJoin(c('person', 'first_m_dates'), symbol= 'person', by = 'person_id', join.type = 'inner', datasources = opals)
  try(datashield.rm(opals, 'first_m_dates'), silent = FALSE) # keep it slim
  # now calculate the age at first measurement:
  dssDeriveColumn('person', 'age', 'round((f.irst_measurement_dat.e - as.numeric(as.Date(birth_datetime)))/365)', datasources = opals)

  ###################  finished with age ##########################################

  dssPivot(symbol = 'wide_m', what ='measurement', value.var = 'value_as_number',
           formula = 'person_id ~ measurement_name',
           by.col = 'person_id',
           fun.aggregate = function(x)x[1], # maybe we'll want mean here?
           datasources = opals)
  try(datashield.rm(opals, 'measurement'), silent = FALSE)
  dssJoin(what = c('wide_m', 'person'),
          symbol = 'working_set',
          by = 'person_id',
          datasources = opals)
  dssSubset('working_set', 'working_set',
            col.filter = 'setdiff(colnames(working_set), c("database", "f.irst_measurement_dat.e", "birth_datetime" , "location_id", "provider_id" , "care_site_id", "person_id")) ',
            datasources = opals) # get rid of superfluous columns
  try(datashield.rm(opals, 'person'), silent = TRUE)
  try(datashield.rm(opals, 'wide_m'), silent = TRUE)
  #### fix column names:
  n <- dssColNames('working_set', datasources = opals)
  sapply(names(n), function(x){
    cnames <- n[[x]]
    if(x == 'tayside_fife_t1t2.hic_sophia'){
      #cnames <- cnames[c(1:210, 343:346)]
      cnames <- cnames[c(1:210, (length(cnames)-3):length(cnames))]
      dssSubset('working_set', 'working_set', col.filter = 'c(1:210, (length(colnames(working_set)) - 3):length(colnames(working_set)))', datasources = opals[[x]])
    }
    cnames <- sub('measurement_name.', '', cnames, fixed = TRUE)
    
    dssColNames('working_set', cnames, datasources = opals[[x]])

  })

  # create varmap:
  n <- dssColNames('working_set', datasources = opals)



  varsToCohorts <- dssSwapKeys(n)
  varsToCohorts <- sapply(varsToCohorts, function(a){
    list(type = 'number', cohorts = a)
  }, simplify = FALSE)
  varsToCohorts[c('ethnicity', 'race', 'gender')] <- sapply(c('ethnicity', 'race', 'gender'), function(a){
    l <- ds.levels(paste0('working_set$',a), datasources = opals)
    enum <- sapply(l, '[[', 'Levels', simplify = FALSE) %>% Reduce(union,.) %>% lapply(function(en)list(value = en, label = en)) # maybe we'll want per cohort at some point?
    list(type = 'nominal', cohorts = varsToCohorts[[a]]$cohorts, enumerations = enum)
  }, simplify = FALSE)
  
#  sw <- dssShowFactors('working_set', datasources = opals) %>%  dssSwapKeys

#  varsToCohorts[c('ethnicity', 'race', 'gender')]  <- sapply(names(sw), function(a){
#    enum <- Reduce(union,sw[[a]]) %>% lapply(function(en)list(value = en, label = en)) # maybe we'll want per cohort at some point?
#    list(type = 'nominal', cohorts = varsToCohorts[[a]]$cohorts, enumerations = enum)
#  }, simplify = FALSE)

  ##### create the json for the bubbles:
  bubble <- list()
  bubble$datasets <- lapply(names(n), function(x) list(id = x, label = x))
  bubble$groups <- list(
    list(id = 'measurement',
         label = 'measurement',
         variables = sapply(n, function(x) setdiff(x, c('age', 'race', 'ethnicity', 'gender')), simplify = FALSE)
         ),
    list( id = 'demographics',
          label = 'demographics',
          variables = c('age','ethnicity', 'race', 'gender'))
   )
  bubble$rootGroup <- list(id = 'root',
                    label = 'Root Group',
                    groups = c('demographics', 'measurement')
  )
  bubble$variables <- lapply(names(varsToCohorts), function(x){
    thisone <- varsToCohorts[[x]]
    out <- thisone[setdiff(names(thisone), 'cohorts')]
    out$id <- x
    out
  })
  assign('varmap', varsToCohorts, envir = .GlobalEnv)
  assign('cnames', n, envir = .GlobalEnv)
  assign('bubble', bubble, envir = .GlobalEnv)
}
