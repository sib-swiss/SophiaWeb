oneVarStats <- function(x, nodes){
  f <- substr(x,1,1)
  if(!is.na(as.numeric(f))){
    newx <- paste0('x.',x)
  } else {
    newx <- x
  }
  dssSubset(newx, 'working_set', col.filter = paste0('"',x,'"'), datasources = nodes)
  dssSubset(newx, newx, row.filter = paste0('complete.cases(',newx,')'), datasources = nodes)
  hist <- try(ds.histogram(paste0(newx,'$',x),datasources = nodes)) %>% unlist(recursive = FALSE)
  quants <- try(ds.quantileMean(paste0(newx,'$',x), datasources = nodes))%>% unlist(recursive = FALSE)
  datashield.rm(nodes, newx)
  list(hist = hist, quants = quants)
}
