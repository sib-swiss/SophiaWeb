oneVarStats <- function(x, nodes){
  dssSubset(x, 'working_set', col.filter = paste0('"',x,'"'), datasources = nodes)
  dssSubset(x, x, row.filter = paste0('complete.cases(',x,')'), datasources = nodes)
  hist <- try(ds.histogram(paste0(x,'$',x),datasources = nodes)) %>% unlist(recursive = FALSE)
  quants <- try(ds.quantileMean(paste0(x,'$',x), datasources = nodes))%>% unlist(recursive = FALSE)
  datashield.rm(nodes, x)
  list(hist = hist, quants = quants)
}
