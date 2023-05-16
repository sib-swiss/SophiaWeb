stats = list(
  method = 'GET',
  FUN = function(req, res){
    
    params <- req$parameters_query
    v  <- paste0(config$dir, '/data/' ,params$cohort, '/', params$variable)
    print(v)
    out <- readChar(v, nchars = file.size(v))
    res$set_body(out)
  }
)

