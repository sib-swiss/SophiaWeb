getvars = list(
  method = 'GET',
  FUN = function(req, res){
        v  <- paste0(config$dir, '/data/vars.json')
         ret <- readChar(v, nchars = file.size(v))
        res$set_body(ret)
  }
)

