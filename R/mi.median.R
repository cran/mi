# FIX THIS

mi.median <- function ( Y, check = TRUE, missing.index = NULL, ...  ) {
    nameY <- deparse( substitute( Y ) )
  mis <- is.na(Y)
  n.mis <- if(is.null(missing.index)){
             sum(mis)
           } else{
             length(missing.index)
           }
  if(is.null(missing.index)& any(mis)){
    missing.index <- mis
  }
  
  if ( check ){
      # input validation
    if ( is.null( Y ) ) { 
      stop ( message = "Inconplete variable must be specified." ) 
    }
    if ( !is.vector ( Y ) ) { 
      stop ( message = "Incomplete variable must be a vector." ) 
    }
    if ( n.mis == 0 ) { 
      warning ( message = "There is no missing value to impute." ) 
    }
  }
  
  if(n.mis > 0){
    y.median <- median(Y[missing.index])
  } else {
    y.median <- median(Y)
  }
    
    # main program
    median.imp    <- y.median
    determ.pred <- rep(y.median, length(Y))
    names( determ.pred ) <- 1:length( determ.pred )
    random.pred <- determ.pred[missing.index]
    # return the result
    result <- new(c("mi.median", "mi.method"),
      model = vector("list", 0),
      expected = numeric(0),
      random = numeric(0))
    result@model$call    <- paste(nameY,"=", median.imp )
    result@expected <- determ.pred
    result@random   <- random.pred
    return( result )
    on.exit( rm( median.imp ) )
}
