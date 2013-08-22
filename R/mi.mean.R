# FIX THIS

mi.mean <- function ( Y, check = TRUE, missing.index = NULL, ...  ) {
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
        if ( is.null( Y ) ) { stop ( message = "Inconplete variable must be specified." ) }
        if ( !is.vector ( Y ) ) { stop ( message = "Incomplete variable must be a vector." ) }
        if ( n.mis == 0 ) { warning ( message = "There is no missing value to impute." ) }
    }
    if(!is.null(missing.index)){
      y.mean <- mean( Y[-c(missing.index)], na.rm=TRUE )    
    }
    y.mean <- mean( Y[-c(missing.index)], na.rm=TRUE )
    # main program
    mean.imp    <- y.mean
    determ.pred <- rep( y.mean, length( Y ) )
    names( determ.pred ) <- 1:length( determ.pred )
    random.pred <- determ.pred[is.na(Y)]
    # return the result
    result <- new(c("mi.mean", "mi.method"),
      model = vector("list", 0),
      expected = numeric(0),
      random = numeric(0))
    result@model$call    <- paste(nameY,"=", mean.imp )
    result@expected <- determ.pred
    result@random   <- random.pred
    return( result )
    on.exit( rm( mean.imp ) )
}
