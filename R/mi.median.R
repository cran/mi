mi.median <- function ( Y, check = TRUE, ...  ) {
    nameY <- deparse( substitute( Y ) )
    mis   <- is.na( Y )
    n.mis <- sum ( mis )
    if ( check ){
        # input validation
        if ( is.null( Y ) ) { stop ( message = "Inconplete variable must be specified." ) }
        if ( !is.vector ( Y ) ) { stop ( message = "Incomplete variable must be a vector." ) }
        if ( n.mis == 0 ) { warning ( message = "There is no missing value to impute." ) }
    }
    y.median <- median( Y, na.rm=TRUE )
    # main program
    median.imp    <- y.median
    determ.pred <- rep( y.median, length( Y ) )
    names( determ.pred ) <- 1:length( determ.pred )
    random.pred <- determ.pred[is.na(Y)]
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
