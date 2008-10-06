mi.mean <- function ( Y, check = TRUE, ...  ) {
    nameY <- deparse( substitute( Y ) )
    mis   <- is.na( Y )
    n.mis <- sum ( mis )
    if ( check ){
        # input validation
        if ( is.null( Y ) ) { stop ( message = "Inconplete variable must be specified." ) }
        if ( !is.vector ( Y ) ) { stop ( message = "Incomplete variable must be a vector." ) }
        if ( n.mis == 0 ) { warning ( message = "There is no missing value to impute." ) }
    }
    y.mean <- mean( Y, na.rm=TRUE )
    # main program
    mean.imp    <- y.mean
    determ.pred <- rep( y.mean, length( Y ) )
    names( determ.pred ) <- 1:length( determ.pred )
    random.pred <- determ.pred[is.na(Y)]
    # return the result
    result <- list( model = list( call = NULL, coefficient = NULL, sigma = NULL ), expected = NULL, random = NULL )
    result$model$call    <- paste(nameY,"=", mean.imp )
    result$expected <- determ.pred
    result$random   <- random.pred
    #result$residual <- residual.val
    class ( result ) <- c( "mi.mean", "mi.method","list" )
    return( result )
    on.exit( rm( mean.imp ) )
}
