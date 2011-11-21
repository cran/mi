bayesglm.mi <- function (formula, mi.object, family = gaussian, ... ) 
{
    call   <- match.call( )
    m      <- m(mi.object)
    result <- vector( "list", m )
    names( result ) <- as.character(paste( "Chain", seq( m ), sep = "" ))
    mi.data <- mi.completed(mi.object)
  
    for ( i in 1:m ) {
      result[[i]] <- bayesglm( formula, family = family, 
                          data = mi.data[[i]], ... )
    }
    coef   <- vector( "list", m )
    se     <- vector( "list", m )
    for( j in 1:m ) {
      coef[[j]]<- lapply( result, summary )[[ j ]]$coef[ ,1]
      se[[j]]  <- lapply( result, summary )[[ j ]]$coef[ ,2]
    }
    pooled <- mi.pooled(coef, se)
    mi.pooled.object <- new("mi.pooled",
                          call = call, 
                          mi.pooled = pooled,
                          mi.fit = result)
    return( mi.pooled.object )
}
