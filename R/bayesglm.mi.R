#==============================================================================
# Generalized Linear Modeling for multiply imputed dataset
#==============================================================================
bayesglm.mi <- function (formula, mi.object, family = gaussian, ... ) 
{
    call   <- match.call( )
    m      <- m(mi.object)
    result <- vector( "list", m )
    names( result ) <- as.character(paste( "Imputation", seq( m ), sep = "" ))
    mi.data <- mi.completed(mi.object)
    mi.data <- mi.postprocess(mi.data)
  
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
    W         <- colSums( do.call( rbind, se )^2 ) / m
    Bhat      <- colSums( do.call( rbind, coef ) ) / m
    Bhat_rep  <- t( matrix( rep( Bhat, m ), length( Bhat ), m ) )
    B         <- colSums( do.call( rbind, coef ) - Bhat_rep ) ^ 2 / ( m - 1 )
    
    pooled <- list( coefficients = NULL, se = NULL )
    pooled$coefficients <- Bhat
    pooled$se <- sqrt( W + ( 1 + 1 / m ) * B )
    
    mi.bglm.object <- new("mi.glm",
                          call = call, 
                          mi.pooled = pooled,
                          mi.fit = result )
    return( mi.bglm.object )
}
