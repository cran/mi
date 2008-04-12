mi.pooled <- function(coef, se, m){
    W         <- colSums( do.call( rbind, se )^2 ) / m
    Bhat      <- colSums( do.call( rbind, coef ) ) / m
    Bhat_rep  <- t( matrix( rep( Bhat, m ), length( Bhat ), m ) )
    B         <- colSums( do.call( rbind, coef ) - Bhat_rep ) ^ 2 / ( m - 1 )
    pooled <- list( coefficients = NULL, se = NULL )
    pooled$coefficients <- Bhat
    pooled$se <- sqrt( W + ( 1 + 1 / m ) * B )
    return(pooled)
}
