mi.pooled <- function(coef, se, m){
    W         <- colMeans(do.call(rbind, se)^2)
    Bhat      <- colMeans(do.call(rbind, coef))
    #Bhat_rep  <- t( matrix( rep( Bhat, m ), length( Bhat ), m ) )
    B <- apply(apply(do.call(rbind, coef), 1,  "-", Bhat), 1, var)
    #B         <- colSums( (do.call( rbind, coef ) - Bhat_rep)^ 2) / ( m - 1 )
    pooled <- list(coefficients = NULL, se = NULL)
    pooled$coefficients <- Bhat
    pooled$se <- sqrt(W+(1+1/m)*B)
    return(pooled)
}
