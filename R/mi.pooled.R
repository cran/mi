mi.pooled <- function(coef, se){
    m <- length(coef)
    ses <- do.call(rbind, se)
    coefs <- do.call(rbind, coef)
    W         <- colMeans(ses^2)
    Bhat      <- colMeans(coefs)
    #Bhat_rep  <- t( matrix( rep( Bhat, m ), length( Bhat ), m ) )
    B.diff <- apply(coefs, 1,  "-", Bhat)
    B <- apply(B.diff, 1, var)
    #B         <- colSums( (do.call( rbind, coef ) - Bhat_rep)^ 2) / ( m - 1 )
    pooled <- list(coefficients = NULL, se = NULL)
    pooled$coefficients <- Bhat
    pooled$se <- sqrt(W+(1+1/m)*B)
    return(pooled)
}
