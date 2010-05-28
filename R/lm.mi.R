#==============================================================================
# Linear Regression for multiply imputed dataset
#==============================================================================
lm.mi <- function (formula, mi.object, ... ) 
{
    call   <- match.call( )
    m      <- m( mi.object )
    result <- vector( "list", m )
    names( result ) <- as.character( paste( "Chain", seq( m ), sep = ""))
    mi.data <- mi.completed(mi.object)
    for ( i in 1:m ) {
      result[[i]] <- lm( formula, data = mi.data[[i]], ... )
    }
    coef   <- vector( "list", m )
    se     <- vector( "list", m )
    for( j in 1:m ) {
      coef[[j]]<-lapply( result, summary )[[j]]$coef[,1]
      se[[j]]  <-lapply( result, summary )[[j]]$coef[,2]
    }
    pooled <- mi.pooled(coef, se)
    mi.pooled.object <- new("mi.pooled",
                          call = call, 
                          mi.pooled = pooled,
                          mi.fit = result)
    return( mi.pooled.object )
}






#glm.mi.norm <- function (formula,mi.object,  family = gaussian, ...) 
#{
#    call <- match.call()
#    m      <- m.mi(mi.object)
#    result <- vector("list",m)
#    for (i in 1:m) {
#        mi.data <- mi.matrix.mi.norm(mi.object, i)
#        result[[i]] <- glm(formula, family=family, data = data.frame(mi.data), ...)
#    }
#    pooled <- list(coefficient=NULL, se=NULL )
#    coef <-list()
#    se <-list()
#    for(j in 1:m) {
#      coef[[j]]<-lapply(result,summary)[[j]]$coef[,1]
#      se[[j]]<-lapply(result,summary)[[j]]$coef[,2]
#    }
#    W<-colSums(do.call(rbind,se)^2)/m
#    Bhat <- colSums(do.call(rbind,coef))/m
#    Bhat_rep <- t(matrix(rep(Bhat,m),length(Bhat),m))
#    B <-colSums(do.call(rbind,coef) - Bhat_rep )^2/(m-1)
#    pooled$se <- sqrt(W+(1+1/m)*B)
#    pooled$coefficient <- Bhat
#    mi.glm.object <- list(call = call, mi.pooled = pooled, mi.fit = result )
#    class( mi.glm.object ) <- c("mi.glm","list")
#    return( mi.glm.object )
#}
