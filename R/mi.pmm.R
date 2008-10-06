# ==============================================================================
# imputation function using predictive mean matching
# ==============================================================================
mi.pmm<-function(formula, data = NULL, start = NULL, n.iter = 100, ... )
{
  call <- match.call()
  mf   <- match.call(expand.dots = FALSE)
  m    <- match(c("formula", "data"), names(mf), 0)
  mf   <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  mf$na.action <- na.pass
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  Y  <- model.response(mf, "any")
  if (length(dim(Y)) == 1) {
    nm <- rownames(Y)
    dim(Y) <- NULL
    if (!is.null(nm)) 
      names(Y) <- nm
  }
  X <- mf[,-1,drop=FALSE]
  namesD <- if( is.null( data ) ) { NULL } else { deparse( substitute( data ) ) }
  mis    <- is.na( Y )
  n.mis  <- sum( mis )
  if(is.null(data)){ data<- mf }
  if(!is.null(start)){n.iter<-1}
  bglm.imp  <- bayesglm( formula , start = start, n.iter = n.iter )
  #sim.bglm.imp    <- sim( bglm.imp,1 )
  #yhat            <- X[mis,] %*% sim.bglm.imp$beta
  yhat <- predict( bglm.imp , newdata = data.frame( Y, X ) ) 
  result <- list( model = list( call = NULL, coefficient = NULL, sigma = NULL ), 
                    expected = NULL, random = NULL, residual = NULL )
  result$model$call <- bglm.imp$call
  result$model$coefficient <- bglm.imp$coefficients
  result$model$sigma <- sigma.hat( bglm.imp )
  result$expected <- yhat
  result$random   <- apply( as.array( yhat[mis] ), 1, 
                              mi.pmm.match, yhat=yhat[!mis], Y=Y[!mis] ) 
  result$residual <- bglm.imp$residuals
  class ( result )<- c( "mi.pmm", "mi.method","list" )
  
#  result <-new("mi.pmm",
#            model    = list( call = bglm.imp$call,
#                             call$formula = as.formula( formula ),
#                             call$start   = round(as.double( start ), 2 ),
#                             call$n.iter  = n.iter,
#                             coefficient  = bglm.imp$coefficients,
#                             sigma        = sigma.hat( bglm.imp ),
#                             dispersion   = bglm.imp$dispersion)
#            expected = yhat,
#            random   = apply( as.array( yhat[mis] ), 1, 
#                              mi.pmm.match, yhat=yhat[!mis], Y=Y[!mis] ),
#            residual = bglm.imp$residuals );
  
  return( result )

}

mi.pmm.match<-function(z, yhat=yhat, Y=Y)
{
    d <- abs( yhat - z )
    m <- Y[ d == min( d )]
    if ( length( m ) > 1 ) m <- sample( m, 1 )
    return( m )
}
