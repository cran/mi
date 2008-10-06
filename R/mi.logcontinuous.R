mi.logcontinuous <- function( formula, data = NULL, start = NULL, n.iter = 100, 
                                draw.from.beta = FALSE, ... ) {
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
  X <- as.matrix(mf[,-1,drop=FALSE])
  namesD <- if( is.null( data ) ) { NULL } else { deparse( substitute( data ) ) }
  mis    <- is.na( Y )
  n.mis  <- sum( mis )
  if(is.null(data)){ data<- mf }
  if( !is.null( start ) ){ 
     n.iter <- 1 
     start[is.na(start)]<-0
  } 
  bglm.imp        <- bayesglm( formula = formula, data = data, family = gaussian, n.iter = n.iter, start = start,Warning=FALSE,... )
  if(any(is.na(coefficients(bglm.imp)))){ warning(message="there are coefficient estimated as NA in the model") }
  determ.pred     <- predict( bglm.imp, newdata = data, type = "response" )
  if(draw.from.beta){
    sim.bglm.imp    <- sim(bglm.imp,1)
    random.pred     <- rnorm(n.mis, tcrossprod(cbind(X[mis,1,drop=FALSE]*0+1,X[mis,,drop=FALSE]),sim.bglm.imp$beta), sim.bglm.imp$sigma )
  }
  else{
    random.pred     <- rnorm( n.mis, determ.pred[mis], sigma.hat( bglm.imp ) );
  }
  names( random.pred ) <- names( determ.pred[mis] );
  
  # calculate residual
  # return the result
  result <- list( model = list( call = NULL, coefficient = NULL, sigma = NULL ), expected = NULL, random = NULL )
#  result$model$call <- paste( "bayesglm(formula = log(", namesY, ") ~ ",
#                                          paste( namesX, collapse = " + " ),
#                                          ", family = gaussian, ",
#                                          if( !is.null( namesD ) ){ paste( "data = ", namesD, ", ", sep="" ) }, 
#                                          if( !is.null( start ) ) { paste( "start = ", paste( start, collapse = "," ), ", ") },
#                                          "n.iter = ", n.iter, ")", sep="" )
  result$model$call        <- bglm.imp$call
  result$model$call$formula<- as.formula( formula );
  result$model$call$start  <- round(as.double( start ), 2 );
  result$model$call$n.iter <- n.iter;
  result$model$coefficient <- bglm.imp$coefficients
  result$model$sigma       <- sigma.hat( bglm.imp )
  result$model$dispersion  <- bglm.imp$dispersion
  result$expected <- exp( determ.pred )
  result$random   <- exp( random.pred )
  class ( result )<- c( "mi.logcontinuous", "mi.method", "list" )
  
#  S4 
#  result <-new("mi.sqrtcontinuous",
#            model    = list( call = bglm.imp$call,
#                             call$formula = as.formula( formula ),
#                             call$start   = round(as.double( start ), 2 ),
#                             call$n.iter  = n.iter,
#                             coefficient  = coefficients( bglm.imp ),
#                             sigma        = sigma.hat( bglm.imp ),
#                             dispersion   = bglm.imp$dispersion)
#            expected = exp( determ.pred ),
#            random   = exp( random.pred ))
  return( result )
  on.exit( rm( bglm.imp ) )
}


is.positive <- function ( data ) {
    len<-ncol(data) 
    result <- logical( len )
    names(result) <- names(data)
    for ( i in 1:len ) {
        if( is.numeric(data[!is.na(data[,i]),i])){
            result[i]<-(min(data[!is.na(data[,i]),i])>0)
        }
        else{ 
            result[i] <- NA 
        }
    }
    return(result)
}
