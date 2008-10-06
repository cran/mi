# ==============================================================================
# imputation function for zero and positive variable
# ==============================================================================
mi.mixed <- function ( formula, data = NULL, start = NULL, n.iter = 100, 
                         draw.from.beta = FALSE, ... ) {
  call <- match.call();
  formula.dict <- if( is.list( formula ) ) { 
                    as.formula( formula[[1]] );
                  }else{
                    formula;
                  }
  formula.cont <- if( is.list( formula ) ) { 
                    as.formula( formula[[2]] );
                  } else {
                    formula;
                  }
  mf   <- match.call( expand.dots = FALSE );
  m    <- match( c( "formula", "data" ), names( mf ), 0 );
  mf   <- mf[ c( 1, m ) ];
  mf[2] <- formula[[2]]
  mf$drop.unused.levels <- TRUE;
  mf$na.action <- na.pass;
  mf[[1]] <- as.name( "model.frame" );
  mf <- eval( mf, parent.frame( ) );
  mt <- attr( mf, "terms" );
  Y  <- model.response(mf, "any");
  if ( length( dim( Y ) ) == 1 ) {
    nm <- rownames( Y );
    dim( Y ) <- NULL;
    if ( !is.null( nm ) ) 
      names( Y ) <- nm;
  }
  X <- as.matrix( mf[ , -1, drop = FALSE] );
  namesD <- if( is.null( data ) ) { NULL; } 
            else { deparse( substitute( data ) ); }
  mis    <- is.na( Y );
  n.mis  <- sum( mis );
  if(is.null(data)){ data<- mf; }
  # main program
  ## fitting the logistic model to estimate the probability of being positive
  if( !is.null( start[[1]] ) ){ 
    n.iter <- 1; 
    start[[1]][ is.na( start[[1]] )] <- 0;
  } 

  glm.sign    <- bayesglm( formula = formula.dict, data= data, 
                            family = binomial( link = "logit" ), 
                             n.iter = n.iter, 
                              start = start[[1]],drop.unused.levels=FALSE,Warning=FALSE,... );
  pred.sign   <- predict( glm.sign, newdata = data, type = "response" );
  ## fitting the model only for the positive values of y
  if( !is.null( start[[2]] ) ){ 
    n.iter <- 1;
    start[[2]][ is.na( start[[2]] ) ] <- 0;
  } 
  #control2    <- if( !is.null(start[[2]] ) ) { glm.control( maxit = 1 )} else { glm.control(...) }
  lm.ifpos    <- bayesglm( formula =  formula.cont, data = data, subset = substitute(Y) > 0, 
                            family = gaussian, n.iter = n.iter, 
                             start = start[[2]], drop.unused.levels=FALSE,Warning=FALSE, ...);
  pred.ifpos  <- predict( lm.ifpos, newdata = data, type = "response" );
  determ.pred <- abs(pred.sign * pred.ifpos);
  if(draw.from.beta){
    sim.glm.sign   <- sim( glm.sign, 1 );
    prob.pred      <- invlogit( tcrossprod(cbind(X[mis,1,drop=FALSE]*0+1,X[mis,,drop=FALSE]), sim.glm.sign$beta ));
    r.pred.pos     <- rbinom  ( n.mis, 1, prob.pred ) ;
    sim.lm.ifpos   <- sim(lm.ifpos,1);
    r.pred.val     <- rnorm(n.mis, tcrossprod(cbind(X[mis,1,drop=FALSE]*0+1,X[mis,,drop=FALSE]), sim.lm.ifpos$beta), sim.lm.ifpos$sigma );
  }
  else{
    r.pred.pos  <- rbinom( n.mis , 1, pred.sign[mis] );   # Predicted 1 if positive, 0 otherwise
    r.pred.val  <- rnorm ( n.mis , pred.ifpos[mis], sigma.hat( lm.ifpos ) );
  }
  random.pred <- abs( r.pred.pos * r.pred.val );
  names( random.pred ) <- names( determ.pred[mis] );
  result <- list( model = list(model.1=list(call=NULL,coefficient=NULL,
                                            sigma=NULL),
                               model.2=list(call=NULL,coefficient=NULL,
                                            sigma=NULL,dispersion=NULL)), 
                  expected = NULL, random = NULL );
#  result$model$model.1$call <- paste( "bayesglm(formula = 1*( ",  namesY, "!= 0 ) ~ ",
#                                                              paste( namesX, collapse = " + " ),
#                                                              ", family = binomial ( link = 'logit' ), ",
#                                                              if( !is.null( namesD ) ){ paste( "data = ", namesD, ", ", sep="" ) }, 
#                                                               if( !is.null( start[[1]] ) ) { paste( "start = ", paste( start[[1]] , collapse = "," ), ", ") },
#                                                              "n.iter = ", n.iter, ")", sep="" )
  result$model$model.1$call         <- glm.sign$call;
  result$model$model.1$call$formula <- formula.dict;
  result$model$model.1$call$start   <- round(as.double(start[[1]]),2);
  result$model$model.1$call$n.iter  <- n.iter;
  result$model$model.1$coefficient  <- glm.sign$coefficients;
  result$model$model.1$sigma        <- sigma.hat(glm.sign);
#  result$model$model.2$call <- paste( "bayesglm(formula = ", namesY, " ~ ",
#                                                              paste( namesX, collapse = " + " ),
#                                                              ", family = gaussian, ",
#                                                              if( !is.null( namesD ) ){ paste( "data = ", namesD, ", ", sep="" ) }, 
#                                                               if( !is.null( start[[2]] ) ) { paste( "start = ", paste( start[[2]] , collapse = "," ), ", ") },
#                                                              "n.iter = ", n.iter, ")", sep="" )
  result$model$model.2$call         <- lm.ifpos$call;
  result$model$model.2$call$formula <- formula.cont;
  result$model$model.2$call$start   <- round(as.double(start[[2]]),2);
  result$model$model.2$call$n.iter  <- n.iter;
  result$model$model.2$coefficient  <- lm.ifpos$coefficients;
  result$model$model.2$sigma        <- sigma.hat( lm.ifpos ); 
  result$model$model.2$dispersion   <- lm.ifpos$dispersion;               
  result$expected <- list( expected.values.1 = pred.sign, 
                           expected.values.2 = pred.ifpos ); #lm.ifpos$fitted ) #determ.pred)
  result$random   <- random.pred;
  #result$residual <- list(residual.values.1=residual.dic, residual.values.2=residual.pos )
  class ( result )<- c( "mi.mixed", "mi.method","list" );
  
#  result <-new("mi.mixed,
#            model    = list( model.1 = list( call         = glm.sign$call,
#                                             call$formula = formula.dict,
#                                             call$start   = round(as.double(start[[1]]),2),
#                                             call$n.iter  = n.iter,
#                                             coefficient  = glm.sign$coefficients,
#                                             sigma        = sigma.hat(glm.sign) ),
#                              model.2 = list( call         = lm.ifpos$call,
#                                              call$formula = formula.cont,
#                                              call$start   = round(as.double(start[[2]]),2),
#                                              call$n.iter  = n.iter,
#                                              coefficient  = lm.ifpos$coefficients,
#                                              sigma        = sigma.hat( lm.ifpos ), 
#                                              dispersion   = lm.ifpos$dispersion ))
#            expected = list( expected.values.1 = pred.sign, expected.values.2 = pred.ifpos ),
#            random   = random.pred);
  return( result );
  on.exit( rm( c( glm.sign, lm.ifpos ) ) );
}
